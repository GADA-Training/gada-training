// =================================================================
// MODULE REÇUS FORMATION ASAFR 2026-2027  (v2 : versements + export Excel)
// =================================================================
const REC_DB = 'formation_receipts';
const REC_BANNER = new URL('images/Bandeau_de_logos_corrige_FormaSIG.png', location.href).href;
const REC_F = {
    titre: 'Recherche Scientifique, Analyse de Données et Intelligence Artificielle',
    sous: "Appliquées à l'Action Humanitaire et au Développement — Du Terrain à la Décision",
    periode: 'Du 2 novembre au 29 décembre 2026',
    horaires: 'Lundi · Mardi · Vendredi, 08h00 – 12h00',
    format: 'Hybride · 26 séances · 104 heures',
    lieu: "ASAFR, Q. Kyeshero, Av. de l'Unité n°1, Goma",
    cohortes: {
        '1': 'Cohorte 1 : Recherche Scientifique & Ingénierie des Données',
        '2': 'Cohorte 2 : Analyse de Données, SIG & IA',
        '1+2': 'Cohortes 1 & 2 (parcours complet)'
    },
    tarifs: { 'Étudiant / Chercheur': 20, 'Professionnel': 30 }
};
let recList = [], recCurrent = null, recListener = null;

// ---------- Accès / navigation ----------
function openRecOnlyView() {
    document.getElementById('app').style.display = 'none';
    openRecusPanel(true);
}
function openRecusPanel(only) {
    if (!isRecAdmin() && !isRecOnlyUser()) { toast('Accès non autorisé'); return; }
    document.getElementById('app').style.display = 'none';
    const adm = document.getElementById('adminTfcPanel'); if (adm) adm.style.display = 'none';
    document.getElementById('recusPanel').style.display = 'block';
    document.getElementById('recBtnBack').style.display = only ? 'none' : 'inline-flex';
    document.getElementById('recUserName').textContent = currentUser.email + (isRecAdmin() ? ' (Admin)' : '');
    recInitForm(); recStart(); recShowTab('list');
}
function closeRecusPanel() {
    document.getElementById('recusPanel').style.display = 'none';
    document.getElementById('app').style.display = 'block';
}
function recLogout() {
    if (recListener) { db.ref(REC_DB).off('value', recListener); recListener = null; }
    document.getElementById('recusPanel').style.display = 'none';
    firebase.auth().signOut();
}
function recShowTab(tab) {
    ['list', 'new', 'preview'].forEach(t => {
        document.getElementById('rec-section-' + t).style.display = t === tab ? 'block' : 'none';
        document.getElementById('rec-tab-' + t).classList.toggle('active', t === tab);
    });
}
function recStart() {
    if (recListener) db.ref(REC_DB).off('value', recListener);
    recListener = snap => {
        recList = [];
        snap.forEach(c => recList.push(c.val()));
        recList.sort((a, b) => (b.createdAt || 0) - (a.createdAt || 0));
        recRenderList();
    };
    db.ref(REC_DB).on('value', recListener);
}

// ---------- Utilitaires ----------
function recFmtDate(d) {
    if (!d) return '—';
    const x = new Date(d + 'T12:00:00');
    return isNaN(x) ? d : x.toLocaleDateString('fr-FR', { day: 'numeric', month: 'long', year: 'numeric' });
}
function recVersements(r) {
    const v = r.versements;
    if (v) {
        const a = Array.isArray(v) ? v : Object.values(v);
        if (a.length) return a;
    }
    // Anciens reçus (sans historique) : un seul versement
    return [{ montant: +r.paye || 0, date: r.date || '', mode: r.mode || '', ref: r.ref || '', ts: r.createdAt || 0 }];
}

// ---------- Formulaire nouveau reçu ----------
function recInitForm() {
    ['rec-f-nom','rec-f-tel','rec-f-email','rec-f-inst','rec-f-paye','rec-f-ref','rec-f-notes'].forEach(id => { const e = document.getElementById(id); if (e) e.value = ''; });
    document.getElementById('rec-f-cat').value = 'Étudiant / Chercheur';
    document.getElementById('rec-f-cohorte').value = '1';
    document.getElementById('rec-f-mode').value = 'Espèces';
    document.getElementById('rec-f-date').value = new Date().toISOString().split('T')[0];
    recCategoryChange();
}
function recCategoryChange() {
    document.getElementById('rec-f-du').value = REC_F.tarifs[document.getElementById('rec-f-cat').value] || 0;
    recCalc();
}
function recCalc() {
    const du = parseFloat(document.getElementById('rec-f-du').value) || 0;
    const paye = parseFloat(document.getElementById('rec-f-paye').value) || 0;
    const reste = Math.max(0, du - paye);
    document.getElementById('rec-f-reste').value = reste;
    document.getElementById('rec-f-statut').value = (du > 0 && reste <= 0) ? 'Soldé' : 'Partiel';
}
async function recSave() {
    if (!isRecAdmin() && !isRecOnlyUser()) return;
    const g = id => document.getElementById(id).value.trim();
    const nom = g('rec-f-nom');
    const du = parseFloat(g('rec-f-du')) || 0;
    const paye = parseFloat(g('rec-f-paye')) || 0;
    if (!nom) { toast('Le nom est obligatoire'); return; }
    if (paye <= 0) { toast('Entrez le montant payé'); return; }
    if (paye > du) { toast('Le montant payé dépasse le montant dû'); return; }
    try {
        const cnt = await db.ref('formation_receipts_counter').transaction(n => (n || 0) + 1);
        const seq = cnt.snapshot.val();
        const ref = db.ref(REC_DB).push();
        const reste = Math.max(0, du - paye);
        const data = {
            id: ref.key,
            num: 'REC-ASAFR-' + String(seq).padStart(4, '0'),
            nom, tel: g('rec-f-tel'), email: g('rec-f-email'), inst: g('rec-f-inst'),
            cohorte: g('rec-f-cohorte'), categorie: g('rec-f-cat'),
            du, paye, reste, statut: reste <= 0 ? 'Soldé' : 'Partiel',
            mode: g('rec-f-mode'), ref: g('rec-f-ref'), date: g('rec-f-date'), notes: g('rec-f-notes'),
            versements: [{ montant: paye, date: g('rec-f-date'), mode: g('rec-f-mode'), ref: g('rec-f-ref'), ts: Date.now(), par: currentUser.email }],
            emisPar: currentUser.email, createdAt: Date.now()
        };
        await ref.set(data);
        toast('✅ Reçu ' + data.num + ' généré');
        recView(data.id, data);
    } catch (e) { console.error(e); toast('Erreur : ' + e.message); }
}

// ---------- Liste ----------
function recRenderList() {
    const q = (document.getElementById('rec-search')?.value || '').toLowerCase();
    const f = document.getElementById('rec-filter')?.value || '';
    document.getElementById('rec-st-count').textContent = recList.length;
    document.getElementById('rec-st-paid').textContent = '$' + recList.reduce((a, r) => a + (+r.paye || 0), 0);
    document.getElementById('rec-st-rest').textContent = '$' + recList.reduce((a, r) => a + (+r.reste || 0), 0);
    const rows = recList.filter(r => ((r.nom || '').toLowerCase().includes(q) || (r.num || '').toLowerCase().includes(q)) && (!f || r.statut === f));
    const tb = document.getElementById('rec-tbody');
    if (!rows.length) { tb.innerHTML = '<tr><td colspan="8" style="text-align:center;padding:40px;color:var(--muted)">Aucun reçu.</td></tr>'; return; }
    tb.innerHTML = rows.map(r => {
        const sc = r.statut === 'Soldé' ? 'inv-sold' : 'inv-partial';
        const nbV = recVersements(r).length;
        return `<tr><td style="font-weight:700;color:#0a5a42">${esc(r.num)}</td><td><strong>${esc(r.nom)}</strong>${r.tel ? `<br><small style="color:var(--muted)">${esc(r.tel)}</small>` : ''}</td><td style="font-size:12px">${esc(REC_F.cohortes[r.cohorte] || r.cohorte)}</td><td style="font-size:12px">${esc(r.categorie)}</td><td style="font-weight:700;color:#0a5a42">$${r.paye}${nbV > 1 ? `<br><small style="color:var(--muted);font-weight:400">${nbV} versements</small>` : ''}</td><td style="font-weight:700;color:#c0392b">$${r.reste}</td><td><span class="inv-status ${sc}">${esc(r.statut)}</span></td><td><div class="inv-actions-row"><button class="inv-btn inv-btn-navy inv-btn-sm" style="background:#0a5a42" onclick="recView('${r.id}')" title="Voir / imprimer"><i class="fa-solid fa-eye"></i></button>${r.reste > 0 ? `<button class="inv-btn inv-btn-gold inv-btn-sm" onclick="recOpenComplete('${r.id}')" title="Compléter le paiement"><i class="fa-solid fa-coins"></i></button>` : ''}${isRecAdmin() ? `<button class="inv-btn inv-btn-red inv-btn-sm" onclick="recDelete('${r.id}')" title="Supprimer"><i class="fa-solid fa-trash"></i></button>` : ''}</div></td></tr>`;
    }).join('');
}
function recView(id, obj) {
    const r = obj || recList.find(x => x.id === id);
    if (!r) return;
    recCurrent = r;
    document.getElementById('rec-printable').innerHTML = recReceiptHTML(r);
    const bc = document.getElementById('rec-btn-complete');
    if (bc) bc.style.display = r.reste > 0 ? 'inline-flex' : 'none';
    recShowTab('preview');
}
async function recDelete(id) {
    if (!isRecAdmin()) { toast('Réservé à l\'administrateur'); return; }
    const r = recList.find(x => x.id === id);
    if (!r || !confirm(`Supprimer le reçu ${r.num} de "${r.nom}" ?`)) return;
    await db.ref(REC_DB + '/' + id).remove();
    toast('Reçu supprimé');
}

// ---------- Compléter le paiement (versement supplémentaire) ----------
function recOpenComplete(id) {
    const r = id ? recList.find(x => x.id === id) : recCurrent;
    if (!r) { toast('Sélectionnez un reçu'); return; }
    if (r.reste <= 0) { toast('Ce reçu est déjà soldé'); return; }
    const old = document.getElementById('recCompleteModal'); if (old) old.remove();
    const m = document.createElement('div');
    m.id = 'recCompleteModal';
    m.className = 'modal-overlay active';
    m.innerHTML = `<div class="modal-box" style="max-width:480px;">
        <div class="modal-top"><h3><i class="fa-solid fa-coins"></i> Compléter le paiement</h3><button class="modal-close" onclick="document.getElementById('recCompleteModal').remove()">×</button></div>
        <div style="background:#f4f8f6;border-left:4px solid #f9d21a;padding:12px 14px;margin-bottom:16px;font-size:0.82rem;line-height:1.7;color:#0d2b4e;">
            <strong>${esc(r.nom)}</strong> · ${esc(r.num)}<br>
            Montant dû : <strong>$${r.du}</strong> · Déjà payé : <strong style="color:#166534">$${r.paye}</strong><br>
            Reste à payer : <strong style="color:#c0392b">$${r.reste}</strong>
        </div>
        <div class="form-field"><label>Montant du nouveau versement ($) *</label><input type="number" id="recc-montant" min="1" max="${r.reste}" value="${r.reste}"></div>
        <div class="form-field"><label>Mode de paiement</label><select id="recc-mode" style="width:100%;padding:12px 14px;border:1.5px solid var(--border);border-left:3px solid #0d2b4e;font-family:inherit;font-size:0.9rem;"><option>Espèces</option><option>Mobile Money</option><option>Virement bancaire</option><option>Autre</option></select></div>
        <div class="form-field"><label>Référence transaction</label><input type="text" id="recc-ref" placeholder="Optionnel"></div>
        <div class="form-field"><label>Date du versement</label><input type="date" id="recc-date" value="${new Date().toISOString().split('T')[0]}"></div>
        <div style="display:flex;gap:10px;margin-top:16px;">
            <button class="btn-submit-post" style="flex:2;background:#0a5a42;border-left-color:#f9d21a;" onclick="recConfirmComplete('${r.id}')"><i class="fa-solid fa-check"></i> Enregistrer le versement</button>
            <button style="flex:1;padding:12px;background:var(--bg);border:1.5px solid var(--border);border-radius:8px;font-weight:700;cursor:pointer;font-family:inherit;color:#0d2b4e" onclick="document.getElementById('recCompleteModal').remove()">Annuler</button>
        </div>
    </div>`;
    document.body.appendChild(m);
    m.addEventListener('click', e => { if (e.target === m) m.remove(); });
}
async function recConfirmComplete(id) {
    if (!isRecAdmin() && !isRecOnlyUser()) return;
    const montant = parseFloat(document.getElementById('recc-montant').value) || 0;
    const mode = document.getElementById('recc-mode').value;
    const ref = document.getElementById('recc-ref').value.trim();
    const date = document.getElementById('recc-date').value;
    if (montant <= 0) { toast('Entrez un montant valide'); return; }
    let err = null;
    try {
        const res = await db.ref(REC_DB + '/' + id).transaction(cur => {
            if (!cur) return cur;
            const reste = +cur.reste || 0;
            if (montant > reste) { err = 'Le montant dépasse le reste à payer ($' + reste + ')'; return; } // annule
            const vs = recVersements(cur).slice();
            vs.push({ montant, date, mode, ref, ts: Date.now(), par: currentUser.email });
            cur.versements = vs;
            cur.paye = (+cur.paye || 0) + montant;
            cur.reste = Math.max(0, (+cur.du || 0) - cur.paye);
            cur.statut = cur.reste <= 0 ? 'Soldé' : 'Partiel';
            cur.updatedAt = Date.now();
            return cur;
        });
        if (err) { toast(err); return; }
        if (!res.committed) { toast('Opération annulée'); return; }
        document.getElementById('recCompleteModal')?.remove();
        const r = res.snapshot.val();
        toast(r.statut === 'Soldé' ? '✅ Paiement soldé !' : '✅ Versement enregistré');
        recView(id, r);
    } catch (e) { console.error(e); toast('Erreur : ' + e.message); }
}

// ---------- Export Excel ----------
function recLoadXLSX() {
    return new Promise((resolve, reject) => {
        if (window.XLSX) return resolve();
        const s = document.createElement('script');
        s.src = 'https://cdnjs.cloudflare.com/ajax/libs/xlsx/0.18.5/xlsx.full.min.js';
        s.onload = resolve;
        s.onerror = () => reject(new Error('Bibliothèque Excel indisponible (vérifiez la connexion)'));
        document.head.appendChild(s);
    });
}
async function recExportExcel() {
    if (!isRecAdmin() && !isRecOnlyUser()) return;
    if (!recList.length) { toast('Aucun reçu à exporter'); return; }
    try { await recLoadXLSX(); } catch (e) { toast(e.message); return; }
    const sorted = recList.slice().sort((a, b) => (a.createdAt || 0) - (b.createdAt || 0));

    const rows = sorted.map(r => ({
        'N° Reçu': r.num, 'Date': r.date || '', 'Nom complet': r.nom, 'Téléphone': r.tel || '',
        'Email': r.email || '', 'Institution': r.inst || '',
        'Cohorte': REC_F.cohortes[r.cohorte] || r.cohorte, 'Catégorie': r.categorie,
        'Montant dû ($)': +r.du || 0, 'Payé ($)': +r.paye || 0, 'Reste ($)': +r.reste || 0,
        'Statut': r.statut, 'Nb versements': recVersements(r).length, 'Remarque': r.notes || ''
    }));
    const sum = k => sorted.reduce((a, r) => a + (+r[k] || 0), 0);
    const total = {};
    Object.keys(rows[0]).forEach(k => total[k] = '');
    total['Nom complet'] = 'TOTAL';
    total['Montant dû ($)'] = sum('du'); total['Payé ($)'] = sum('paye'); total['Reste ($)'] = sum('reste');
    rows.push(total);

    const vrows = [];
    sorted.forEach(r => recVersements(r).forEach((v, i) => vrows.push({
        'N° Reçu': r.num, 'Nom complet': r.nom, 'Versement n°': i + 1,
        'Date': v.date || '', 'Montant ($)': +v.montant || 0,
        'Mode': v.mode || '', 'Référence': v.ref || '', 'Enregistré par': v.par || r.emisPar || ''
    })));

    const wb = XLSX.utils.book_new();
    const ws1 = XLSX.utils.json_to_sheet(rows);
    ws1['!cols'] = [{wch:16},{wch:12},{wch:28},{wch:16},{wch:26},{wch:24},{wch:46},{wch:20},{wch:14},{wch:10},{wch:10},{wch:10},{wch:14},{wch:30}];
    XLSX.utils.book_append_sheet(wb, ws1, 'Reçus');
    const ws2 = XLSX.utils.json_to_sheet(vrows);
    ws2['!cols'] = [{wch:16},{wch:28},{wch:13},{wch:12},{wch:12},{wch:18},{wch:20},{wch:28}];
    XLSX.utils.book_append_sheet(wb, ws2, 'Versements');
    XLSX.writeFile(wb, 'Recus_Formation_ASAFR_' + new Date().toISOString().split('T')[0] + '.xlsx');
    toast('✅ Export Excel téléchargé');
}

// ---------- Reçu (HTML imprimable) ----------
function recReceiptHTML(r) {
    const ds = recFmtDate(r.date);
    const solde = r.reste <= 0;
    const pill = solde ? 'background:#dcfce7;color:#166534' : 'background:#fef3c7;color:#92400e';
    const vs = recVersements(r);
    const vRows = vs.map((v, i) => `<tr><td>${i + 1}</td><td>${esc(recFmtDate(v.date))}</td><td>${esc(v.mode || '—')}</td><td>${esc(v.ref || '—')}</td><td style="font-weight:700;color:#166534">$${v.montant}</td></tr>`).join('');
    return `<div class="rcp-doc" id="rcp-inner">
        <img class="rcp-banner" src="${REC_BANNER}" alt="Partenaires" onerror="this.style.display='none'">
        <div class="rcp-band">
            <div><h2>REÇU DE PAIEMENT</h2><div style="font-size:12px;color:rgba(255,255,255,.8);margin-top:4px">Formation intensive · Cohortes 2026-2027</div></div>
            <div><div class="rcp-num">N° ${esc(r.num)}</div><p>${esc(ds)}</p></div>
        </div>
        <div class="rcp-body">
            <div class="rcp-formation"><small>Formation</small><h3>${esc(REC_F.titre)}</h3><p>${esc(REC_F.sous)}</p></div>
            <div class="rcp-client">
                <div><div class="rcp-client-name">${esc((r.nom || '').toUpperCase())}</div>
                    ${r.tel ? `<p><strong>Téléphone :</strong> ${esc(r.tel)}</p>` : ''}
                    ${r.email ? `<p><strong>Email :</strong> ${esc(r.email)}</p>` : ''}
                    ${r.inst ? `<p><strong>Institution :</strong> ${esc(r.inst)}</p>` : ''}
                </div>
                <div style="text-align:right"><p style="font-size:12.5px"><strong>Reçu n°</strong> ${esc(r.num)}</p><p style="font-size:12.5px"><strong>Date :</strong> ${esc(ds)}</p><div class="rcp-pill" style="${pill}">${esc(r.statut)}</div></div>
            </div>
            <table class="rcp-table"><thead><tr><th style="width:34%">DÉSIGNATION</th><th>MONTANT DÛ</th><th>PAYÉ</th><th>RESTE</th></tr></thead><tbody>
                <tr><td><strong>Frais de formation</strong><br><small style="color:#666">${esc(REC_F.cohortes[r.cohorte] || r.cohorte)}<br>Catégorie : ${esc(r.categorie)}</small></td><td>$${r.du}</td><td style="font-weight:700;color:#166534">$${r.paye}</td><td style="font-weight:700;color:#c0392b">$${r.reste}</td></tr>
                <tr class="rcp-total"><td>TOTAL</td><td>$${r.du}</td><td>$${r.paye}</td><td>$${r.reste}</td></tr>
            </tbody></table>
            <div style="font-size:11.5px;font-weight:800;color:#0a5a42;margin:0 0 6px">HISTORIQUE DES VERSEMENTS</div>
            <table class="rcp-table"><thead><tr><th style="width:8%">N°</th><th>DATE</th><th>MODE</th><th>RÉFÉRENCE</th><th>MONTANT</th></tr></thead><tbody>
                ${vRows}
                <tr class="rcp-total"><td colspan="4" style="text-align:right">TOTAL VERSÉ</td><td>$${r.paye}</td></tr>
            </tbody></table>
            <div class="rcp-infos">
                <div><b>Période :</b> ${esc(REC_F.periode)}</div>
                <div><b>Horaires :</b> ${esc(REC_F.horaires)}</div>
                <div><b>Format :</b> ${esc(REC_F.format)}</div>
                <div><b>Lieu :</b> ${esc(REC_F.lieu)}</div>
                <div style="grid-column:1/-1"><b>Certification :</b> Brevet / Certificat de Formation cosigné</div>
                ${r.notes ? `<div style="grid-column:1/-1"><b>Remarque :</b> ${esc(r.notes)}</div>` : ''}
            </div>
            <div class="rcp-sign"><div>Le / La participant(e)<span>Signature</span></div><div>Le Responsable de la formation<span>Signature &amp; cachet</span></div></div>
            <div class="rcp-note">Ce reçu fait foi de paiement. Merci de le conserver.</div>
        </div>
        <div class="rcp-footer">
            <div><p>Contacts de la formation</p><p>cehmdgadaasafr@gmail.com</p><p>+243 995 093 024 (WhatsApp)</p></div>
            <div style="text-align:right"><p>www.gadatraining.org</p><p>geomindsafrica.com</p><p>www.hopelandcongo.org</p></div>
        </div>
        <div class="rcp-bar"></div>
    </div>`;
}
function recPrint() {
    const inner = document.getElementById('rcp-inner');
    if (!inner) { toast('Sélectionnez d\'abord un reçu'); return; }
    const styles = Array.from(document.querySelectorAll('style')).map(s => s.outerHTML).join('');
    const w = window.open('', '_blank', 'width=900,height=800');
    w.document.write(`<!DOCTYPE html><html lang="fr"><head><meta charset="UTF-8"><title>Reçu ${recCurrent ? recCurrent.num + ' — ' + recCurrent.nom : ''}</title><link href="https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;600;700;800&display=swap" rel="stylesheet">${styles}<style>body{background:#fff!important;min-height:0}@page{margin:0;size:A4}</style></head><body>${inner.outerHTML}</body></html>`);
    w.document.close();
    setTimeout(() => { w.focus(); w.print(); }, 900);
}
function recWhatsApp() {
    if (!recCurrent) { toast('Sélectionnez d\'abord un reçu'); return; }
    const r = recCurrent;
    const ds = r.date ? new Date(r.date + 'T12:00:00').toLocaleDateString('fr-FR') : '';
    const nbV = recVersements(r).length;
    const msg = `Bonjour ${r.nom},\n\nVoici votre reçu de paiement — Formation intensive Recherche Scientifique, Analyse de Données et IA (2026-2027).\n\n🧾 *Reçu n° ${r.num}*\n📅 Date : ${ds}\n━━━━━━━━━━━━━━\n💰 Montant dû : *$${r.du}*\n✅ Total payé : *$${r.paye}*${nbV > 1 ? ` (${nbV} versements)` : ''}\n⏳ Reste : *$${r.reste}*\n📊 Statut : *${r.statut}*\n━━━━━━━━━━━━━━\n📍 ${REC_F.lieu}\n🗓 ${REC_F.periode}\n\nMerci pour votre confiance !\n🎓 *GADA Training · CEH · ASAFR*\n📞 +243 995 093 024`;
    window.open('https://wa.me/' + (r.tel || '').replace(/\D/g, '') + '?text=' + encodeURIComponent(msg), '_blank');
}
// ===== FIN MODULE REÇUS FORMATION =====
