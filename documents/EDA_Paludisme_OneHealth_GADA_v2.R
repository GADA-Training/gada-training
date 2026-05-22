# ============================================================================
#  GADA TRAINING & SOLUTIONS — FORMATION EN MODÉLISATION SPATIALE
#  MODULE 2 : ANALYSE EXPLORATOIRE DES DONNÉES (EDA) AVANCÉE
#  Approche : ONE HEALTH — Épidémiologie / Environnement / Spatial
#  Application : Dynamique épidémiologique du paludisme (Base OMS WMR 2024 — Annexe 4F)
#  Auteur  : GADA Training & Solutions
#  Version : 3.0 — Reconstruction complète de la structure Excel fusionnée
# ============================================================================
#
# ⚠️  POURQUOI CETTE VERSION 3.0 EST NÉCESSAIRE ⚠️
# ─────────────────────────────────────────────────────────────────────────────
#  Le fichier wmr2024_annex_4f.xlsx de l'OMS utilise une structure Excel
#  "fusionnée" (merged cells pattern) non standard :
#
#  L04 | "Algeria"  | 2000 | pop | cas_inf | cas_pt | cas_sup | dc_inf | dc_pt | dc_sup
#  L05 | ""         | 2001 | pop | ...
#  L06 | ""         | 2002 | pop | ...
#  ...
#  L28 | "Angola"   | 2000 | pop | ...
#
#  - Le nom du pays n'apparaît qu'UNE SEULE FOIS (1ère année du pays)
#  - Les lignes suivantes ont la colonne A vide → read_excel() ne peut pas
#    deviner à quel pays appartient chaque année
#  - Utiliser skip=1 ou skip=2 avec read_excel() seul produit soit 1 seule
#    année, soit des pays mal nommés → résultats corrompus
#
#  SOLUTION : Lire le fichier brut sans en-têtes, puis propager manuellement
#  le nom de pays vers le bas (fill-down) avant de filtrer les lignes de données.
# ============================================================================


# ============================================================================
# SECTION 1 — GESTION DES PACKAGES ET ENVIRONNEMENT
# ============================================================================

# Nettoyage complet de l'environnement R (bonne pratique en formation)
rm(list = ls())
options(warn = -1)

# Installation conditionnelle des packages (vérifie avant de télécharger)
packages_requis <- c("readxl", "tidyverse", "DataExplorer", "scales", "ggrepel")

for (pkg in packages_requis) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste0(">> Installation : ", pkg, "..."))
    install.packages(pkg, dependencies = TRUE)
  }
}

# Chargement des bibliothèques
library(readxl)       # Lecture native des fichiers Excel .xlsx / .xls
library(tidyverse)    # dplyr (manipulation) + ggplot2 (visualisation) + tidyr (restructuration)
library(DataExplorer) # EDA automatisée : rapports HTML, distributions, valeurs manquantes
library(scales)       # Formatage des axes ggplot2 (millions, milliers, %)
library(ggrepel)      # Étiquetage intelligent sans chevauchement (extension ggplot2)

message(">> Tous les packages chargés avec succès.")


# ============================================================================
# SECTION 2 — IMPORTATION ET RECONSTRUCTION DE LA STRUCTURE EXCEL FUSIONNÉE
# ============================================================================

# --- 2.1 Chemin d'accès au fichier source ---
# ⚠️ Adaptez ce chemin à l'emplacement réel du fichier sur votre machine.
# Conseil : utilisez "/" même sous Windows pour éviter les erreurs d'échappement.
chemin_excel <- "E:/GADA Solution/site clone/gada-training/documents/wmr2024_annex_4f.xlsx"

# --- 2.2 Lecture BRUTE sans en-têtes (col_names = FALSE) ---
# On lit TOUT le fichier sans sauter de lignes ni interpréter d'en-têtes.
# Cela nous permet de voir la structure réelle telle qu'Excel la stocke.
#
# Structure réelle confirmée par diagnostic :
#   Ligne 1 : Titre général OMS (à ignorer)
#   Ligne 2 : En-têtes partiels (Year, Population...) — non complets
#   Ligne 3 : En-têtes complets (Country/area, Lower, Point, Upper...)
#   Ligne 4+: Données → Col A = Nom pays (vide si même pays), Col B = Année
message(">> Lecture brute du fichier Excel (sans interprétation des en-têtes)...")

malaria_brut <- read_excel(
  path      = chemin_excel,
  col_names = FALSE,   # Ne pas interpréter la première ligne comme en-têtes
  col_types = "text"   # Tout lire comme texte pour éviter les conversions prématurées
)

cat(sprintf(">> Table brute : %d lignes x %d colonnes lues\n",
            nrow(malaria_brut), ncol(malaria_brut)))

# --- 2.3 Extraction des lignes de données uniquement ---
# On commence à la ligne 4 (index R = 4) car :
#   - Lignes 1-3 = en-têtes et titres → à ignorer
#   - Ligne 4 = "Algeria, 2000..." → première vraie ligne de données
#
# On renomme également les colonnes avec des noms explicites.
message(">> Extraction et renommage des colonnes de données...")

malaria_donnees <- malaria_brut %>%
  slice(4:n()) %>%   # Garder toutes les lignes à partir de la 4ème
  rename(
    nom_pays_brut       = 1,  # Col A : Nom du pays (vide si même pays que ligne précédente)
    annee_brut          = 2,  # Col B : Année (toujours remplie)
    population_brut     = 3,  # Col C : Population à risque
    cas_inf_brut        = 4,  # Col D : Cas estimés — Borne inférieure IC95%
    cas_point_brut      = 5,  # Col E : Cas estimés — Valeur centrale (indicateur principal)
    cas_sup_brut        = 6,  # Col F : Cas estimés — Borne supérieure IC95%
    deces_inf_brut      = 7,  # Col G : Décès estimés — Borne inférieure IC95%
    deces_point_brut    = 8,  # Col H : Décès estimés — Valeur centrale (mortalité)
    deces_sup_brut      = 9   # Col I : Décès estimés — Borne supérieure IC95%
  )

# --- 2.4 ÉTAPE CLÉ : Propagation du nom de pays vers le bas (Fill-Down) ---
# C'est la correction fondamentale.
# Dans le fichier OMS, "Algeria" n'apparaît qu'à la ligne de l'année 2000.
# Les lignes 2001 à 2023 ont la colonne A vide.
#
# tidyr::fill() propage la DERNIÈRE valeur non-vide vers le bas
# jusqu'à rencontrer une nouvelle valeur non-vide (= un nouveau pays).
# Résultat : chaque ligne de données se retrouve correctement associée à son pays.
message(">> Propagation du nom de pays vers le bas (fill-down)...")

malaria_donnees <- malaria_donnees %>%
  mutate(
    # Normaliser les cellules vides : "" → NA pour que fill() fonctionne correctement
    nom_pays_brut = na_if(nom_pays_brut, "")
  ) %>%
  fill(nom_pays_brut, .direction = "down")  # Propagation vers le bas uniquement

# --- 2.5 Filtrage des lignes de données valides ---
# Après le fill-down, certaines lignes ne sont pas des données pays :
#   - Titres de régions OMS en MAJUSCULES (ex: "AFRICA", "AMERICAS")
#   - Notes de bas de page (ex: "NMP: national malaria programme...")
#   - Lignes de résumés régionaux ("REGIONAL SUMMARY")
#
# Critère de filtre : une vraie ligne de données a une année numérique
# entre 2000 et 2023 dans la colonne B.
message(">> Filtrage des lignes de données valides (années 2000-2023)...")

malaria_filtree <- malaria_donnees %>%
  mutate(
    # Conversion test : l'année peut-elle être un nombre ?
    annee_num_test = suppressWarnings(as.numeric(annee_brut))
  ) %>%
  # Garder uniquement les lignes où l'année est entre 2000 et 2023
  filter(!is.na(annee_num_test), annee_num_test >= 2000, annee_num_test <= 2023) %>%
  select(-annee_num_test)  # Supprimer la colonne de test temporaire

cat(sprintf(">> Lignes de données valides : %d\n", nrow(malaria_filtree)))

# --- 2.6 Nettoyage du nom de pays et conversion des types ---
# - Supprimer les exposants de notes de bas de page (ex: "Algeria1,2,3" → "Algeria")
# - Convertir les valeurs numériques depuis le format texte
# - Remplacer "–" (tiret OMS = données non disponibles) par NA
message(">> Nettoyage des noms de pays et typage des variables numériques...")

malaria_clean <- malaria_filtree %>%
  mutate(
    # Nettoyage du nom de pays : suppression des chiffres et virgules en exposant
    # Ex : "Algeria1,2,3" → "Algeria" | "South Sudan5" → "South Sudan"
    region_pays = str_trim(str_remove_all(nom_pays_brut, "[0-9,]+")),

    # Conversion en numérique — les tirets "–" et textes divers → NA automatiquement
    annee               = as.numeric(annee_brut),
    population_a_risque = as.numeric(population_brut),
    cas_estimes_inf     = as.numeric(cas_inf_brut),
    cas_estimes_point   = as.numeric(cas_point_brut),
    cas_estimes_sup     = as.numeric(cas_sup_brut),
    deces_estimes_inf   = as.numeric(deces_inf_brut),
    deces_estimes_point = as.numeric(deces_point_brut),
    deces_estimes_sup   = as.numeric(deces_sup_brut)
  ) %>%

  # Sélection finale des colonnes propres (suppression des colonnes brutes "_brut")
  select(
    region_pays, annee, population_a_risque,
    cas_estimes_inf, cas_estimes_point, cas_estimes_sup,
    deces_estimes_inf, deces_estimes_point, deces_estimes_sup
  ) %>%

  # Tri logique : par pays puis par année
  arrange(region_pays, annee)

# --- 2.7 Rapport de validation de la base nettoyée ---
cat("\n========================================\n")
cat("VALIDATION DE LA BASE NETTOYÉE :\n")
cat(sprintf("  - Lignes totales          : %d\n", nrow(malaria_clean)))
cat(sprintf("  - Colonnes                : %d\n", ncol(malaria_clean)))
cat(sprintf("  - Plage temporelle        : %d — %d\n",
            min(malaria_clean$annee, na.rm = TRUE),
            max(malaria_clean$annee, na.rm = TRUE)))
cat(sprintf("  - Entités géographiques   : %d\n", n_distinct(malaria_clean$region_pays)))
cat(sprintf("  - Valeurs manquantes (cas): %.1f%%\n",
            mean(is.na(malaria_clean$cas_estimes_point)) * 100))
cat("========================================\n\n")

# Aperçu interactif dans RStudio
View(malaria_clean)

# Résumé statistique dans la console
print(summary(malaria_clean[, c("annee", "population_a_risque",
                                 "cas_estimes_point", "deces_estimes_point")]))


# ============================================================================
# SECTION 3 — EDA AUTOMATISÉE AVEC LE PACKAGE 'DataExplorer'
# ============================================================================

# --- 3.1 Diagnostic structurel ---
# Visualise le type de chaque variable et les dimensions globales.
# Permet de vérifier que tous les types sont corrects après nettoyage.
message(">> Diagnostic structurel (plot_str)...")
plot_str(malaria_clean)

# --- 3.2 Analyse des valeurs manquantes ---
# Visualise le taux de complétude (en %) de chaque variable.
# INTERPRÉTATION One Health : les NA sur les bornes inf/sup indiquent des pays
# pour lesquels l'OMS ne dispose que de données NMP (programme national),
# sans modélisation statistique de l'incertitude → à traiter avec précaution
# lors d'une jointure avec des couches environnementales (NDVI, précipitations).
message(">> Analyse des valeurs manquantes (plot_missing)...")
plot_missing(malaria_clean)

# --- 3.3 Distributions des variables numériques ---
# Les données de santé mondiale suivent typiquement une distribution très
# asymétrique à droite : quelques pays africains concentrent la quasi-totalité
# des cas mondiaux. Cette asymétrie doit être prise en compte dans le choix
# des classes de symbologie dans ArcGIS (privilégier Natural Breaks / Jenks).
message(">> Distributions des variables numériques (plot_histogram)...")
plot_histogram(malaria_clean)

# --- 3.4 Matrice de corrélations ---
# Corrélation attendue forte entre cas_estimes_point et deces_estimes_point
# (lien morbidité-mortalité). Une corrélation faible pour certains pays
# peut indiquer un meilleur accès aux soins ou une sous-déclaration des décès.
message(">> Matrice de corrélations (plot_correlation)...")
plot_correlation(
  data  = malaria_clean,
  type  = "continuous",
  title = "Matrice de corrélation — Variables épidémiologiques (OMS WMR 2024)"
)

# --- 3.5 Rapport HTML interactif complet ---
# Génère un fichier HTML autonome dans le répertoire de travail courant.
# Durée estimée : 30 à 90 secondes selon les ressources de la machine.
message(">> Génération du rapport HTML complet (create_report)...")
create_report(
  data         = malaria_clean,
  output_file  = "Rapport_Exploratoire_Malaria_OMS.html",
  report_title = "EDA — Paludisme Mondial 2000-2023 | GADA Training & Solutions"
)
message(paste0(">> Rapport HTML sauvegardé dans : ", getwd()))


# ============================================================================
# SECTION 4 — VISUALISATIONS AVANCÉES (ggplot2)
#             PROFILS TEMPORELS ONE HEALTH — TENDANCE & VARIABILITÉ
# ============================================================================

# ---- GRAPHIQUE 1 : Évolution temporelle avec enveloppe d'incertitude ----
# Objectif : représenter simultanément la tendance centrale (médiane mondiale)
# et la variabilité inter-pays (interquartile + amplitude maximale).
# L'approche "ribbon + line" est la norme en épidémiologie globale publiée.
message(">> Graphique 1 : Évolution temporelle avec enveloppe d'incertitude...")

malaria_summary_temps <- malaria_clean %>%
  filter(!is.na(annee), !is.na(cas_estimes_point)) %>%
  group_by(annee) %>%
  summarise(
    cas_mediane = median(cas_estimes_point, na.rm = TRUE),
    cas_maximum = max(cas_estimes_point, na.rm = TRUE),
    cas_q25     = quantile(cas_estimes_point, 0.25, na.rm = TRUE),
    cas_q75     = quantile(cas_estimes_point, 0.75, na.rm = TRUE),
    .groups     = "drop"
  )

ggplot(data = malaria_summary_temps, aes(x = annee)) +
  # Enveloppe extérieure : amplitude totale (0 → pic mondial)
  geom_ribbon(aes(ymin = 0, ymax = cas_maximum), fill = "steelblue", alpha = 0.08) +
  # Enveloppe intérieure : zone interquartile (50% des pays)
  geom_ribbon(aes(ymin = cas_q25, ymax = cas_q75), fill = "steelblue", alpha = 0.25) +
  # Ligne de tendance : médiane mondiale (robuste aux valeurs extrêmes)
  geom_line(aes(y = cas_mediane), color = "firebrick", linewidth = 1.3) +
  geom_point(aes(y = cas_mediane), color = "firebrick", size = 2.5,
             shape = 21, fill = "white", stroke = 1.5) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 9, color = "grey50", hjust = 0),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title    = "Évolution de la Charge Mondiale du Paludisme (2000–2023)",
    subtitle = "Ligne rouge : Médiane mondiale | Bleu foncé : Zone interquartile | Bleu clair : Amplitude maximale",
    x        = "Année d'observation",
    y        = "Cas de paludisme estimés (millions)",
    caption  = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ---- GRAPHIQUE 2 : Relation Morbidité ↔ Mortalité — Nuage de bulles 2023 ----
# Objectif : explorer la corrélation cas-décès à l'échelle pays en 2023.
# La taille des bulles représente la population à risque.
# La couleur encode le taux de létalité apparent (CFR = décès/cas × 100).
# Un CFR élevé signale un accès aux soins insuffisant → priorité d'intervention.
message(">> Graphique 2 : Morbidité vs Mortalité 2023 (Bubble Chart)...")

malaria_2023 <- malaria_clean %>%
  filter(
    annee == 2023,
    !is.na(cas_estimes_point),
    !is.na(deces_estimes_point),
    !is.na(population_a_risque),
    # Exclusion des résumés régionaux OMS : ils agrègent plusieurs pays
    # et ne correspondent pas à une entité spatiale cartographiable unique
    !str_detect(str_to_upper(region_pays),
                "^(AFRICA|AMERICAS|EASTERN|EUROPEAN|SOUTH-EAST|WESTERN|REGIONAL|GLOBAL)")
  ) %>%
  mutate(
    # Taux de létalité apparent (CFR) : décès pour 100 cas estimés
    taux_letalite_pct = round((deces_estimes_point / cas_estimes_point) * 100, 3),
    # Incidence standardisée pour 1 000 habitants à risque
    incidence_pour_1000 = round((cas_estimes_point / population_a_risque) * 1000, 3)
  )

cat(sprintf(">> Pays avec données complètes en 2023 : %d\n", nrow(malaria_2023)))

# Sélection des 5 pays avec la charge en cas la plus élevée pour étiquetage
top5_cas <- malaria_2023 %>% slice_max(order_by = cas_estimes_point, n = 5)

ggplot(malaria_2023, aes(x = cas_estimes_point, y = deces_estimes_point,
                          size = population_a_risque)) +
  geom_point(aes(color = taux_letalite_pct), alpha = 0.65, shape = 16) +
  geom_label_repel(
    data        = top5_cas,
    aes(label   = region_pays),
    size        = 3.2, fontface = "bold",
    box.padding = 0.5, max.overlaps = 10, color = "grey20"
  ) +
  scale_color_gradient(low = "#27ae60", high = "#c0392b",
                       name = "Taux de\nlétalité (%)") +
  scale_size_continuous(name   = "Population\nà risque",
                        labels = label_number(scale = 1e-6, suffix = " M"),
                        range  = c(2, 16)) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " k")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title   = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Morbidité vs. Mortalité Paludéenne par Pays — 2023",
    subtitle = "Taille des bulles : Population à risque | Couleur : CFR apparent (Décès/Cas × 100)",
    x        = "Cas estimés (millions)",
    y        = "Décès estimés (milliers)",
    caption  = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ---- GRAPHIQUE 3 : Top 15 pays — Lollipop Chart ----
# Objectif : visualiser la concentration de la charge mondiale paludéenne.
# Ce graphique illustre le principe One Health : les pays à forte charge sont
# souvent ceux avec la plus grande surface de zones humides / rizicoles / forêts.
message(">> Graphique 3 : Classement Top 15 pays (Lollipop Chart)...")

top15_pays <- malaria_2023 %>%
  slice_max(order_by = cas_estimes_point, n = 15) %>%
  mutate(region_pays = reorder(region_pays, cas_estimes_point))

ggplot(top15_pays, aes(x = region_pays, y = cas_estimes_point)) +
  geom_segment(aes(xend = region_pays, yend = 0),
               color = "steelblue", linewidth = 0.9) +
  geom_point(color = "firebrick", size = 4.5) +
  geom_text(
    aes(label = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)(cas_estimes_point)),
    hjust = -0.15, size = 3.2, color = "grey25"
  ) +
  coord_flip() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M"),
                     expand = expansion(mult = c(0, 0.20))) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(size = 10, color = "grey40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(face = "bold", size = 11)
  ) +
  labs(
    title    = "Top 15 des Pays les Plus Touchés par le Paludisme — 2023",
    subtitle = "Classement par estimation centrale des cas (valeur OMS point estimate)",
    x        = NULL,
    y        = "Cas de paludisme estimés (millions)",
    caption  = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ============================================================================
# SECTION 5 — PRÉPARATION ET EXPORTATION POUR ArcGIS Pro / ArcMap
# ============================================================================

# --- 5.1 Construction de la table pour jointure attributaire ---
# Critères de sélection :
#   ✔ Année 2023 (couche temporelle la plus récente)
#   ✔ Une seule ligne par pays (pas de doublons pour la jointure)
#   ✔ Exclusion des totaux régionaux OMS (non cartographiables)
#   ✔ Variables dérivées calculées pour la symbologie thématique
message(">> Préparation de la table pour ArcGIS Pro...")

malaria_arcgis <- malaria_clean %>%

  # Filtrage 2023 uniquement
  filter(annee == 2023) %>%

  # Exclusion des résumés régionaux OMS (agrégats multi-pays)
  filter(
    !str_detect(str_to_upper(region_pays),
                "^(AFRICA|AMERICAS|EASTERN|EUROPEAN|SOUTH-EAST|WESTERN|REGIONAL|GLOBAL)")
  ) %>%

  # Calcul des indicateurs dérivés pour la cartographie thématique
  mutate(
    # CFR : Taux de létalité apparent (qualité des soins)
    taux_letalite_pct   = ifelse(
      !is.na(cas_estimes_point) & cas_estimes_point > 0,
      round((deces_estimes_point / cas_estimes_point) * 100, 4),
      NA_real_
    ),
    # Incidence standardisée pour 1 000 hab. à risque (comparaison inter-pays)
    incidence_pour_1000 = ifelse(
      !is.na(population_a_risque) & population_a_risque > 0,
      round((cas_estimes_point / population_a_risque) * 1000, 4),
      NA_real_
    ),
    # Mortalité pour 100 000 hab. à risque (indicateur ODD 3.3)
    mortalite_pour_100k = ifelse(
      !is.na(population_a_risque) & population_a_risque > 0,
      round((deces_estimes_point / population_a_risque) * 100000, 4),
      NA_real_
    )
  ) %>%

  # Sélection et ordre final des colonnes pour ArcGIS
  select(
    region_pays,           # ← CLE DE JOINTURE avec le Shapefile (harmoniser les noms !)
    annee,
    population_a_risque,
    cas_estimes_inf,       # Borne inférieure IC95% — pour cartographie de l'incertitude
    cas_estimes_point,     # ← VARIABLE PRINCIPALE pour la symbologie (Choroplèthe)
    cas_estimes_sup,       # Borne supérieure IC95%
    deces_estimes_inf,
    deces_estimes_point,   # Mortalité — variable secondaire de symbologie
    deces_estimes_sup,
    taux_letalite_pct,     # CFR (%) — indicateur de qualité des soins
    incidence_pour_1000,   # Incidence standardisée — comparaison normalisée
    mortalite_pour_100k    # Mortalité standardisée — indicateur ODD 3.3
  ) %>%

  arrange(region_pays)

# --- 5.2 Rapport de validation pré-export ---
cat(sprintf(">> Table ArcGIS prête : %d pays x %d variables\n",
            nrow(malaria_arcgis), ncol(malaria_arcgis)))

# Vérification des pays avec toutes les variables complètes
pays_complets <- malaria_arcgis %>%
  filter(!is.na(cas_estimes_point) & !is.na(deces_estimes_point))
cat(sprintf(">> Pays avec données complètes : %d / %d\n",
            nrow(pays_complets), nrow(malaria_arcgis)))

# --- 5.3 Exportation CSV (UTF-8 sans BOM, compatible ArcGIS) ---
# write_csv() du tidyverse = UTF-8 propre, sans numéros de ligne.
# Éviter write.csv() de base R qui ajoute une colonne de numéros non souhaitée.
chemin_csv <- "malaria_arcgis_2023.csv"
write_csv(malaria_arcgis, chemin_csv)
message(paste0(">> CSV exporté : ", chemin_csv))
message(paste0("   Répertoire  : ", getwd()))


# ============================================================================
# SECTION 6 — GUIDE DE JOINTURE DANS ArcGIS PRO (Mémo Formateur)
# ============================================================================

# ┌─────────────────────────────────────────────────────────────────────────┐
# │   ÉTAPES DANS ArcGIS Pro — À PROJETER POUR LES APPRENANTS              │
# ├─────────────────────────────────────────────────────────────────────────┤
# │                                                                         │
# │  ÉTAPE A — Charger la couche géographique de référence :               │
# │    Catalogue → Connexion dossier → Glisser "World_Countries.shp"       │
# │    (ou équivalent depuis votre géodatabase de formation)                │
# │                                                                         │
# │  ÉTAPE B — Ajouter la table CSV exportée :                             │
# │    Map → Add Data → Table → Sélectionner "malaria_arcgis_2023.csv"     │
# │    → La table apparaît dans Contents sans représentation spatiale.      │
# │                                                                         │
# │  ÉTAPE C — Jointure attributaire (Add Join) :                          │
# │    Clic droit sur World_Countries → Joins and Relates → Add Join        │
# │      • Input Join Field (Shapefile) : COUNTRY_NAME (ou ISO_A3)         │
# │      • Join Table                  : malaria_arcgis_2023               │
# │      • Output Join Field (CSV)     : region_pays                       │
# │    → Valider. La table des attributs est maintenant enrichie.           │
# │                                                                         │
# │  ÉTAPE D — Symbologie choroplèthe :                                    │
# │    Clic droit → Symbology → Graduated Colors                            │
# │      • Field  : cas_estimes_point  (ou incidence_pour_1000 si normalisé)│
# │      • Method : Natural Breaks (Jenks) — adapté aux distributions       │
# │                 asymétriques typiques des données épidémiologiques      │
# │      • Classes : 5                                                      │
# │      • Palette : Jaune → Orange → Rouge (convention santé publique)    │
# │                                                                         │
# │  ⚠️  NOTE — Harmonisation des noms de pays :                           │
# │    "region_pays" peut différer du champ pays du Shapefile.             │
# │    Exemples de divergences courantes :                                  │
# │      OMS : "Congo"          → Shapefile : "Republic of the Congo"      │
# │      OMS : "Côte d'Ivoire"  → Shapefile : "Ivory Coast"               │
# │    Solution : créer une table de correspondance ISO 3166-1 Alpha-3     │
# │    et ajouter une colonne "iso3" au CSV avant la jointure.             │
# └─────────────────────────────────────────────────────────────────────────┘


# ============================================================================
# FIN DU SCRIPT
# ============================================================================

message("")
message("╔══════════════════════════════════════════════════════════════╗")
message("║   GADA TRAINING & SOLUTIONS — Script v3.0 exécuté           ║")
message("║   Base reconstituée | EDA complète | CSV ArcGIS généré      ║")
message("╚══════════════════════════════════════════════════════════════╝")
