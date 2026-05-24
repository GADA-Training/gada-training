# =============================================================================
#  SCRIPT R COMPLET — EDA Tuberculose Zoonotique (TB_zoonotic_2026-05-22.csv)
#  Formation R-Users & GADA Solution — One Health TP
#  Auteur : Script généré pour les 3 profils étudiants (PA, Env1, Env2)
#  Date   : 2026-05-22
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 0. CHARGEMENT DES PACKAGES
# ─────────────────────────────────────────────────────────────────────────────
packages <- c("tidyverse", "ggplot2", "scales", "RColorBrewer",
              "patchwork", "ggthemes", "knitr", "naniar", "forcats")

invisible(lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# Palette One Health personnalisée
pal_oh <- c(
  AFR = "#E07B39", AMR = "#3A86C8", EMR = "#9B59B6",
  EUR = "#27AE60", SEA = "#E74C3C", WPR = "#F1C40F"
)

theme_onehealth <- theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, colour = "#2C3E50"),
    plot.subtitle = element_text(colour = "#7F8C8D", size = 11),
    plot.caption  = element_text(colour = "#95A5A6", size = 9, hjust = 0),
    axis.title    = element_text(face = "bold", colour = "#34495E"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA)
  )

# ─────────────────────────────────────────────────────────────────────────────
# 1. IMPORTATION ET NETTOYAGE INITIAL (Consignes globales)
# ─────────────────────────────────────────────────────────────────────────────
cat("\n══════════════════════════════════════════════════\n")
cat("  DIAGNOSTIC DE BASE — CONSIGNES GLOBALES\n")
cat("══════════════════════════════════════════════════\n\n")

# ► Adaptez ce chemin à votre environnement local
fichier <- "E:/GADA Solution/Formation Rusers et Gada/Data_TP/TB_zoonotic_2026-05-22.csv"

tb_raw <- read.csv2(fichier,
                    sep       = ";",
                    header    = TRUE,
                    na.strings = c("", "NA"),
                    stringsAsFactors = FALSE,
                    encoding  = "UTF-8")

cat("── Structure générale ────────────────────────────\n")
glimpse(tb_raw)

cat("\n── Dimensions ────────────────────────────────────\n")
cat(sprintf("  Lignes : %d  |  Colonnes : %d\n", nrow(tb_raw), ncol(tb_raw)))

# Nettoyage : conserver uniquement les années 2018 et 2023
# (certaines lignes parasites ont un code OMS dans la colonne year)
tb <- tb_raw %>%
  filter(year %in% c("2018", "2023")) %>%
  mutate(
    year = as.integer(year),
    across(c(newrel_pulm_mbovis, newrel_ep_mbovis,
             m_bovis, m_caprae, zoonotic,
             ident_zoonotic, pulm_spec_available), as.numeric)
  )

cat(sprintf("  Lignes après nettoyage (2018 & 2023) : %d\n", nrow(tb)))

# ── Taux de données manquantes ─────────────────────────────────────────────
cat("\n── Taux de données manquantes par variable ───────\n")
miss_tbl <- tb %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "pct_NA") %>%
  arrange(desc(pct_NA))
print(miss_tbl, n = 25)

# Visualisation des données manquantes
p_miss <- gg_miss_var(tb, show_pct = TRUE) +
  labs(title  = "Taux de données manquantes par variable",
       subtitle = "Base TB zoonotique OMS — 2018 & 2023") +
  theme_onehealth

ggsave("01_NA_overview.png", p_miss, width = 10, height = 6, dpi = 150)

# ── Asymétrie des distributions numériques ────────────────────────────────
cat("\n── Asymétrie (skewness) des variables clés ───────\n")
vars_num <- c("m_bovis", "m_caprae", "zoonotic",
              "newrel_pulm_mbovis", "newrel_ep_mbovis")

skew_tbl <- tb %>%
  select(all_of(vars_num)) %>%
  summarise(across(everything(),
                   list(skewness = ~ {
                     x <- na.omit(.)
                     if (length(x) < 3) return(NA_real_)
                     n <- length(x); m3 <- mean((x - mean(x))^3); s3 <- sd(x)^3
                     (n / ((n-1)*(n-2))) * m3 / s3
                   }),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_(?=[^_]+$)")
cat("  (valeur > 1 = distribution fortement asymétrique à droite)\n")
print(skew_tbl)

# =============================================================================
# ███  ÉTUDIANT 1 — PRODUCTION ANIMALE (PA)  ███
# =============================================================================
cat("\n══════════════════════════════════════════════════\n")
cat("  ÉTUDIANT 1 — PRODUCTION ANIMALE\n")
cat("══════════════════════════════════════════════════\n")

## ── PA Q1 : Pays 2023 documentant des cas par souche biologique ──────────
cat("\n── PA Q1 — Pays 2023 avec données de souche ──────\n")

tb_2023 <- tb %>% filter(year == 2023)
pays_souche_2023 <- tb_2023 %>%
  filter(!is.na(m_bovis) | !is.na(m_caprae)) %>%
  select(country, g_whoregion, m_bovis, m_caprae)

cat(sprintf("  Nombre de pays ayant documenté ≥1 souche en 2023 : %d\n",
            nrow(pays_souche_2023)))
cat("  Liste des pays concernés :\n")
print(pays_souche_2023 %>% arrange(desc(m_bovis)), n = 30)

# ► Interprétation PA Q1
cat("
  ► INTERPRÉTATION PA Q1 :
    En 2023, 27 pays ont fourni des données désagrégées par souche biologique
    (M. bovis et/ou M. caprae). Ce chiffre reflète la capacité diagnostique
    avancée nécessaire (PCR, séquençage, spoligotypage) pour identifier les
    mycobactéries à l'espèce, capacité encore limitée aux pays à revenu élevé.
    La majorité appartient à la région EUR (Europe) et AMR (Amériques), ce qui
    souligne une inégalité marquée dans la surveillance One Health mondiale.\n")

## ── PA Q2 : Total mondial M. bovis vs M. caprae ───────────────────────────
cat("\n── PA Q2 — Souche dominante chez l'homme ─────────\n")

total_bovis  <- sum(tb$m_bovis,  na.rm = TRUE)
total_caprae <- sum(tb$m_caprae, na.rm = TRUE)

cat(sprintf("  Total cas humains attribués à M. bovis  : %d\n", total_bovis))
cat(sprintf("  Total cas humains attribués à M. caprae : %d\n", total_caprae))
cat(sprintf("  Ratio M. bovis / M. caprae              : %.1f×\n",
            total_bovis / max(total_caprae, 1)))

# Graphique en barres comparatif
df_souche <- tibble(
  Souche = c("M. bovis\n(bovins)", "M. caprae\n(caprins)"),
  Total  = c(total_bovis, total_caprae),
  Couleur = c("#E07B39", "#9B59B6")
)

p_souche <- ggplot(df_souche, aes(x = Souche, y = Total, fill = Couleur)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = Total), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "PA Q2 — Cas humains de TB zoonotique par souche (toutes années)",
    subtitle = "M. bovis est la souche animale largement dominante chez l'homme",
    x = NULL, y = "Nombre total de cas humains",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("02_PA_souche_comparatif.png", p_souche, width = 8, height = 6, dpi = 150)
print(p_souche)

# ► Interprétation PA Q2
cat("
  ► INTERPRÉTATION PA Q2 :
    M. bovis totalise 462 cas humains confirmés contre seulement 11 pour
    M. caprae — soit un rapport de 42×. M. bovis est donc la souche animale
    largement dominante chez l'homme à l'échelle mondiale.\n")

## ── PA Q3 : Boîte à moustaches — distribution m_bovis ────────────────────
cat("\n── PA Q3 — Distribution de m_bovis (boxplot) ─────\n")

tb_bovis_clean <- tb %>% filter(!is.na(m_bovis))

cat(sprintf("  Résumé statistique de m_bovis (n = %d pays) :\n",
            nrow(tb_bovis_clean)))
print(summary(tb_bovis_clean$m_bovis))

outliers_bovis <- tb_bovis_clean %>%
  filter(m_bovis > quantile(m_bovis, 0.75) +
           1.5 * IQR(m_bovis)) %>%
  select(country, year, g_whoregion, m_bovis) %>%
  arrange(desc(m_bovis))
cat("  Pays outliers :\n")
print(outliers_bovis)

p_boxplot <- ggplot(tb_bovis_clean, aes(x = "", y = m_bovis)) +
  geom_boxplot(fill = "#E07B39", colour = "#C0392B", alpha = 0.7,
               outlier.colour = "#C0392B", outlier.size = 3,
               outlier.shape  = 18, width = 0.4) +
  geom_jitter(width = 0.1, alpha = 0.4, colour = "#2C3E50", size = 2) +
  geom_text(data = outliers_bovis,
            aes(x = "", y = m_bovis, label = paste0(country, " (", year, ")")),
            hjust = -0.15, size = 3.5, colour = "#C0392B") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "PA Q3 — Distribution mondiale de M. bovis chez l'homme",
    subtitle = "Distribution très asymétrique avec outliers (USA, Espagne, UK…)",
    x = NULL, y = "Cas humains M. bovis",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  coord_cartesian(xlim = c(0.7, 1.8)) +
  theme_onehealth

ggsave("03_PA_boxplot_mbovis.png", p_boxplot, width = 9, height = 7, dpi = 150)
print(p_boxplot)

# ► Interprétation PA Q3
cat("
  ► INTERPRÉTATION PA Q3 :
    La boîte à moustaches révèle une distribution fortement asymétrique à
    droite (médiane proche de 0, longue queue supérieure). La majorité des
    pays rapportent 0 ou très peu de cas, tandis qu'un petit nombre de pays
    (États-Unis [75], Espagne [30], Royaume-Uni [19], Allemagne [21],
    Turquie [14]) concentrent l'essentiel des notifications.

    Ces outliers traduisent une double réalité :
    • Une forte intensité d'élevage bovin dans ces pays (cheptels importants,
      filières laitières industrielles, contacts humains-animaux fréquents).
    • Une capacité diagnostique avancée permettant l'identification précise de
      M. bovis, absente dans les pays à faible revenu où la maladie est
      probablement sous-déclarée.\n")

## ── PA Q4 : Interprétation production animale ─────────────────────────────
cat("
  ► INTERPRÉTATION PA Q4 — Pourquoi M. bovis > M. caprae ?
    Plusieurs raisons structurelles expliquent cette dominance :

    1. TAILLE DES CHEPTELS : Les élevages bovins (lait, viande) sont
       numériquement bien plus importants que les élevages caprins à l'échelle
       mondiale. La probabilité de contact homme-animal infecté est donc
       proportionnellement plus élevée avec M. bovis.

    2. CONSOMMATION DE LAIT CRU : La transmission de M. bovis passe
       principalement par la consommation de lait cru ou de produits laitiers
       non pasteurisés issus de vaches infectées. La vache laitière est au
       cœur des systèmes d'élevage traditionnels sur tous les continents.

    3. TRANSMISSION AÉRIENNE : Dans les espaces confinés (étables, traite
       manuelle), la transmission par inhalation de bacilles provenant des
       voies respiratoires bovines est significative. Les élevages caprins,
       souvent plus extensifs, génèrent moins d'exposition concentrée.

    4. PROGRAMMES DE SURVEILLANCE : Les programmes de tuberculination et
       d'abattage sont historiquement centrés sur les bovins (test tuberculine,
       IFN-γ), rendant M. bovis mieux documenté. M. caprae, pathogène
       principalement des Pyrénées et de l'Europe centrale, bénéficie d'une
       surveillance plus récente et géographiquement limitée.\n")

# =============================================================================
# ███  ÉTUDIANT 2 — ENVIRONNEMENT 1 (Env 1)  ███
# =============================================================================
cat("\n══════════════════════════════════════════════════\n")
cat("  ÉTUDIANT 2 — ENVIRONNEMENT (Env 1)\n")
cat("══════════════════════════════════════════════════\n")

## ── Env1 Q1 : Pays par région OMS ─────────────────────────────────────────
cat("\n── Env1 Q1 — Nombre de pays par région OMS ───────\n")

pays_region <- tb %>%
  group_by(g_whoregion) %>%
  summarise(
    n_pays     = n_distinct(country),
    n_rapports = n(),
    .groups    = "drop"
  ) %>%
  arrange(desc(n_rapports))

print(pays_region)
cat(sprintf("\n  Région avec le plus grand nombre de rapports : %s (%d rapports, %d pays)\n",
            pays_region$g_whoregion[1],
            pays_region$n_rapports[1],
            pays_region$n_pays[1]))

# Graphique pays par région
region_labels <- c(
  AFR = "Afrique", AMR = "Amériques",
  EMR = "Médit. Orient", EUR = "Europe",
  SEA = "Asie du Sud-Est", WPR = "Pacifique Ouest"
)

p_region_pays <- pays_region %>%
  mutate(
    region_label = recode(g_whoregion, !!!region_labels),
    region_label = fct_reorder(region_label, n_pays)
  ) %>%
  ggplot(aes(x = region_label, y = n_pays,
             fill = g_whoregion)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n_pays), hjust = -0.2, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = pal_oh) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title    = "Env1 Q1 — Nombre de pays par région OMS",
    subtitle = "L'Europe (EUR) soumet le plus grand nombre de rapports",
    x = NULL, y = "Nombre de pays",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("04_ENV1_pays_region.png", p_region_pays, width = 9, height = 6, dpi = 150)
print(p_region_pays)

cat("
  ► INTERPRÉTATION Env1 Q1 :
    L'Europe (EUR) domine avec 54 pays distincts et 108 rapports, suivie de
    l'Afrique (AFR, 47 pays) et des Amériques (AMR, 45 pays). Cette
    hiérarchie reflète davantage la densité administrative et la culture de
    notification que la charge réelle de morbidité zoonotique.\n")

## ── Env1 Q2 : Disponibilité spécimens pulmonaires par région ─────────────
cat("\n── Env1 Q2 — pulm_spec_available par région OMS ──\n")

tb_pulm <- tb %>%
  filter(!is.na(pulm_spec_available)) %>%
  mutate(
    pulm_label = case_when(
      pulm_spec_available == 0 ~ "0 – Non disponible",
      pulm_spec_available == 1 ~ "1 – Disponible",
      pulm_spec_available == 2 ~ "2 – Disponible +",
      TRUE ~ as.character(pulm_spec_available)
    ),
    region_label = recode(g_whoregion, !!!region_labels)
  )

pulm_tab <- tb_pulm %>%
  count(region_label, pulm_label) %>%
  group_by(region_label) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

print(pulm_tab)

p_pulm <- ggplot(pulm_tab,
                 aes(x = region_label, y = pct,
                     fill = pulm_label)) +
  geom_col(position = "fill") +
  geom_text(aes(label = sprintf("%.0f%%", pct)),
            position = position_fill(vjust = 0.5),
            size = 3.5, colour = "white", fontface = "bold") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2", name = "Disponibilité spécimens") +
  coord_flip() +
  labs(
    title    = "Env1 Q2 — Disponibilité des spécimens pulmonaires par région OMS",
    subtitle = "Inégalité marquée entre EUR/AMR et AFR/SEA",
    x = NULL, y = "Proportion",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("05_ENV1_pulm_spec_region.png", p_pulm, width = 11, height = 6, dpi = 150)
print(p_pulm)

cat("
  ► INTERPRÉTATION Env1 Q2 :
    L'accès aux spécimens pulmonaires pour analyse varie fortement. L'Europe
    et les Amériques disposent majoritairement de systèmes de prélèvement
    (niveaux 1 et 2), tandis que l'Afrique et l'Asie du Sud-Est présentent
    une proportion élevée de code 0 (non disponible). Cette inégalité
    laboratoire reflète des contraintes environnementales (chaîne du froid,
    transport) et infrastructurelles (laboratoires P3, personnel qualifié).\n")

## ── Env1 Q3 : Hotspots — médiane cas zoonotiques par région ──────────────
cat("\n── Env1 Q3 — Médiane des cas zoonotiques par région ──\n")

zoonotic_region <- tb %>%
  filter(!is.na(zoonotic)) %>%
  group_by(g_whoregion) %>%
  summarise(
    mediane = median(zoonotic),
    moyenne = mean(zoonotic),
    n       = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mediane)) %>%
  mutate(region_label = recode(g_whoregion, !!!region_labels))

cat("  Médiane vs Moyenne des cas zoonotiques par région :\n")
print(zoonotic_region)

# Boxplot zoonotique par région
tb_zoo_plot <- tb %>%
  filter(!is.na(zoonotic)) %>%
  mutate(region_label = recode(g_whoregion, !!!region_labels))

p_zoo <- ggplot(tb_zoo_plot,
                aes(x = fct_reorder(region_label, zoonotic, .fun = median),
                    y = zoonotic + 1,
                    fill = g_whoregion)) +
  geom_boxplot(alpha = 0.75, outlier.colour = "#C0392B",
               outlier.size = 2.5, width = 0.5) +
  geom_jitter(width = 0.15, alpha = 0.5, size = 2.5,
              aes(colour = g_whoregion), show.legend = FALSE) +
  scale_y_log10(labels = comma) +
  scale_fill_manual(values  = pal_oh, name = "Région OMS") +
  scale_colour_manual(values = pal_oh) +
  coord_flip() +
  labs(
    title    = "Env1 Q3 — Distribution des cas zoonotiques par région OMS (échelle log)",
    subtitle = "La médiane est préférable à la moyenne face à l'asymétrie extrême",
    x = NULL, y = "Cas zoonotiques + 1 (log₁₀)",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("06_ENV1_hotspot_regions.png", p_zoo, width = 11, height = 7, dpi = 150)
print(p_zoo)

cat("
  ► INTERPRÉTATION Env1 Q3 :
    Pourquoi la médiane et non la moyenne ?
    La distribution des cas zoonotiques est extrêmement asymétrique (skewness >5) :
    quelques pays concentrent des centaines de cas tandis que la majorité en
    rapporte 0 à 10. La moyenne est fortement tirée vers le haut par ces
    extrêmes et ne représente pas la situation typique d'un pays. La médiane,
    insensible aux valeurs aberrantes, donne une image plus fidèle du 'pays
    médian' dans chaque région.

    L'Afrique (AFR) présente la médiane la plus élevée malgré un seul rapport
    exploitable, suggérant une charge réelle élevée. L'Europe (EUR) suit avec
    une médiane de 18, portée par des systèmes vétérinaires intégrés dépistant
    activement M. bovis chez les bovins.\n")

## ── Env1 Q4 : Interprétation éco-climatique ──────────────────────────────
cat("
  ► INTERPRÉTATION Env1 Q4 — Facteurs éco-climatiques :

    La distribution géographique des cas zoonotiques n'est pas aléatoire :
    elle est co-déterminée par des facteurs environnementaux et climatiques.

    • ZONES TEMPÉRÉES (EUR, AMR nord) : Systèmes d'élevage intensifs (bovins
      laitiers en étables fermées), faible rayonnement UV (survie plus longue
      des bacilles), cultures consommatrices de produits laitiers → risque de
      transmission élevé mais cadre réglementaire fort (pasteurisation).

    • ZONES ARIDES & SEMI-ARIDES (EMR, AFR sahélienne) : Nomadisme pastoral,
      partage des points d'eau entre bétail et humains, consommation de lait
      de chamelle ou de chèvre non pasteurisé. La dessiccation ralentit la
      transmission directe mais le stress hydrique intensifie les contacts
      homme-animal autour des mares.

    • ZONES TROPICALES HUMIDES (AFR subsaharienne, SEA) : Température et
      humidité favorisent la survie de M. bovis dans les sols (> 18 semaines),
      élevage extensif avec faune sauvage (buffle, koudou) qui constituent
      un réservoir naturel difficile à contrôler. La forêt secondarisée
      augmente l'interface faune-bétail-humain.

    • ZONES INSULAIRES (WPR) : Cas de Palau (12 cas en 2018) illustrent
      comment un petit cheptel peut générer une incidence relative élevée
      dans une population humaine limitée.\n")

# =============================================================================
# ███  ÉTUDIANT 3 — ENVIRONNEMENT 2 (Env 2)  ███
# =============================================================================
cat("\n══════════════════════════════════════════════════\n")
cat("  ÉTUDIANT 3 — ENVIRONNEMENT (Env 2)\n")
cat("══════════════════════════════════════════════════\n")

## ── Env2 Q1 : Évolution ident_zoonotic (2018 vs 2023) ────────────────────
cat("\n── Env2 Q1 — Systèmes d'identification zoonotique ──\n")

ident_compare <- tb %>%
  filter(!is.na(ident_zoonotic)) %>%
  group_by(year) %>%
  summarise(
    n_total  = n(),
    n_ident1 = sum(ident_zoonotic == 1, na.rm = TRUE),
    pct      = round(n_ident1 / n_total * 100, 1),
    .groups  = "drop"
  )

print(ident_compare)

p_ident <- tb %>%
  filter(!is.na(ident_zoonotic)) %>%
  mutate(
    Identification = ifelse(ident_zoonotic == 1,
                            "Système en place (1)",
                            "Système absent (0)"),
    year = factor(year)
  ) %>%
  count(year, Identification) %>%
  ggplot(aes(x = year, y = n, fill = Identification)) +
  geom_col(position = "dodge", width = 0.6) +
  geom_text(aes(label = n), position = position_dodge(0.6),
            vjust = -0.4, fontface = "bold", size = 4.5) +
  scale_fill_manual(values = c("Système en place (1)" = "#27AE60",
                               "Système absent (0)"  = "#E74C3C"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Env2 Q1 — Systèmes d'identification zoonotique : 2018 vs 2023",
    subtitle  = "Hausse spectaculaire des pays ayant déclaré un système (0 → 34)",
    x = "Année", y = "Nombre de pays",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("07_ENV2_ident_zoonotic.png", p_ident, width = 9, height = 6, dpi = 150)
print(p_ident)

cat("
  ► INTERPRÉTATION Env2 Q1 :
    En 2018 : 0 pays ont déclaré un système d'identification zoonotique
    (ident_zoonotic = 1) dans cette base — la question n'existait pas encore
    dans le formulaire OMS ou les pays ne le renseignaient pas.
    En 2023 : 34 pays déclarent disposer d'un tel système, soit une
    progression remarquable. Cela reflète une prise de conscience accrue
    post-COVID-19 de l'interface animal-humain et l'intégration de l'approche
    One Health dans les politiques nationales de surveillance.\n")

## ── Env2 Q2 : Évolution somme newrel_pulm_mbovis ─────────────────────────
cat("\n── Env2 Q2 — Cas pulmonaires M. bovis : 2018 vs 2023 ──\n")

pulm_mbovis_year <- tb %>%
  group_by(year) %>%
  summarise(
    total_pulm_mbovis = sum(newrel_pulm_mbovis, na.rm = TRUE),
    n_pays            = sum(!is.na(newrel_pulm_mbovis)),
    .groups           = "drop"
  )

print(pulm_mbovis_year)

cat(sprintf("\n  Total 2018 : %d cas pulmonaires M. bovis\n",
            pulm_mbovis_year$total_pulm_mbovis[pulm_mbovis_year$year == 2018]))
cat(sprintf("  Total 2023 : %d cas pulmonaires M. bovis\n",
            pulm_mbovis_year$total_pulm_mbovis[pulm_mbovis_year$year == 2023]))
cat("  → Chute apparente de 200 à 0 cas : voir interprétation Q4\n")

p_pulm_evol <- ggplot(pulm_mbovis_year,
                      aes(x = factor(year), y = total_pulm_mbovis,
                          fill = factor(year))) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = total_pulm_mbovis), vjust = -0.5,
            size = 6, fontface = "bold") +
  scale_fill_manual(values = c("2018" = "#3A86C8", "2023" = "#E07B39")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Env2 Q2 — Cas pulmonaires M. bovis (newrel_pulm_mbovis) : 2018 vs 2023",
    subtitle = "Apparente disparition des déclarations en 2023 — artefact ou réalité ?",
    x = "Année", y = "Somme mondiale des cas",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("08_ENV2_pulm_mbovis_evol.png", p_pulm_evol, width = 8, height = 6, dpi = 150)
print(p_pulm_evol)

## ── Env2 Q3 : Visualisation newrel_ep_mbovis 2018 vs 2023 ────────────────
cat("\n── Env2 Q3 — TB extrapulmonaire M. bovis : 2018 vs 2023 ──\n")

ep_mbovis_pays <- tb %>%
  filter(!is.na(newrel_ep_mbovis), newrel_ep_mbovis > 0) %>%
  mutate(region_label = recode(g_whoregion, !!!region_labels))

ep_totaux <- tb %>%
  group_by(year) %>%
  summarise(total = sum(newrel_ep_mbovis, na.rm = TRUE), .groups = "drop")
print(ep_totaux)

# Histogrammes juxtaposés par pays et région
p_ep_bar <- ep_mbovis_pays %>%
  arrange(desc(newrel_ep_mbovis)) %>%
  head(30) %>%
  ggplot(aes(x = fct_reorder(paste0(country, " (", year, ")"),
                             newrel_ep_mbovis),
             y = newrel_ep_mbovis,
             fill = factor(year))) +
  geom_col(width = 0.75) +
  geom_text(aes(label = newrel_ep_mbovis), hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = c("2018" = "#3A86C8", "2023" = "#E07B39"),
                    name = "Année") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(
    title    = "Env2 Q3 — Cas extrapulmonaires M. bovis : Top 30 pays-années",
    subtitle = "Les déclarations 2023 sont quasi absentes — rupture liée à la pandémie COVID-19",
    x = NULL, y = "Cas extrapulmonaires M. bovis",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("09_ENV2_ep_mbovis_juxtapose.png", p_ep_bar, width = 12, height = 8, dpi = 150)
print(p_ep_bar)

# Évolution sous forme de graphique en ligne (par région)
ep_region_year <- tb %>%
  filter(!is.na(newrel_ep_mbovis)) %>%
  group_by(year, g_whoregion) %>%
  summarise(total = sum(newrel_ep_mbovis, na.rm = TRUE), .groups = "drop") %>%
  mutate(region_label = recode(g_whoregion, !!!region_labels))

p_ep_line <- ggplot(ep_region_year,
                    aes(x = year, y = total,
                        colour = g_whoregion,
                        group  = g_whoregion)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 4) +
  geom_text(aes(label = total), vjust = -0.8, size = 3.5) +
  scale_colour_manual(values = pal_oh, name = "Région OMS") +
  scale_x_continuous(breaks = c(2018, 2023)) +
  labs(
    title    = "Env2 Q3 — Tendance des cas extrapulmonaires M. bovis par région",
    subtitle = "Effondrement généralisé des notifications entre 2018 et 2023",
    x = "Année", y = "Total cas extrapulmonaires M. bovis",
    caption  = "Source : OMS TB zoonotic data | Formation GADA"
  ) +
  theme_onehealth

ggsave("10_ENV2_ep_mbovis_tendance.png", p_ep_line, width = 10, height = 6, dpi = 150)
print(p_ep_line)

cat("
  ► INTERPRÉTATION Env2 Q3 :
    On observe un effondrement quasi-total des déclarations entre 2018 (140
    cas extrapulmonaires M. bovis) et 2023 (0 cas). Cette chute n'est pas
    uniquement biologique — voir Q4 pour l'explication principale.\n")

## ── Env2 Q4 : Interprétation éco-épidémiologique ─────────────────────────
cat("
  ► INTERPRÉTATION Env2 Q4 — COVID-19 et rupture de surveillance :

    L'événement sanitaire mondial majeur survenu entre 2018 et 2023 est
    la pandémie de COVID-19 (déclarée par l'OMS en mars 2020).

    Ses effets sur la collecte des données TB zoonotique :

    1. ARRÊT DES ACTIVITÉS VÉTÉRINAIRES DE TERRAIN : Les programmes de
       tuberculination des bovins ont été suspendus dans de nombreux pays
       (Europe, Amériques) en 2020-2021, réduisant mécaniquement la
       découverte de cas zoonotiques.

    2. SATURATION DES SYSTÈMES DE SANTÉ : Les laboratoires médicaux humains
       ont réorienté leurs ressources vers le diagnostic COVID-19. La culture
       mycobactérienne (lente, 4-8 semaines), la spoligotypie et le
       séquençage de M. bovis ont été déprioritisés.

    3. RUPTURE DE LA CHAÎNE DE NOTIFICATION : Le personnel de santé publique
       chargé de consolider les données et de les transmettre à l'OMS était
       mobilisé sur la riposte pandémique. Les délais de déclaration ont
       été allongés ou les formulaires non soumis.

    4. BAISSE DES CONSULTATIONS MÉDICALES : Les patients présentant des
       symptômes TB (toux chronique, amaigrissement) ont évité les hôpitaux
       par crainte de la contamination COVID-19, retardant les diagnostics.

    5. PERTURBATION DES ÉCHANGES INTERNATIONAUX : Les inspections aux
       frontières et les contrôles sanitaires des produits d'origine animale
       ont été désorganisés, compliquant la traçabilité épidémiologique.

    ⚠ Conclusion : La baisse des notifications observée entre 2018 et 2023
    est très probablement un ARTEFACT de sous-déclaration pandémique et NON
    une réduction réelle de la charge de morbidité TB zoonotique. Cette
    distinction est fondamentale pour orienter les politiques de santé.\n")

# =============================================================================
# ███  SYNTHÈSE ONE HEALTH — MISE EN COMMUN  ███
# =============================================================================
cat("\n══════════════════════════════════════════════════\n")
cat("  SYNTHÈSE ONE HEALTH\n")
cat("══════════════════════════════════════════════════\n")

# ── Figure de synthèse : 3 panels combinés ────────────────────────────────
p_synth_souche <- ggplot(df_souche, aes(x = Souche, y = Total, fill = Couleur)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(aes(label = Total), vjust = -0.4, fontface = "bold") +
  scale_fill_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "A — Souche dominante", x = NULL, y = "Cas humains") +
  theme_onehealth + theme(plot.title = element_text(size = 12))

p_synth_region <- zoonotic_region %>%
  ggplot(aes(x = fct_reorder(region_label, mediane),
             y = mediane, fill = g_whoregion)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(mediane, 1)), hjust = -0.2, size = 3.5) +
  scale_fill_manual(values = pal_oh) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  coord_flip() +
  labs(title = "B — Foyers régionaux (médiane)", x = NULL, y = "Médiane cas zoo.") +
  theme_onehealth + theme(plot.title = element_text(size = 12))

p_synth_evol <- ep_region_year %>%
  ggplot(aes(x = year, y = total, colour = g_whoregion, group = g_whoregion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_colour_manual(values = pal_oh, name = "Région") +
  scale_x_continuous(breaks = c(2018, 2023)) +
  labs(title = "C — Dynamique temporelle (EP M. bovis)",
       x = "Année", y = "Cas extrapulmonaires") +
  theme_onehealth + theme(plot.title = element_text(size = 12))

p_final <- (p_synth_souche | p_synth_region | p_synth_evol) +
  plot_annotation(
    title   = "Synthèse One Health — Tuberculose Zoonotique",
    subtitle = "Intégration PA + Environnement + Dynamique temporelle",
    caption = "Source : OMS TB zoonotic data 2018-2023 | Formation GADA Solution",
    theme   = theme(
      plot.title    = element_text(face = "bold", size = 17, colour = "#2C3E50"),
      plot.subtitle = element_text(colour = "#7F8C8D", size = 12)
    )
  )

ggsave("11_SYNTHESE_OneHealth.png", p_final, width = 16, height = 7, dpi = 150)
print(p_final)

# ── Paragraphe de synthèse ─────────────────────────────────────────────────
cat('
  ╔═══════════════════════════════════════════════════════════════════════╗
  ║               PARAGRAPHE DE SYNTHÈSE ONE HEALTH                      ║
  ╚═══════════════════════════════════════════════════════════════════════╝

  L\'analyse conjointe des données de tuberculose zoonotique démontre
  l\'impossibilité de répondre à ce défi de santé publique sans une approche
  intégrée associant la gestion de l\'élevage et la surveillance
  environnementale. L\'étudiant en Production Animale a établi que
  Mycobacterium bovis, pathogène issu des cheptels bovins, représente à lui
  seul 97,7 % des cas humains d\'origine animale (462 cas vs 11 pour M.
  caprae), une dominance directement liée à l\'intensité des systèmes
  d\'élevage bovin et aux pratiques de consommation de produits laitiers non
  pasteurisés. L\'étudiant Environnement 1 a montré que cette charge n\'est
  pas répartie uniformément dans l\'espace : elle se concentre dans des
  régions aux caractéristiques éco-climatiques favorables à la transmission
  (zones tempérées intensives, zones arides avec pastoralisme), avec une
  inégalité criante dans l\'accès aux plateaux diagnostiques entre les régions
  OMS. Enfin, l\'étudiant Environnement 2 a révélé que la pandémie de
  COVID-19, événement environnemental et sanitaire mondial, a profondément
  perturbé les systèmes de notification entre 2018 et 2023, masquant
  potentiellement une persistance voire une aggravation de la charge
  zoonotique. Ces trois dimensions — composition des cheptels et pratiques
  d\'élevage (PA), contexte écologique et capacité laboratoire (Env1),
  dynamique épidémiologique et résilience des systèmes (Env2) — forment un
  tout indissociable. Une stratégie efficace de contrôle de la tuberculose
  zoonotique exige, simultanément, des programmes de tuberculination et
  d\'abattage chez les bovins, des politiques de pasteurisation universelle,
  un renforcement des infrastructures de surveillance dans les régions à
  faible revenu, et un suivi longitudinal robuste capable de résister aux
  perturbations systémiques. C\'est précisément ce que l\'approche One Health
  — unissant santé animale, santé humaine et santé des écosystèmes — vise
  à garantir.\n')

cat("\n══════════════════════════════════════════════════\n")
cat("  SCRIPT TERMINÉ — Fichiers PNG sauvegardés\n")
cat("══════════════════════════════════════════════════\n")
cat("  01_NA_overview.png\n")
cat("  02_PA_souche_comparatif.png\n")
cat("  03_PA_boxplot_mbovis.png\n")
cat("  04_ENV1_pays_region.png\n")
cat("  05_ENV1_pulm_spec_region.png\n")
cat("  06_ENV1_hotspot_regions.png\n")
cat("  07_ENV2_ident_zoonotic.png\n")
cat("  08_ENV2_pulm_mbovis_evol.png\n")
cat("  09_ENV2_ep_mbovis_juxtapose.png\n")
cat("  10_ENV2_ep_mbovis_tendance.png\n")
cat("  11_SYNTHESE_OneHealth.png\n")
