# ============================================================================
#  GADA TRAINING & SOLUTIONS — FORMATION EN MODÉLISATION SPATIALE
#  MODULE 2 : ANALYSE EXPLORATOIRE DES DONNÉES (EDA) AVANCÉE
#  Approche : ONE HEALTH — Couplage Épidémiologie / Environnement / Spatial
#  Application : Dynamique épidémiologique du paludisme (Base OMS WMR 2024 — Annexe 4F)
#  Auteur     : GADA Training & Solutions
#  Date       : 2025
#  Version    : 2.0 — Script de formation enrichi
# ============================================================================
#
# OBJECTIFS PÉDAGOGIQUES DE CE MODULE :
#   1. Maîtriser l'importation et le nettoyage d'une base de données réelle de l'OMS
#   2. Conduire une EDA automatisée avec le package DataExplorer
#   3. Produire des visualisations avancées avec ggplot2 (tendances, incertitudes)
#   4. Préparer un jeu de données structuré pour la jointure attributaire dans ArcGIS Pro
#
# DONNÉES SOURCE :
#   World Malaria Report 2024 — Annex 4F (Organisation Mondiale de la Santé)
#   URL officielle : https://www.who.int/publications/i/item/9789240101135
#
# PRÉREQUIS :
#   - R version >= 4.1.0
#   - RStudio (recommandé pour l'utilisation de View() et la navigation des graphiques)
# ============================================================================


# ============================================================================
# SECTION 1 — GESTION DES PACKAGES ET DE L'ENVIRONNEMENT DE TRAVAIL
# ============================================================================

# --- 1.1 Nettoyage de l'environnement global ---
# Bonne pratique : supprimer tous les objets en mémoire avant de commencer
# pour éviter les conflits avec d'éventuelles sessions précédentes.
rm(list = ls())

# --- 1.2 Suppression des avertissements inutiles durant la session de formation ---
# Permet d'avoir un affichage console plus propre pour les apprenants.
options(warn = -1)

# --- 1.3 Installation conditionnelle des packages ---
# Cette approche vérifie si chaque package est déjà installé avant de lancer
# un téléchargement, ce qui évite des erreurs inutiles dans une salle de formation.
packages_requis <- c("readxl", "tidyverse", "DataExplorer", "scales", "ggrepel")

for (pkg in packages_requis) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(paste0("Installation du package manquant : ", pkg, " ..."))
    install.packages(pkg, dependencies = TRUE)
  }
}

# --- 1.4 Chargement des bibliothèques ---
library(readxl)       # Importation native des fichiers Excel (.xlsx / .xls) sans dépendance Java
library(tidyverse)    # Méta-package : dplyr (manipulation), ggplot2 (visualisation), tidyr (restructuration)
library(DataExplorer) # EDA automatisée : rapports HTML, graphiques de distribution, valeurs manquantes
library(scales)       # Formatage des axes ggplot2 (milliers, pourcentages, notations scientifiques)
library(ggrepel)      # Étiquetage intelligent des points sans chevauchement (extension ggplot2)

message(">> Tous les packages ont été chargés avec succès.")


# ============================================================================
# SECTION 2 — IMPORTATION ET NETTOYAGE RIGOUREUX DE LA BASE DE DONNÉES
# ============================================================================

# --- 2.1 Définition du chemin d'accès au fichier source ---
# IMPORTANT : Adaptez ce chemin selon l'emplacement réel du fichier sur votre machine.
# Conseil : utiliser des slashes "/" même sous Windows pour éviter les erreurs d'échappement.
chemin_excel <- "E:/GADA Solution/site clone/gada-training/documents/wmr2024_annex_4f.xlsx"

# --- 2.2 Lecture du fichier brut depuis Excel ---
# L'argument `skip = 1` permet de sauter la première ligne du fichier OMS
# qui contient un titre général (non-tabulaire) et non des en-têtes de colonnes.
message(">> Importation du fichier Excel en cours...")
malaria_raw <- read_excel(chemin_excel, skip = 1)

# Vérification rapide post-importation : dimensions de la table brute
cat(sprintf(
  ">> Table brute importée : %d lignes x %d colonnes\n",
  nrow(malaria_raw),
  ncol(malaria_raw)
))

# --- 2.3 Nettoyage, renommage et typage des colonnes ---
# Le fichier OMS utilise des noms de colonnes complexes avec des caractères spéciaux.
# On renomme par position (index numérique) pour garantir la robustesse,
# puis on convertit chaque colonne dans son type R approprié.
message(">> Nettoyage et structuration des données...")

malaria_clean <- malaria_raw %>%

  # -- Renommage explicite par position de colonne --
  rename(
    region_pays         = 1, # Col. 1 : Identifiant géopolitique (Région OMS ou Pays)
    annee               = 2, # Col. 2 : Année d'observation épidémiologique
    population_a_risque = 3, # Col. 3 : Population estimée exposée au risque paludéen
    cas_estimes_inf     = 4, # Col. 4 : Borne inférieure de l'IC 95% — Cas estimés
    cas_estimes_point   = 5, # Col. 5 : Estimation centrale des cas (valeur pivot — morbidité)
    cas_estimes_sup     = 6, # Col. 6 : Borne supérieure de l'IC 95% — Cas estimés
    deces_estimes_inf   = 7, # Col. 7 : Borne inférieure de l'IC 95% — Décès estimés
    deces_estimes_point = 8, # Col. 8 : Estimation centrale des décès (valeur pivot — mortalité)
    deces_estimes_sup   = 9  # Col. 9 : Borne supérieure de l'IC 95% — Décès estimés
  ) %>%

  # -- Conversion des types de variables --
  # as.numeric() convertit les valeurs textuelles (ex: "–", "...", " ") en NA automatiquement.
  # C'est le comportement souhaité : la coercition silencieuse remplace les symboles
  # de données manquantes propres aux publications de l'OMS.
  mutate(
    annee               = as.numeric(annee),
    population_a_risque = as.numeric(population_a_risque),
    cas_estimes_inf     = as.numeric(cas_estimes_inf),
    cas_estimes_point   = as.numeric(cas_estimes_point),
    cas_estimes_sup     = as.numeric(cas_estimes_sup),
    deces_estimes_inf   = as.numeric(deces_estimes_inf),
    deces_estimes_point = as.numeric(deces_estimes_point),
    deces_estimes_sup   = as.numeric(deces_estimes_sup)
  ) %>%

  # -- Suppression des lignes entièrement vides (artefacts de mise en page Excel) --
  filter(!is.na(region_pays))

# --- 2.4 Rapport de validation post-nettoyage ---
cat(sprintf(
  ">> Table nettoyée : %d lignes x %d colonnes\n",
  nrow(malaria_clean),
  ncol(malaria_clean)
))
cat(sprintf(
  ">> Plage temporelle couverte : %d — %d\n",
  min(malaria_clean$annee, na.rm = TRUE),
  max(malaria_clean$annee, na.rm = TRUE)
))
cat(sprintf(
  ">> Nombre d'entités géographiques uniques : %d\n",
  n_distinct(malaria_clean$region_pays)
))

# --- 2.5 Inspection visuelle interactive dans RStudio ---
# Permet aux apprenants d'explorer la table dans l'interface RStudio
View(malaria_clean)

# Affichage des 6 premières lignes dans la console pour une vérification rapide
print(head(malaria_clean, 10))

# Résumé statistique univarié rapide pour toutes les colonnes numériques
print(summary(malaria_clean))


# ============================================================================
# SECTION 3 — EDA AUTOMATISÉE AVEC LE PACKAGE 'DataExplorer'
# ============================================================================

# --- 3.1 Diagnostic structurel de la base ---
# Visualise le type de données (numérique, texte, logique) et le nombre de valeurs
# uniques par colonne. Utile pour confirmer que le typage est correct après nettoyage.
message(">> Génération du diagnostic structurel (plot_str)...")
plot_str(malaria_clean)

# --- 3.2 Analyse quantitative des données manquantes ---
# Visualise le taux de complétude (en %) de chaque variable.
# Fondamental en approche One Health : avant tout croisement avec des données
# environnementales (NDVI, pluviométrie, température) ou de terrain, il est
# impératif de comprendre les lacunes de la base épidémiologique.
message(">> Analyse des valeurs manquantes (plot_missing)...")
plot_missing(malaria_clean)

# --- 3.3 Distribution univariée de toutes les variables numériques ---
# Génère automatiquement les histogrammes pour chaque colonne numérique.
# Permet de repérer : asymétries, distributions bimodales, valeurs aberrantes.
# NOTE : Les données de santé globale suivent souvent une distribution très asymétrique
# à droite (quelques pays concentrent une part immense de la charge mondiale).
message(">> Visualisation des distributions (plot_histogram)...")
plot_histogram(malaria_clean)

# --- 3.4 Analyse des corrélations entre variables numériques ---
# Génère une matrice de corrélation (Pearson par défaut).
# Permet de vérifier la cohérence interne : on attend une forte corrélation
# entre cas_estimes_point et deces_estimes_point (lien morbidité-mortalité).
message(">> Calcul et visualisation des corrélations (plot_correlation)...")
plot_correlation(
  data    = malaria_clean,
  type    = "continuous",
  title   = "Matrice de corrélation — Variables numériques (Paludisme OMS 2024)"
)

# --- 3.5 Génération du rapport EDA interactif complet ---
# Crée un document HTML autonome rassemblant toutes les analyses exploratives de base.
# Ce fichier sera sauvegardé dans le répertoire de travail courant (getwd()).
# Temps d'exécution estimé : 30 à 90 secondes selon la taille de la base.
message(">> Génération du rapport HTML interactif (create_report)...")
create_report(
  data        = malaria_clean,
  output_file = "Rapport_Exploratoire_Malaria_OMS.html",
  report_title = "EDA — Paludisme Mondial 2000-2023 | GADA Training"
)
message(">> Rapport HTML généré avec succès dans : ", getwd())


# ============================================================================
# SECTION 4 — VISUALISATIONS AVANCÉES AVEC GGPLOT2
#             PROFILS TEMPORELS ONE HEALTH — TENDANCE & VARIABILITÉ
# ============================================================================

# ---- 4.1 GRAPHIQUE 1 : Évolution de la charge globale avec bande d'incertitude ----
# Objectif pédagogique : montrer comment représenter simultanément la tendance centrale
# (médiane) et l'amplitude maximale (enveloppe de variabilité inter-pays).
# L'approche "ribbon + line" est standard en épidémiologie globale.

message(">> Construction du Graphique 1 — Tendance temporelle avec enveloppe...")

# Agrégation annuelle des indicateurs de morbidité
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
  # Zone ombrée extérieure : Amplitude totale (0 → Maximum mondial)
  # Représente la variabilité maximale entre les pays — analogue à une sensibilité environnementale
  geom_ribbon(
    aes(ymin = 0, ymax = cas_maximum),
    fill  = "steelblue",
    alpha = 0.08
  ) +
  # Zone ombrée intérieure : Interquartile (Q25 → Q75)
  # Représente la concentration de 50% des pays — cœur de la distribution
  geom_ribbon(
    aes(ymin = cas_q25, ymax = cas_q75),
    fill  = "steelblue",
    alpha = 0.25
  ) +
  # Ligne de la médiane mondiale — Indicateur de tendance centrale robuste
  geom_line(aes(y = cas_mediane), color = "firebrick", linewidth = 1.3) +
  geom_point(aes(y = cas_mediane), color = "firebrick", size = 2.5, shape = 21,
             fill = "white", stroke = 1.5) +
  # Formatage des axes : notation en millions pour la lisibilité
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_x_continuous(breaks = seq(2000, 2023, by = 2)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    plot.caption  = element_text(size = 9, color = "grey50", hjust = 0),
    axis.title    = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title    = "Évolution de la Charge Mondiale du Paludisme (2000–2023)",
    subtitle = paste0(
      "Ligne rouge : Médiane mondiale | Zone bleue foncée : Interquartile (Q25-Q75)",
      "\nZone bleue claire : Amplitude maximale (enveloppe de variabilité inter-pays)"
    ),
    x        = "Année d'observation",
    y        = "Cas de paludisme estimés (millions)",
    caption  = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ---- 4.2 GRAPHIQUE 2 : Relation Morbidité ↔ Mortalité (Nuage de Points Avancé) ----
# Objectif pédagogique : explorer la corrélation cas-décès à l'échelle pays en 2023.
# Intègre la population à risque comme variable de taille (bulle proportionnelle).
# Ce type de graphique est central dans les analyses One Health pour cibler les pays
# présentant un ratio décès/cas anormalement élevé (accès aux soins limité).

message(">> Construction du Graphique 2 — Relation Morbidité-Mortalité 2023...")

# Filtrage pour l'année 2023 uniquement, sans valeurs manquantes clés
malaria_2023 <- malaria_clean %>%
  filter(
    annee == 2023,
    !is.na(cas_estimes_point),
    !is.na(deces_estimes_point),
    !is.na(population_a_risque),
    # Exclusion des lignes de résumés régionaux (généralement en majuscules)
    !grepl("^[A-Z\\s]+$", region_pays)
  ) %>%
  mutate(
    # Calcul du taux de létalité apparent (CFR = Case Fatality Ratio)
    # Indicateur clé de la qualité du système de santé local
    taux_letalite = (deces_estimes_point / cas_estimes_point) * 100
  )

# Identification des 5 pays avec la charge en cas la plus élevée pour l'étiquetage
top5_cas <- malaria_2023 %>%
  slice_max(order_by = cas_estimes_point, n = 5)

ggplot(
  data = malaria_2023,
  aes(x = cas_estimes_point, y = deces_estimes_point, size = population_a_risque)
) +
  # Bulles proportionnelles à la population à risque, colorées par taux de létalité
  geom_point(aes(color = taux_letalite), alpha = 0.65, shape = 16) +
  # Étiquetage intelligent des pays les plus touchés (sans chevauchement)
  geom_label_repel(
    data         = top5_cas,
    aes(label    = region_pays),
    size         = 3.2,
    fontface     = "bold",
    box.padding  = 0.4,
    max.overlaps = 10,
    color        = "grey20"
  ) +
  # Palette de couleurs divergente : vert (faible létalité) → rouge (forte létalité)
  scale_color_gradient(
    low    = "#27ae60",
    high   = "#c0392b",
    name   = "Taux de\nlétalité (%)"
  ) +
  # Taille des bulles proportionnelle à la population
  scale_size_continuous(
    name   = "Population\nà risque",
    labels = label_number(scale = 1e-6, suffix = " M"),
    range  = c(2, 14)
  ) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = " M")) +
  scale_y_continuous(labels = label_number(scale = 1e-3, suffix = " k")) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  labs(
    title    = "Morbidité vs. Mortalité Paludéenne par Pays — 2023",
    subtitle = "Taille des bulles : Population à risque | Couleur : Taux de létalité apparent (Décès/Cas × 100)",
    x        = "Cas estimés (millions)",
    y        = "Décès estimés (milliers)",
    caption  = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ---- 4.3 GRAPHIQUE 3 : Classement des 15 pays les plus touchés en 2023 ----
# Objectif pédagogique : produire un graphique de classement (lollipop chart)
# mettant en évidence la concentration de la charge mondiale dans quelques pays.
# C'est une visualisation directement exploitable pour une présentation ou un rapport.

message(">> Construction du Graphique 3 — Top 15 pays (Lollipop Chart)...")

top15_pays <- malaria_2023 %>%
  slice_max(order_by = cas_estimes_point, n = 15) %>%
  mutate(
    # Réordonnancement des niveaux pour un affichage décroissant dans ggplot2
    region_pays = reorder(region_pays, cas_estimes_point)
  )

ggplot(data = top15_pays, aes(x = region_pays, y = cas_estimes_point)) +
  # Tige du lollipop
  geom_segment(
    aes(xend = region_pays, yend = 0),
    color    = "steelblue",
    linewidth = 0.8
  ) +
  # Tête du lollipop : cercle plein
  geom_point(color = "firebrick", size = 4) +
  # Étiquette de valeur au bout de chaque tige
  geom_text(
    aes(label = label_number(scale = 1e-6, suffix = " M", accuracy = 0.1)(cas_estimes_point)),
    hjust  = -0.15,
    size   = 3.2,
    color  = "grey30"
  ) +
  coord_flip() +  # Inversion axes pour un graphique horizontal (meilleure lisibilité des pays)
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y   = element_text(face = "bold")
  ) +
  labs(
    title   = "Top 15 des Pays les Plus Touchés par le Paludisme — 2023",
    subtitle = "Classement décroissant selon l'estimation centrale des cas (OMS)",
    x       = NULL,
    y       = "Cas de paludisme estimés (millions)",
    caption = "Source : World Malaria Report 2024, OMS (Annexe 4F) | Analyse : GADA Training & Solutions"
  )


# ============================================================================
# SECTION 5 — PRÉPARATION ET EXPORTATION DES DONNÉES POUR ArcGIS Pro / ArcMap
# ============================================================================

# --- 5.1 Contexte de la jointure attributaire ---
# Pour réaliser une cartographie du paludisme dans ArcGIS Pro, il faut créer
# une table CSV "propre" pouvant être jointe à un Shapefile des frontières mondiales
# via le champ commun : region_pays ↔ NOM_PAYS (ou équivalent ISO Alpha-3).
#
# Critères de filtrage :
#   - Année 2023 uniquement (couche temporelle la plus récente disponible)
#   - Une seule ligne par entité géographique (pays)
#   - Exclusion des totaux régionaux (non cartographiables directement)
#   - Sélection des seules variables utiles pour la cartographie thématique

message(">> Préparation de la table pour exportation ArcGIS...")

malaria_arcgis <- malaria_clean %>%

  # -- Filtrage temporel --
  filter(annee == 2023) %>%

  # -- Exclusion des agrégats régionaux de l'OMS --
  # Ces lignes regroupent plusieurs pays et ne peuvent pas être jointes à
  # une entité géographique unique dans un Shapefile mondial.
  filter(!is.na(cas_estimes_point), !is.na(deces_estimes_point)) %>%
  filter(!grepl("^[A-Z\\s]+$", region_pays)) %>%

  # -- Calcul d'indicateurs dérivés utiles pour la cartographie thématique --
  mutate(
    # Taux de létalité apparent (CFR) : indicateur de qualité des soins
    taux_letalite_pct = round((deces_estimes_point / cas_estimes_point) * 100, 4),

    # Incidence estimée pour 1 000 habitants à risque
    incidence_pour_1000 = round((cas_estimes_point / population_a_risque) * 1000, 4)
  ) %>%

  # -- Sélection finale des colonnes exportées --
  select(
    region_pays,          # Clé de jointure pour ArcGIS (à harmoniser avec le Shapefile)
    annee,                # Année de référence (2023)
    population_a_risque,  # Dénominateur pour les calculs de taux
    cas_estimes_inf,      # Borne inférieure IC95% — pour cartographie de l'incertitude
    cas_estimes_point,    # Valeur centrale des cas (variable principale de la carte)
    cas_estimes_sup,      # Borne supérieure IC95%
    deces_estimes_inf,    # Borne inférieure IC95% — Décès
    deces_estimes_point,  # Valeur centrale des décès (mortalité)
    deces_estimes_sup,    # Borne supérieure IC95% — Décès
    taux_letalite_pct,    # CFR calculé (%) — variable thématique dérivée
    incidence_pour_1000   # Incidence pour 1 000 hab. à risque — variable standardisée
  ) %>%

  # -- Tri alphabétique par pays pour faciliter la vérification visuelle --
  arrange(region_pays)

# --- 5.2 Rapport de validation avant exportation ---
cat(sprintf(
  ">> Table pour ArcGIS : %d pays x %d variables\n",
  nrow(malaria_arcgis),
  ncol(malaria_arcgis)
))

# --- 5.3 Exportation au format CSV (encodage UTF-8 sans BOM) ---
# write_csv() du tidyverse produit un CSV UTF-8 sans BOM, compatible avec ArcGIS.
# Éviter write.csv() de base R qui ajoute des numéros de ligne par défaut.
chemin_export_csv <- "malaria_arcgis_2023.csv"
write_csv(malaria_arcgis, chemin_export_csv)

message(paste0(">> Fichier CSV exporté avec succès : ", chemin_export_csv))
cat(sprintf("   => Répertoire de travail actuel : %s\n", getwd()))


# ============================================================================
# SECTION 6 — NOTES D'UTILISATION DANS ArcGIS PRO (Mémo Formateur)
# ============================================================================

# ÉTAPES DANS ArcGIS Pro (à projeter pour les apprenants) :
#
#   ÉTAPE A — Charger la couche géographique de référence :
#     Catalogue → Dossier de données → Glisser le Shapefile "World_Countries.shp"
#     (ou l'équivalent de votre géodatabase) dans la Map View.
#
#   ÉTAPE B — Ajouter la table CSV :
#     Map → Add Data → Table → Sélectionner "malaria_arcgis_2023.csv"
#     La table apparaît dans la table des matières (Contents) sans représentation géographique.
#
#   ÉTAPE C — Jointure attributaire (Add Join) :
#     Clic droit sur la couche World_Countries → Joins and Relates → Add Join
#       - Input Join Field (Shapefile)  : COUNTRY_NAME  (ou ISO_A3 selon le Shapefile)
#       - Join Table                    : malaria_arcgis_2023
#       - Output Join Field (CSV)       : region_pays
#     → Valider. La table des attributs du Shapefile est maintenant enrichie.
#
#   ÉTAPE D — Symbologie (Symbology) :
#     Clic droit sur la couche → Symbology → Graduated Colors
#       - Field : cas_estimes_point  (ou incidence_pour_1000 pour une carte normalisée)
#       - Method : Natural Breaks (Jenks) — recommandé pour les données épidémiologiques
#       - Classes : 5
#     → Ajuster la palette (ex: jaune → orange → rouge pour la charge paludéenne)
#
#   NOTE IMPORTANTE — Harmonisation des noms de pays :
#     Les noms de pays dans la colonne "region_pays" peuvent différer de ceux
#     du Shapefile utilisé. Un nettoyage manuel ou une table de correspondance
#     ISO 3166-1 Alpha-3 est souvent nécessaire pour garantir une jointure complète.

# ============================================================================
# FIN DU SCRIPT — GADA TRAINING & SOLUTIONS
# ============================================================================

message("============================================================")
message(" Script EDA Paludisme — Exécution complète et réussie !    ")
message(" Prêt pour la phase cartographique dans ArcGIS Pro.        ")
message("============================================================")
