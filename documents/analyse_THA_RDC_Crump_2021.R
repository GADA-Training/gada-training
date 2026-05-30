# =============================================================================
# SCRIPT R — MODÉLISATION STATISTIQUE DE LA THA EN RDC
# Document pédagogique : Approche One Health
# Données : Crump et al. (2021) — Atlas OMS de la THA
# Répertoire : E:\GADA Solution\Formation Rusers et Gada\Modélisation
# Auteur : GADA Solution — Formation 2024
# =============================================================================
# STRUCTURE DU SCRIPT :
#   PARTIE 1 — Installation des packages et chemins
#   PARTIE 2 — Importation et nettoyage des données
#   PARTIE 3 — Exploration des données (descriptif + visualisation)
#   PARTIE 4 — Modélisation SEIR compartimentale
#   PARTIE 5 — Inférence bayésienne MCMC (Metropolis-Hastings)
#   PARTIE 6 — Diagnostics de convergence (R-hat, ESS, LOO-CV)
#   PARTIE 7 — Projections prospectives 2017–2030
#   PARTIE 8 — Résumé et export des résultats
# =============================================================================


# ─────────────────────────────────────────────────────────────────────────────
# PARTIE 1 — INSTALLATION DES PACKAGES ET CONFIGURATION
# ─────────────────────────────────────────────────────────────────────────────

# Liste de tous les packages nécessaires
packages_requis <- c(
  # Manipulation et visualisation
  "readxl",      # Lecture des fichiers Excel (.xlsx)
  "tidyverse",   # Collection : dplyr, tidyr, ggplot2, purrr, readr...
  "scales",      # Mise en forme des axes ggplot (%, milliers...)
  "viridis",     # Palettes accessibles (daltonisme)
  "patchwork",   # Assembler plusieurs graphiques ggplot
  "corrplot",    # Matrice de corrélation visuelle
  # Modélisation ODE
  "deSolve",     # Solveur d'équations différentielles ordinaires (ODE)
  # Inférence bayésienne et MCMC
  "coda",        # Diagnostics des chaînes MCMC (R-hat, ESS, traceplots)
  "MCMCpack",    # Fonctions MCMC utilitaires
  "bayesplot",   # Visualisation bayésienne (posterior, traceplots)
  # Validation et modèles
  "loo",         # Validation croisée LOO-CV et LOOIC
  "broom",       # Mise en forme des sorties de modèles (tidy)
  "lme4"         # Modèles mixtes (effets aléatoires par zone)
)

# Installation automatique des packages manquants
packages_manquants <- packages_requis[!packages_requis %in%
                                        installed.packages()[, "Package"]]
if (length(packages_manquants) > 0) {
  message("⚙  Installation : ", paste(packages_manquants, collapse = ", "))
  install.packages(packages_manquants, dependencies = TRUE)
}

# Chargement silencieux de tous les packages
invisible(lapply(packages_requis, library, character.only = TRUE))

cat("✔ Tous les packages sont chargés.\n")

# ── Chemins ──────────────────────────────────────────────────────────────────
# Répertoire principal contenant les données et où seront sauvegardés
# les graphiques et les résultats exportés
REP_DONNEES <- "E:/GADA Solution/Formation Rusers et Gada/Modélisation"

# Nom du fichier Excel fourni par Crump et al. (2021)
NOM_FICHIER <- "THA_RDC_Crump_et_al_2021.xlsx"

# Chemin complet vers le fichier de données
CHEMIN_FICHIER <- file.path(REP_DONNEES, NOM_FICHIER)

# Répertoire de sortie pour les graphiques (créé automatiquement)
REP_GRAPHIQUES <- file.path(REP_DONNEES, "Graphiques_R")
if (!dir.exists(REP_GRAPHIQUES)) dir.create(REP_GRAPHIQUES, recursive = TRUE)

# Vérification de l'existence du fichier avant toute opération
if (!file.exists(CHEMIN_FICHIER)) {
  stop("ERREUR : Fichier introuvable ─ ", CHEMIN_FICHIER, "\n",
       "Vérifiez le chemin ou le nom du fichier.")
}

cat("✔ Fichier trouvé :", CHEMIN_FICHIER, "\n")

# Graine aléatoire pour la reproductibilité des simulations MCMC
set.seed(2024)


# ─────────────────────────────────────────────────────────────────────────────
# PARTIE 2 — IMPORTATION ET NETTOYAGE DES DONNÉES
# ─────────────────────────────────────────────────────────────────────────────
# Source : Atlas de la THA (OMS) — 117 573 enregistrements individuels
# (agrégés ici par zone × année) sur 168 zones endémiques, 2000–2016

cat("\n── Importation des 7 feuilles Excel ────────────────────────────\n")

# ── 2.1 Importation de chaque feuille ──────────────────────────────────────

# Feuille 1 : Cas annuels par zone (passifs, actifs, stade 1, stade 2, réels)
df_cas_zone <- read_excel(CHEMIN_FICHIER, sheet = "1_Cas_observes_par_zone")

# Feuille 2 : Caractéristiques des zones (population, R0, durées de stade)
df_zones <- read_excel(CHEMIN_FICHIER, sheet = "2_Zones_sante_endemiques")

# Feuille 3 : Taux de détection annuels par zone (passive et active)
df_depistage <- read_excel(CHEMIN_FICHIER, sheet = "3_Donnees_depistage")

# Feuille 4 : Résultats agrégés par ancienne province
df_province <- read_excel(CHEMIN_FICHIER, sheet = "4_Resultats_par_province")

# Feuille 5 : Totaux nationaux OMS année par année (2000–2016)
df_national <- read_excel(CHEMIN_FICHIER, sheet = "5_Totaux_nationaux_OMS")

# Feuille 6 : Estimations du R0 médian et IC95% par province
df_r0 <- read_excel(CHEMIN_FICHIER, sheet = "6_R0_estimations")

# Feuille 7 : Réductions de transmission 2000–2016 par province
df_reduction <- read_excel(CHEMIN_FICHIER, sheet = "7_Reductions_transmission")

cat("✔ 7 feuilles importées.\n")

# ── 2.2 Nettoyage des données ───────────────────────────────────────────────

# ❶ Standardisation des noms de zones de santé
# (En production, charger une table de correspondance CSV construite manuellement
#  pour harmoniser les variantes orthographiques entre années)
# Exemple de structure attendue :
#   correspondance <- read_csv(file.path(REP_DONNEES, "table_correspondance_zones.csv"))
#   df_cas_zone <- df_cas_zone |>
#     left_join(correspondance, by = c("zone_nom" = "nom_ancien")) |>
#     mutate(zone_nom = coalesce(nom_harmonise, zone_nom)) |>
#     select(-nom_harmonise)

# ❷ Suppression des valeurs aberrantes sur les années
df_cas_zone <- df_cas_zone |>
  filter(annee >= 2000, annee <= 2016)

df_depistage <- df_depistage |>
  filter(annee >= 2000, annee <= 2016)

# ❸ Conversion de la colonne "reduction_pct" (chaîne "93%") en proportion numérique
df_province <- df_province |>
  mutate(reduction_pct_num = as.numeric(gsub("%", "", reduction_pct)) / 100)

df_reduction <- df_reduction |>
  mutate(reduction_pct_num = as.numeric(gsub("%", "", reduction_pct)) / 100)

# ❹ Vérification des valeurs manquantes par feuille
cat("\nValeurs manquantes par feuille :\n")
list(
  Cas_zone  = df_cas_zone,
  Zones     = df_zones,
  Depistage = df_depistage,
  Province  = df_province,
  National  = df_national,
  R0        = df_r0,
  Reduction = df_reduction
) |>
  lapply(function(df) colSums(is.na(df))) |>
  print()

# ❺ Imputation des valeurs manquantes (médiane par zone × année)
# Appliqué sur les taux de détection si nécessaire
df_depistage <- df_depistage |>
  group_by(zone_id, annee) |>
  mutate(
    eta_passif = if_else(is.na(eta_passif), median(eta_passif, na.rm = TRUE), eta_passif),
    eta_actif  = if_else(is.na(eta_actif),  median(eta_actif,  na.rm = TRUE), eta_actif)
  ) |>
  ungroup()

# ❻ Agrégation temporelle et spatiale (structure attendue par le modèle)
# Les données individuelles seraient agrégées ici si disponibles en format long.
# Avec les données résumées fournies, on les consolide directement :
donnees_agregees <- df_cas_zone |>
  group_by(zone_id, zone_nom, ancienne_province, annee) |>
  summarise(
    cas_actifs_s1  = round(cas_actif  * 0.4),   # Estimation stade 1 actif (40%)
    cas_actifs_s2  = round(cas_actif  * 0.6),   # Estimation stade 2 actif (60%)
    cas_passifs_s1 = cas_stade1,                # Stade 1 passif
    cas_passifs_s2 = cas_stade2,                # Stade 2 passif
    cas_total      = cas_observes,
    cas_reels      = cas_reels,
    .groups = "drop"
  )

# ❼ Fusion des tables sur la clé (zone_id, annee) pour la modélisation
df_complet <- donnees_agregees |>
  left_join(
    df_zones |> dplyr::select(zone_id, population_2015, r0_median, r0_ic95_inf, 
                              r0_ic95_sup, duree_stade1_ans, duree_stade2_ans),
    by = "zone_id"
  ) |>
  left_join(
    df_depistage |> dplyr::select(zone_id, annee, eta_passif, eta_actif),
    by = c("zone_id", "annee")
  ) |>
  left_join(
    df_cas_zone |> dplyr::select(zone_id, annee, cas_observes),
    by = c("zone_id", "annee")
  )

cat("✔ Table fusionnée : df_complet (", nrow(df_complet), "lignes ×",
    ncol(df_complet), "colonnes)\n")
# ─────────────────────────────────────────────────────────────────────────────
# PARTIE 3 — EXPLORATION DES DONNÉES
# ─────────────────────────────────────────────────────────────────────────────

cat("\n── Partie 3 : Exploration des données ──────────────────────────\n")

# ── 3.1 Statistiques descriptives univariées ───────────────────────────────

# Statistiques annuelles agrégées
cat("\nStatistiques descriptives par année :\n")
donnees_agregees |>
  group_by(annee) |>
  summarise(
    n_zones   = n(),
    total_cas = sum(cas_total,  na.rm = TRUE),
    moy_cas   = round(mean(cas_total,   na.rm = TRUE), 1),
    med_cas   = median(cas_total,       na.rm = TRUE),
    sd_cas    = round(sd(cas_total,     na.rm = TRUE), 1),
    max_cas   = max(cas_total,          na.rm = TRUE)
  ) |>
  print(n = Inf)

# ── 3.2 Tendances temporelles ─────────────────────────────────────────────

# Graphique 1 : Évolution nationale (total OMS) avec aire colorée
g1 <- ggplot(df_national, aes(x = annee, y = cas_totaux)) +
  geom_area(fill = "#90CAF9", alpha = 0.35) +
  geom_line(color = "#1565C0", linewidth = 1.3) +
  geom_point(color = "#0D47A1", size = 3) +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Évolution des cas de THA en RDC — Totaux nationaux OMS (2000–2016)",
    subtitle = "Réduction de ~83 % entre 2000 et 2016",
    x = "Année", y = "Nombre de cas",
    caption  = "Source : OMS / Crump et al. (2021)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray40"))

print(g1)

ggsave(file.path(REP_GRAPHIQUES, "01_evolution_nationale.png"),
       g1, width = 10, height = 6, dpi = 300, 
       device = png, type = "cairo")
df_type <- donnees_agregees |>
  group_by(annee) |>
  summarise(
    cas_actifs_s1  = sum(cas_actifs_s1,  na.rm = TRUE),
    cas_actifs_s2  = sum(cas_actifs_s2,  na.rm = TRUE),
    cas_passifs_s1 = sum(cas_passifs_s1, na.rm = TRUE),
    cas_passifs_s2 = sum(cas_passifs_s2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = starts_with("cas"),
               names_to = "type", values_to = "n")

g2 <- ggplot(df_type, aes(x = annee, y = n, color = type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = c(
      cas_actifs_s1  = "#1F3864",
      cas_passifs_s1 = "#2E75B6",
      cas_actifs_s2  = "#C00000",
      cas_passifs_s2 = "#E07070"
    ),
    labels = c("Actifs stade 1", "Passifs stade 1",
               "Actifs stade 2", "Passifs stade 2"),
    name = "Type de cas"
  ) +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Évolution des cas de THA par mode de détection et stade (2000–2016)",
    subtitle = "Distinction passif / actif et stade 1 / stade 2",
    x = "Année", y = "Nombre de cas",
    caption  = "Source : Crump et al. (2021)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")
print(g2)


# ── 3.3 Hétérogénéité spatiale ────────────────────────────────────────────
# Vérifier les noms des colonnes dans df_complet
colnames(df_complet)
# Top 10 zones les plus touchées (2000–2016 cumulé)
cat("\nTop 10 zones les plus touchées (cumul 2000-2016) :\n")
df_complet |>
  group_by(zone_id, zone_nom, ancienne_province) |>
  summarise(total_cas = sum(cas_total, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_cas)) |>
  head(10) |>
  print()

# Graphique 3 : Carte de chaleur (zone × année) — top 20 zones
top20_zones <- df_complet |>
  group_by(zone_nom) |>
  summarise(total = sum(cas_total, na.rm = TRUE), .groups = "drop") |>
  slice_max(total, n = 20) |>
  pull(zone_nom)

g3 <- df_complet |>
  filter(zone_nom %in% top20_zones) |>
  ggplot(aes(x = annee, y = zone_nom, fill = cas_total)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "plasma", name = "Cas", labels = comma_format()) +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  labs(
    title    = "Carte de chaleur : Cas de THA dans les 20 zones les plus touchées",
    subtitle = "Intensité = nombre de cas annuels observés",
    x = "Année", y = "Zone de santé",
    caption  = "Source : Crump et al. (2021)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 8))
print(g3)

# ── 3.4 Analyse des stades de la maladie ─────────────────────────────────
# La proportion de stade 2 est un indicateur clé du délai de diagnostic.
# Une proportion élevée signale un accès limité aux soins ou un faible
# dépistage actif.

g4 <- df_complet |>
  filter(cas_total > 0) |>
  mutate(prop_s2 = (cas_passifs_s2 + cas_actifs_s2) / cas_total) |>
  group_by(ancienne_province, annee) |>
  summarise(prop_s2_med = median(prop_s2, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = annee, y = prop_s2_med, color = ancienne_province)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.2, span = 0.6) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_color_viridis_d(name = "Province") +
  labs(
    title    = "Proportion de cas de stade 2 par province (2000–2016)",
    subtitle = "Médiane par province — lissage LOESS",
    x = "Année", y = "Proportion de stade 2 (%)",
    caption  = "Source : Crump et al. (2021)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "right")
print(g4)

# ── 3.5 Matrice de corrélation ────────────────────────────────────────────

mat_cor <- df_complet |>
  dplyr::select(cas_total, cas_actifs_s1, cas_passifs_s1,
                cas_actifs_s2, cas_passifs_s2, r0_median, eta_passif, eta_actif) |>
  drop_na() |>
  cor(use = "complete.obs")

print(mat_cor)

#MATRICE DE CORRÉLATION PROFESSIONNELLE (STYLE METAN)

# Sélection des variables pour la corrélation (8 variables THA)
variables_correlation <- c("cas_total", "cas_actifs_s1", "cas_passifs_s1",
                           "cas_actifs_s2", "cas_passifs_s2", "r0_median", 
                           "eta_passif", "eta_actif")

# Vérification des variables existantes
variables_existantes <- variables_correlation[variables_correlation %in% colnames(df_complet)]

cat("=== MATRICE DE CORRÉLATION ===\n")
cat("Variables utilisées pour la matrice de corrélation:\n")
print(variables_existantes)

# Préparation des données pour la corrélation
data_cor <- df_complet %>%
  dplyr::select(all_of(variables_existantes)) %>%
  tidyr::drop_na()

cat("\nNombre d'observations complètes (sans NA) :", nrow(data_cor), "\n")

# Vérifier qu'il reste suffisamment de données
if(nrow(data_cor) > 5) {
  
  # Calcul de la matrice de corrélation avec metan
  cat("\nCalcul de la matrice de corrélation...\n")
  matrice_cor <- metan::corr_coef(data_cor)
  
  # Affichage de la matrice
  cat("\n=== MATRICE DES COEFFICIENTS DE CORRÉLATION ===\n")
  print(matrice_cor)
  
  # Visualisation avec metan
  cat("\nVisualisation de la matrice de corrélation...\n")
  plot_matrice <- plot(matrice_cor)
  print(plot_matrice)
  
  # Sauvegarde de la matrice en CSV
  fichier_csv <- file.path(REP_DONNEES, "matrice_correlation_THA.csv")
  write.csv(as.data.frame(matrice_cor$cor), fichier_csv, row.names = TRUE)
  cat("\n[Succès] Matrice de corrélation sauvegardée :", fichier_csv, "\n")
  
  # Sauvegarde du graphique (optionnel, car plot() l'affiche déjà)
  fichier_png <- file.path(REP_GRAPHIQUES, "05_matrice_correlation_metan.png")
  png(fichier_png, width = 1000, height = 800, res = 150)
  plot(matrice_cor)
  dev.off()
  cat("[Succès] Graphique sauvegardé :", fichier_png, "\n")
  
} else {
  cat("Erreur : Données insuffisantes pour calculer la matrice de corrélation.\n")
  cat("Minimum requis : 6 observations complètes. Observations disponibles :", nrow(data_cor), "\n")
}

cat("\n=== FIN DE L'ANALYSE DE CORRÉLATION ===\n")


# ─────────────────────────────────────────────────────────────────────────────
# PARTIE 4 — MODÈLE COMPARTIMENTAL SEIR (DOUBLE POPULATION)
# ─────────────────────────────────────────────────────────────────────────────
# Structure du modèle (Crump et al. 2021, approche One Health) :
#
#   COMPARTIMENTS HUMAINS :
#     S_L(t) : Susceptibles à faible risque  (~90% de la population)
#     S_H(t) : Susceptibles à haut risque    (~10%, exposés professionnellement)
#     E(t)   : Exposés (incubation : 21 jours)
#     I1(t)  : Infectieux stade 1 (hémolymphatique, ~18 mois sans traitement)
#     I2(t)  : Infectieux stade 2 (méningo-encéphalitique, ~12 mois, fatal sans ttt)
#     R(t)   : Traités / Guéris
#
#   COMPARTIMENTS VECTORIELS (glossines — population supposée constante) :
#     V_S(t) : Mouches saines
#     V_E(t) : Mouches exposées (incubation : 20 jours)
#     V_I(t) : Mouches infectieuses
#
#   FORCES D'INFECTION :
#     lambda_j(t) = a * b * V_I(t)/N_H * c_j   (humain, j = L ou H)
#     lambda_V(t) = a * c * (I1 + I2) / N_H    (vecteur)

# ── 4.1 Modélisation des taux de détection (time-varying) ─────────────────

# Détection passive — fonction logistique croissante (amélioration des soins)
# Paramètres : delta_min = 0.05 ; delta_max = 0.50 ; k = 0.3 ; t0 = 2008
taux_detection_passif <- function(t,
                                  delta_min = 0.05,
                                  delta_max = 0.50,
                                  k = 0.30,
                                  t0 = 2008) {
  delta_min + (delta_max - delta_min) / (1 + exp(-k * (t - t0)))
}

# Détection passive Bandundu — modèle piecewise (effet « bosse » 2005–2008)
# L'augmentation paradoxale s'explique par un programme intensif temporaire
taux_detection_bandundu <- function(t) {
  dplyr::case_when(
    t >= 2000 & t <= 2004 ~ 0.08 + 0.02 * (t - 2000),
    t >= 2005 & t <= 2008 ~ 0.16 + 0.08 * (t - 2004),
    t >= 2009 & t <= 2012 ~ 0.48 - 0.06 * (t - 2008),
    t >= 2013 & t <= 2016 ~ 0.24 - 0.02 * (t - 2012),
    TRUE ~ 0.20  # Valeur par défaut hors période
  )
}

# Détection active — décroissance exponentielle à partir de 2010
# (réduction des financements des équipes mobiles)
taux_detection_actif <- function(t, delta0 = 0.35, alpha = 0.15) {
  delta0 * exp(-alpha * pmax(0, t - 2010))
}

# Graphique 6 : Visualisation des fonctions de détection
annees_seq <- seq(2000, 2016, by = 0.1)
df_detection <- tibble(
  annee       = annees_seq,
  passif_std  = taux_detection_passif(annees_seq),
  passif_band = taux_detection_bandundu(annees_seq),
  actif       = taux_detection_actif(annees_seq)
) |>
  pivot_longer(-annee, names_to = "type", values_to = "taux")

g6 <- ggplot(df_detection, aes(x = annee, y = taux, color = type)) +
  geom_line(linewidth = 1.3) +
  scale_color_manual(
    values = c(passif_std  = "#1565C0",
               passif_band = "#E65100",
               actif       = "#2E7D32"),
    labels = c("Passif (logistique std)",
               "Passif Bandundu (piecewise)",
               "Actif (exponentiel décroissant)"),
    name = "Modèle"
  ) +
  scale_x_continuous(breaks = seq(2000, 2016, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.55)) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "gray50") +
  annotate("text", x = 2010.2, y = 0.50,
           label = "Réduction dépistage actif", color = "gray40", size = 3.2, hjust = 0) +
  labs(
    title    = "Modèles des taux de détection de la THA (2000–2016)",
    subtitle = "Passif logistique | Bandundu piecewise | Actif exponentiel",
    x = "Année", y = "Taux de détection",
    caption  = "Source : Crump et al. (2021) — paramètres calibrés"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")

print(g6)

# ── 4.2 Système d'équations différentielles du modèle SEIR-vectoriel ──────

tha_odes <- function(t, state, params) {
  with(as.list(c(state, params)), {

    # ─ Forces d'infection ─
    # Humaine : proportionnelle à la densité de mouches infectieuses
    lambda_L <- a * b * V_I / N_H * c_L    # Faible risque
    lambda_H <- a * b * V_I / N_H * c_H    # Haut risque
    # Vectorielle : proportionnelle à la densité d'humains infectieux
    lambda_V <- a * cc * (I1 + I2) / N_H

    # ─ Taux de détection (time-varying) ─
    # Choix selon la province : logistique standard ou Bandundu piecewise
    if (province == "Bandundu") {
      delta1 <- taux_detection_bandundu(t)
    } else {
      delta1 <- taux_detection_passif(t)
    }
    delta1_act <- taux_detection_actif(t)
    delta2     <- delta1 * 0.8   # Stade 2 légèrement moins bien détecté

    # ─ Équations humaines ─
    dS_L <- -lambda_L * S_L
    dS_H <- -lambda_H * S_H
    dE   <-  lambda_L * S_L + lambda_H * S_H - sigma * E
    dI1  <-  sigma * E - (gamma1 + delta1 + delta1_act) * I1
    dI2  <-  gamma1 * I1 - (gamma2 + delta2) * I2
    dR   <- (delta1 + delta1_act) * I1 + delta2 * I2

    # ─ Équations vectorielles ─
    dV_S <- mu_V * N_V - (lambda_V + mu_V) * V_S
    dV_E <- lambda_V * V_S - (nu + mu_V) * V_E
    dV_I <- nu * V_E - mu_V * V_I

    list(c(dS_L, dS_H, dE, dI1, dI2, dR, dV_S, dV_E, dV_I))
  })
}

# ── 4.3 Paramètres biologiques du modèle ─────────────────────────────────

# Fonction de calcul du R0 théorique (formule analytique pour modèle vectoriel)
# R0 = [a² * b * c * N_V/(mu_V*N_H)] * [1/gamma1 + gamma1/(gamma1+mu_H) * 1/gamma2]
calculer_R0 <- function(a, b, cc, N_V, N_H, mu_V, mu_H, gamma1, gamma2) {
  bloc_vecteur <- (a^2 * b * cc * N_V) / (mu_V * N_H)
  bloc_duree   <- 1/gamma1 + (gamma1/(gamma1 + mu_H)) * (1/gamma2)
  sqrt(bloc_vecteur * bloc_duree)  # sqrt car formule de la next-generation matrix
}

# Paramètres de référence (issus de la littérature et de Crump et al. 2021)
# Paramètres de référence CORRIGÉS (tout en JOURS)
params_ref_corriges <- list(
  # Biologie de la transmission
  a      = 0.05,          # Taux de piqûre par glossine par jour
  b      = 0.40,          # Probabilité transmission mouche → humain
  cc     = 0.30,          # Probabilité transmission humain → mouche
  
  # Facteurs d'exposition relative
  c_L    = 0.10,          # Exposition relative : faible risque
  c_H    = 0.90,          # Exposition relative : haut risque
  
  # Démographie humaine
  N_H    = 50000,
  mu_H   = 1/(65*365),    # Mortalité humaine par JOUR (65 ans → 23725 jours)
  
  # Démographie vectorielle
  N_V    = 5000,
  mu_V   = 1/33,          # Mortalité glossines par JOUR (33 jours)
  nu     = 1/20,          # Taux fin incubation vecteur par JOUR (20 jours)
  
  # Durées cliniques (en JOURS)
  sigma  = 1/21,          # Taux fin incubation humaine par JOUR (21 jours)
  gamma1 = 1/(1.5*365),   # Progression stade 1→2 par JOUR (18 mois = 547.5 jours)
  gamma2 = 1/(1.0*365),   # Résolution stade 2 par JOUR (12 mois = 365 jours)
  
  # Province par défaut
  province = "Autre"
)

# Recalculer R0
R0_calcule_corrige <- with(params_ref_corriges,
                           calculer_R0(a, b, cc, N_V, N_H, mu_V, mu_H, gamma1, gamma2))
cat(sprintf("\n✔ R0 corrigé = %.4f (devrait être ~1.05)\n", R0_calcule_corrige))

# Conditions initiales (proportions réalistes)
etat_initial_corrige <- c(
  S_L = params_ref_corriges$N_H * 0.88,
  S_H = params_ref_corriges$N_H * 0.10,
  E   = 10,               # Très peu d'exposés au départ
  I1  = 5,                # Quelques cas stade 1
  I2  = 2,                # Quelques cas stade 2
  R   = 0,                # Pas de guéris
  V_S = params_ref_corriges$N_V * 0.99,
  V_E = params_ref_corriges$N_V * 0.01,
  V_I = 0                 # Pas de mouches infectées au départ
)

# Grille temporelle (en JOURS, mais on veut afficher en années)
jours <- seq(0, 16*365, by = 30)  # 16 ans en pas de 30 jours
temps_sim_jours <- jours / 365    # Convertir en années pour l'affichage

# Intégration numérique
solution_ode_corrigee <- ode(
  y      = etat_initial_corrige,
  times  = jours,                  # En jours pour le solveur
  func   = tha_odes,
  parms  = params_ref_corriges,
  method = "rk4"
)

# Convertir en dataframe avec temps en années
df_ode_corrige <- as.data.frame(solution_ode_corrigee)
df_ode_corrige$time_annees <- df_ode_corrige$time / 365

