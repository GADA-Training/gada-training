# ==============================================================================
# SCRIPT R COMPLET - ANALYSES STATISTIQUES AGRONOMIE & ENVIRONNEMENT
# VERSION CORRIGÉE - Lecture correcte des en-têtes
# ==============================================================================

# ==============================================================================
# 1. CHARGEMENT DES LIBRAIRIES
# ==============================================================================

# Installation automatique des packages manquants
packages <- c("readxl", "dplyr", "ggplot2", "car", "lme4", "lmerTest",
              "MASS", "performance", "see", "FactoMineR", "factoextra",
              "forecast", "gstat", "sp", "tidyverse", "corrplot",
              "pscl", "MuMIn", "gridExtra", "tseries")

for(pkg in packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# ==============================================================================
# 2. IMPORTATION DES DONNÉES
# ==============================================================================

# À MODIFIER: Indiquez le chemin vers votre fichier Excel
file_path <- "Donnees_Agronomie_Environnement.xlsx"

if(file.exists(file_path)) {
  cat("Fichier trouvé. Importation des données...\n")
  
  # Pour chaque feuille: 
  # - skip = 1 : saute la première ligne (description)
  # - col_names = TRUE : utilise la ligne suivante comme noms de colonnes
  
  # Production Végétale
  production_vegetale <- read_excel(file_path, sheet = "Production_Vegetale", 
                                    skip = 1, col_names = TRUE)
  
  # Production Animale
  production_animale <- read_excel(file_path, sheet = "Production_Animale", 
                                   skip = 1, col_names = TRUE)
  
  # Environnement
  environnement <- read_excel(file_path, sheet = "Environnement", 
                              skip = 1, col_names = TRUE)
  
  # Séries Temporelles
  series_temporelles <- read_excel(file_path, sheet = "Series_Temporelles", 
                                   skip = 1, col_names = TRUE)
  
  # Nettoyer les noms de colonnes (supprimer les espaces et caractères spéciaux)
  nettoyer_noms <- function(noms) {
    noms <- gsub(" ", "_", noms)
    noms <- gsub("\\(", "", noms)
    noms <- gsub("\\)", "", noms)
    noms <- gsub("°", "", noms)
    noms <- gsub("%", "pct", noms)
    noms <- gsub("/", "_", noms)
    noms <- gsub("-", "_", noms)
    noms <- gsub("__", "_", noms)
    noms <- gsub("_$", "", noms)
    return(noms)
  }
  
  names(production_vegetale) <- nettoyer_noms(names(production_vegetale))
  names(production_animale) <- nettoyer_noms(names(production_animale))
  names(environnement) <- nettoyer_noms(names(environnement))
  names(series_temporelles) <- nettoyer_noms(names(series_temporelles))
  
  # Supprimer les lignes vides ou toutes NA
  production_vegetale <- production_vegetale %>% filter(!is.na(ID_Obs) | !is.na(Annee))
  production_animale <- production_animale %>% filter(!is.na(ID_Animal) | !is.na(Ferme))
  environnement <- environnement %>% filter(!is.na(ID_Parcelle) | !is.na(Zone))
  series_temporelles <- series_temporelles %>% filter(!is.na(Annee) | !is.na(Wilaya))
  
  cat("Données importées avec succès!\n")
  cat("Production végétale:", nrow(production_vegetale), "observations\n")
  cat("Production animale:", nrow(production_animale), "observations\n")
  cat("Environnement:", nrow(environnement), "observations\n")
  cat("Séries temporelles:", nrow(series_temporelles), "observations\n")
  
} else {
  cat("ERREUR: Fichier non trouvé à l'emplacement:", file_path, "\n")
  cat("Veuillez vérifier le chemin du fichier.\n")
  stop("Arrêt du script.")
}

# Afficher les noms des colonnes pour vérification
cat("\n=== Noms des colonnes ===\n")
cat("Production végétale:", paste(names(production_vegetale), collapse=", "), "\n")
cat("Production animale:", paste(names(production_animale), collapse=", "), "\n")
cat("Environnement:", paste(names(environnement), collapse=", "), "\n")
cat("Séries temporelles:", paste(names(series_temporelles), collapse=", "), "\n")

# ==============================================================================
# 3. FONCTIONS UTILITAIRES
# ==============================================================================

afficher_section <- function(titre) {
  cat("\n", rep("=", 80), sep="")
  cat("\n", titre, "\n")
  cat(rep("=", 80), "\n", sep="")
}

# ==============================================================================
# 4. ANALYSE DESCRIPTIVE PRÉLIMINAIRE
# ==============================================================================

afficher_section("ANALYSE DESCRIPTIVE PRÉLIMINAIRE")

# Production Végétale
cat("\n--- PRODUCTION VÉGÉTALE ---\n")
cat("Nombre d'observations:", nrow(production_vegetale), "\n")
cat("Variables disponibles:\n")
print(names(production_vegetale))

# Convertir les colonnes numériques
colonnes_numeriques <- c("Rendement_t_ha", "Dose_N_kg_ha", "Irrigation_mm", 
                         "Temp_moy_C", "Pluie_mm", "Densite_semis", 
                         "Poids_1000_grains_g", "Taux_proteine_pct", 
                         "Hauteur_plant_cm", "Indice_recolte")

for(col in colonnes_numeriques) {
  if(col %in% names(production_vegetale)) {
    production_vegetale[[col]] <- as.numeric(production_vegetale[[col]])
  }
}

if("Rendement_t_ha" %in% names(production_vegetale)) {
  cat("\nRésumé du rendement (t/ha):\n")
  print(summary(production_vegetale$Rendement_t_ha))
}

if("Variete" %in% names(production_vegetale)) {
  cat("\nVariétés présentes:\n")
  print(table(production_vegetale$Variete))
}

# Production Animale
cat("\n--- PRODUCTION ANIMALE ---\n")
cat("Nombre d'observations:", nrow(production_animale), "\n")

colonnes_animale <- c("Lait_305j_kg", "Poids_vif_kg", "Age_mois", 
                      "Duree_lactation_j", "TB_g_kg", "TP_g_kg", 
                      "CCS_log10", "GMQ_g_j")

for(col in colonnes_animale) {
  if(col %in% names(production_animale)) {
    production_animale[[col]] <- as.numeric(production_animale[[col]])
  }
}

if("Lait_305j_kg" %in% names(production_animale)) {
  cat("\nRésumé production laitière (kg/305j):\n")
  print(summary(production_animale$Lait_305j_kg))
}

if("Ferme" %in% names(production_animale)) {
  cat("\nFermes:\n")
  print(table(production_animale$Ferme))
}

if("Race" %in% names(production_animale)) {
  cat("\nRaces:\n")
  print(table(production_animale$Race))
}

# Environnement
cat("\n--- ENVIRONNEMENT ---\n")
cat("Nombre de parcelles:", nrow(environnement), "\n")

colonnes_env <- c("pH_eau", "MO_pct", "N_total_pct", "P_Olsen_mg_kg", 
                  "CEC_cmol_kg", "Argile_pct", "Nb_especes_veg", 
                  "Nb_vers_terre_m2", "Pollution_nitrate_mgL")

for(col in colonnes_env) {
  if(col %in% names(environnement)) {
    environnement[[col]] <- as.numeric(environnement[[col]])
  }
}

if("Zone" %in% names(environnement)) {
  cat("\nZones:\n")
  print(table(environnement$Zone))
}

if("Usage_sol" %in% names(environnement)) {
  cat("\nUsages du sol:\n")
  print(table(environnement$Usage_sol))
}

# ==============================================================================
# 5. PRODUCTION VÉGÉTALE - ANALYSES
# ==============================================================================

afficher_section("PRODUCTION VÉGÉTALE - ANALYSES")

# ------------------------------------------------------------------------------
# 5.1 ANOVA à 1 facteur - Effet de la variété sur le rendement
# ------------------------------------------------------------------------------

if(all(c("Rendement_t_ha", "Variete") %in% names(production_vegetale))) {
  
  cat("\n--- 5.1 ANOVA à 1 FACTEUR: Effet de la variété ---\n")
  
  modele_anova_variete <- aov(Rendement_t_ha ~ Variete, data = production_vegetale)
  print(summary(modele_anova_variete))
  
  # Test post-hoc de Tukey
  tukey_result <- TukeyHSD(modele_anova_variete)
  cat("\n--- Test post-hoc Tukey (différences significatives) ---\n")
  print(tukey_result)
  
  # Graphique
  p1 <- ggplot(production_vegetale, aes(x = Variete, y = Rendement_t_ha, fill = Variete)) +
    geom_boxplot(alpha = 0.7) +
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
    labs(title = "Rendement par variété de blé dur", 
         x = "Variété", y = "Rendement (t/ha)") +
    theme_minimal() +
    theme(legend.position = "none")
  print(p1)
  
  # ----------------------------------------------------------------------------
  # 5.2 ANOVA à 2 facteurs - Variété × Dose d'azote
  # ----------------------------------------------------------------------------
  
  if("Dose_N_kg_ha" %in% names(production_vegetale)) {
    cat("\n--- 5.2 ANOVA à 2 FACTEURS: Variété × Dose d'azote ---\n")
    
    # Catégorisation de la dose d'azote
    production_vegetale <- production_vegetale %>%
      mutate(Dose_N_cat = factor(case_when(
        Dose_N_kg_ha == 0 ~ "0",
        Dose_N_kg_ha == 40 ~ "40",
        Dose_N_kg_ha == 80 ~ "80",
        Dose_N_kg_ha == 120 ~ "120",
        Dose_N_kg_ha == 160 ~ "160",
        TRUE ~ "Autre"
      ), levels = c("0", "40", "80", "120", "160")))
    
    modele_anova_2fact <- aov(Rendement_t_ha ~ Variete * Dose_N_cat, data = production_vegetale)
    print(summary(modele_anova_2fact))
    
    # Graphique d'interaction
    interaction.plot(x.factor = production_vegetale$Dose_N_cat,
                     trace.factor = production_vegetale$Variete,
                     response = production_vegetale$Rendement_t_ha,
                     fun = mean, type = "b", col = 1:4, pch = 1:4,
                     xlab = "Dose d'azote (kg/ha)", 
                     ylab = "Rendement moyen (t/ha)",
                     main = "Interaction Variété × Dose d'azote")
  }
  
  # ----------------------------------------------------------------------------
  # 5.3 ANCOVA - Avec covariable irrigation
  # ----------------------------------------------------------------------------
  
  if("Irrigation_mm" %in% names(production_vegetale)) {
    cat("\n--- 5.3 ANCOVA: Effet variété contrôlé par irrigation ---\n")
    
    modele_ancova <- aov(Rendement_t_ha ~ Variete + Irrigation_mm, data = production_vegetale)
    print(summary(modele_ancova))
  }
  
  # ----------------------------------------------------------------------------
  # 5.4 RÉGRESSION LINÉAIRE MULTIPLE
  # ----------------------------------------------------------------------------
  
  cat("\n--- 5.4 RÉGRESSION LINÉAIRE MULTIPLE ---\n")
  
  # Variables explicatives disponibles
  vars_explicatives <- c()
  if("Dose_N_kg_ha" %in% names(production_vegetale)) vars_explicatives <- c(vars_explicatives, "Dose_N_kg_ha")
  if("Irrigation_mm" %in% names(production_vegetale)) vars_explicatives <- c(vars_explicatives, "Irrigation_mm")
  if("Temp_moy_C" %in% names(production_vegetale)) vars_explicatives <- c(vars_explicatives, "Temp_moy_C")
  if("Pluie_mm" %in% names(production_vegetale)) vars_explicatives <- c(vars_explicatives, "Pluie_mm")
  if("Densite_semis" %in% names(production_vegetale)) vars_explicatives <- c(vars_explicatives, "Densite_semis")
  
  if(length(vars_explicatives) > 0) {
    formule_reg <- as.formula(paste("Rendement_t_ha ~", paste(vars_explicatives, collapse = " + ")))
    modele_reg_multiple <- lm(formule_reg, data = production_vegetale)
    print(summary(modele_reg_multiple))
    
    # Test de multicolinéarité
    if(length(vars_explicatives) >= 2) {
      cat("\n--- Test de multicolinéarité (VIF) ---\n")
      print(vif(modele_reg_multiple))
    }
    
    # Graphiques de diagnostic
    par(mfrow = c(2, 2))
    plot(modele_reg_multiple)
    par(mfrow = c(1, 1))
  }
  
  # ----------------------------------------------------------------------------
  # 5.5 RÉGRESSION DOSE-RÉPONSE
  # ----------------------------------------------------------------------------
  
  if("Dose_N_kg_ha" %in% names(production_vegetale)) {
    cat("\n--- 5.5 RÉGRESSION DOSE-RÉPONSE ---\n")
    
    modele_N_lin <- lm(Rendement_t_ha ~ Dose_N_kg_ha, data = production_vegetale)
    production_vegetale$Dose_N_2 <- production_vegetale$Dose_N_kg_ha^2
    modele_N_quad <- lm(Rendement_t_ha ~ Dose_N_kg_ha + Dose_N_2, data = production_vegetale)
    
    cat("Modèle linéaire R² =", round(summary(modele_N_lin)$r.squared, 3), "\n")
    cat("Modèle quadratique R² =", round(summary(modele_N_quad)$r.squared, 3), "\n")
    cat("AIC linéaire:", AIC(modele_N_lin), "\n")
    cat("AIC quadratique:", AIC(modele_N_quad), "\n")
    
    # Graphique
    p2 <- ggplot(production_vegetale, aes(x = Dose_N_kg_ha, y = Rendement_t_ha)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue", aes(linetype = "Linéaire")) +
      geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "red", aes(linetype = "Quadratique")) +
      labs(title = "Relation dose-réponse: Azote vs Rendement",
           x = "Dose d'azote (kg/ha)", y = "Rendement (t/ha)") +
      theme_minimal()
    print(p2)
  }
  
  # ----------------------------------------------------------------------------
  # 5.6 GLM LOGISTIQUE - Présence de maladie
  # ----------------------------------------------------------------------------
  
  if("Maladie_present" %in% names(production_vegetale)) {
    cat("\n--- 5.6 GLM LOGISTIQUE: Facteurs de risque de maladie ---\n")
    
    # Sélection des prédicteurs
    preds <- c()
    if("Dose_N_kg_ha" %in% names(production_vegetale)) preds <- c(preds, "Dose_N_kg_ha")
    if("Pluie_mm" %in% names(production_vegetale)) preds <- c(preds, "Pluie_mm")
    if("Rendement_t_ha" %in% names(production_vegetale)) preds <- c(preds, "Rendement_t_ha")
    
    if(length(preds) > 0) {
      formule_logit <- as.formula(paste("Maladie_present ~", paste(preds, collapse = " + ")))
      modele_logit <- glm(formule_logit, family = binomial(link = "logit"), data = production_vegetale)
      print(summary(modele_logit))
      
      # Odds Ratios
      cat("\n--- Odds Ratios ---\n")
      print(exp(coef(modele_logit)))
      
      # Pseudo-R²
      pseudo_r2 <- 1 - (modele_logit$deviance / modele_logit$null.deviance)
      cat("\nPseudo-R² (McFadden):", pseudo_r2, "\n")
    }
  }
}

# ==============================================================================
# 6. PRODUCTION ANIMALE - ANALYSES
# ==============================================================================

afficher_section("PRODUCTION ANIMALE - ANALYSES")

# ------------------------------------------------------------------------------
# 6.1 MODÈLES MIXTES (LMM) - Production laitière
# ------------------------------------------------------------------------------

if(all(c("Lait_305j_kg", "Ferme") %in% names(production_animale))) {
  
  cat("\n--- 6.1 MODÈLE LINÉAIRE MIXTE: Production laitière ---\n")
  
  # Construction du modèle mixte
  formule_mixte <- "Lait_305j_kg ~ 1"
  
  if("Race" %in% names(production_animale)) {
    formule_mixte <- paste(formule_mixte, "+ Race")
  }
  if("Lactation_num" %in% names(production_animale)) {
    production_animale$Lactation_num <- as.numeric(production_animale$Lactation_num)
    formule_mixte <- paste(formule_mixte, "+ Lactation_num")
  }
  if("Regime_alim" %in% names(production_animale)) {
    formule_mixte <- paste(formule_mixte, "+ Regime_alim")
  }
  
  formule_mixte <- paste(formule_mixte, "+ (1 | Ferme)")
  
  modele_mixte_lait <- lmer(as.formula(formule_mixte), data = production_animale)
  print(summary(modele_mixte_lait))
  
  # ICC - Corrélation intra-classe
  icc_lait <- performance::icc(modele_mixte_lait)
  cat("\n--- Corrélation Intra-Classe (ICC) ---\n")
  cat("ICC:", round(icc_lait$ICC, 3), "\n")
  cat("Variance entre fermes:", round(icc_lait$ICC * 100, 1), "%\n")
  
  # Diagnostics
  par(mfrow = c(1, 2))
  plot(fitted(modele_mixte_lait), residuals(modele_mixte_lait, type = "pearson"),
       main = "Résidus vs Valeurs ajustées", 
       xlab = "Valeurs ajustées", ylab = "Résidus")
  abline(h = 0, col = "red", lty = 2)
  
  qqnorm(residuals(modele_mixte_lait, type = "pearson"), main = "QQ-plot des résidus")
  qqline(residuals(modele_mixte_lait, type = "pearson"), col = "red")
  par(mfrow = c(1, 1))
  
  # ----------------------------------------------------------------------------
  # 6.2 GLMM - Incidence de la mammite
  # ----------------------------------------------------------------------------
  
  if("Malade_mastite" %in% names(production_animale)) {
    cat("\n--- 6.2 GLMM: Facteurs de risque de mammite ---\n")
    
    formule_glmm <- "Malade_mastite ~ 1"
    if("Race" %in% names(production_animale)) formule_glmm <- paste(formule_glmm, "+ Race")
    if("Lactation_num" %in% names(production_animale)) formule_glmm <- paste(formule_glmm, "+ Lactation_num")
    if("GMQ_g_j" %in% names(production_animale)) formule_glmm <- paste(formule_glmm, "+ GMQ_g_j")
    formule_glmm <- paste(formule_glmm, "+ (1 | Ferme)")
    
    modele_glmm_mastite <- glmer(as.formula(formule_glmm),
                                 family = binomial(link = "logit"),
                                 data = production_animale,
                                 control = glmerControl(optimizer = "bobyqa"))
    print(summary(modele_glmm_mastite))
    
    # Odds Ratios
    cat("\n--- Odds Ratios (effets fixes) ---\n")
    print(exp(fixef(modele_glmm_mastite)))
  }
  
  # ----------------------------------------------------------------------------
  # 6.3 RÉGRESSION - Poids vif vs GMQ
  # ----------------------------------------------------------------------------
  
  if(all(c("GMQ_g_j", "Poids_vif_kg") %in% names(production_animale))) {
    cat("\n--- 6.3 RÉGRESSION: Poids vif vs GMQ ---\n")
    
    if("Race" %in% names(production_animale)) {
      modele_gmq <- lm(GMQ_g_j ~ Poids_vif_kg + Race, data = production_animale)
    } else {
      modele_gmq <- lm(GMQ_g_j ~ Poids_vif_kg, data = production_animale)
    }
    print(summary(modele_gmq))
    
    # Graphique
    p3 <- ggplot(production_animale, aes(x = Poids_vif_kg, y = GMQ_g_j, color = Race)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE) +
      labs(title = "Relation entre poids vif et GMQ",
           x = "Poids vif (kg)", y = "GMQ (g/j)") +
      theme_minimal()
    print(p3)
  }
}

# ==============================================================================
# 7. ENVIRONNEMENT - ANALYSES MULTIVARIÉES
# ==============================================================================

afficher_section("ENVIRONNEMENT - ANALYSES MULTIVARIÉES")

# ------------------------------------------------------------------------------
# 7.1 ACP - Analyse en Composantes Principales
# ------------------------------------------------------------------------------

# Variables pour l'ACP
vars_acp <- c("pH_eau", "MO_pct", "N_total_pct", "P_Olsen_mg_kg", "CEC_cmol_kg", "Argile_pct")
vars_acp <- vars_acp[vars_acp %in% names(environnement)]

if(length(vars_acp) >= 3) {
  
  cat("\n--- 7.1 ACP: Caractérisation de la qualité des sols ---\n")
  cat("Variables incluses:", paste(vars_acp, collapse=", "), "\n")
  
  # Sélection et nettoyage
  variables_acp <- environnement[, vars_acp, drop = FALSE]
  variables_acp <- na.omit(variables_acp)
  
  # ACP
  res_acp <- PCA(variables_acp, scale.unit = TRUE, graph = FALSE)
  
  cat("\nValeurs propres et variance expliquée:\n")
  print(res_acp$eig)
  
  # Graphique des valeurs propres
  fviz_eig(res_acp, addlabels = TRUE, main = "Variance expliquée par les composantes principales")
  
  # Cercle des corrélations
  fviz_pca_var(res_acp, col.var = "contrib",
               gradient.cols = c("blue", "green", "red"),
               repel = TRUE, title = "Cercle des corrélations")
  
  # Biplot avec groupement par zone
  if("Zone" %in% names(environnement)) {
    # Aligner les données
    idx_complets <- complete.cases(environnement[, vars_acp])
    zones_alignees <- environnement$Zone[idx_complets]
    
    fviz_pca_biplot(res_acp, habillage = zones_alignees,
                    addEllipses = TRUE, ellipse.level = 0.95,
                    title = "ACP - Projection par zone agro-écologique")
  }
  
  # ----------------------------------------------------------------------------
  # 7.2 CLUSTERING - Classification des parcelles
  # ----------------------------------------------------------------------------
  
  cat("\n--- 7.2 CLASSIFICATION HIÉRARCHIQUE (CAH) ---\n")
  
  res_hcpc <- HCPC(res_acp, nb.clust = 3, graph = FALSE)
  cat("Effectifs des clusters:\n")
  print(table(res_hcpc$data.clust$clust))
  
  fviz_cluster(res_hcpc, repel = TRUE, title = "Clusters de parcelles")
  
  # ----------------------------------------------------------------------------
  # 7.3 GLM POISSON - Richesse spécifique
  # ----------------------------------------------------------------------------
  
  if("Nb_especes_veg" %in% names(environnement)) {
    cat("\n--- 7.3 GLM POISSON: Richesse spécifique végétale ---\n")
    
    # Prédicteurs pour le modèle Poisson
    pred_poisson <- c()
    if("Usage_sol" %in% names(environnement)) pred_poisson <- c(pred_poisson, "Usage_sol")
    if("pH_eau" %in% names(environnement)) pred_poisson <- c(pred_poisson, "pH_eau")
    if("MO_pct" %in% names(environnement)) pred_poisson <- c(pred_poisson, "MO_pct")
    
    if(length(pred_poisson) > 0) {
      formule_poisson <- as.formula(paste("Nb_especes_veg ~", paste(pred_poisson, collapse = " + ")))
      modele_poisson <- glm(formule_poisson, family = poisson(link = "log"), data = environnement)
      print(summary(modele_poisson))
      
      # Vérification de la surdispersion
      dispersion_poisson <- modele_poisson$deviance / modele_poisson$df.residual
      cat("\nIndice de surdispersion:", dispersion_poisson, "\n")
      
      if(dispersion_poisson > 1.5) {
        cat("Surdispersion détectée → Utilisation de la Binomiale Négative\n")
        modele_nb <- glm.nb(formule_poisson, data = environnement)
        print(summary(modele_nb))
      }
    }
  }
  
  # ----------------------------------------------------------------------------
  # 7.4 RÉGRESSION - Pollution par nitrates
  # ----------------------------------------------------------------------------
  
  if("Pollution_nitrate_mgL" %in% names(environnement)) {
    cat("\n--- 7.4 RÉGRESSION: Facteurs influençant les nitrates ---\n")
    
    pred_nitrates <- c()
    if("pH_eau" %in% names(environnement)) pred_nitrates <- c(pred_nitrates, "pH_eau")
    if("MO_pct" %in% names(environnement)) pred_nitrates <- c(pred_nitrates, "MO_pct")
    if("Argile_pct" %in% names(environnement)) pred_nitrates <- c(pred_nitrates, "Argile_pct")
    if("Usage_sol" %in% names(environnement)) pred_nitrates <- c(pred_nitrates, "Usage_sol")
    
    if(length(pred_nitrates) >= 2) {
      formule_nitrates <- as.formula(paste("Pollution_nitrate_mgL ~", paste(pred_nitrates, collapse = " + ")))
      modele_nitrates <- lm(formule_nitrates, data = environnement)
      print(summary(modele_nitrates))
      
      # Graphiques de diagnostic
      par(mfrow = c(2, 2))
      plot(modele_nitrates)
      par(mfrow = c(1, 1))
    }
  }
}

# ==============================================================================
# 8. SÉRIES TEMPORELLES
# ==============================================================================

afficher_section("SÉRIES TEMPORELLES")

if(all(c("Annee", "Wilaya", "Rendement_bl_t_ha") %in% names(series_temporelles))) {
  
  # Convertir en numérique
  series_temporelles$Annee <- as.numeric(series_temporelles$Annee)
  series_temporelles$Rendement_bl_t_ha <- as.numeric(series_temporelles$Rendement_bl_t_ha)
  
  if("Pluviometrie_mm" %in% names(series_temporelles)) {
    series_temporelles$Pluviometrie_mm <- as.numeric(series_temporelles$Pluviometrie_mm)
  }
  
  # Liste des wilayas disponibles
  wilayas <- unique(series_temporelles$Wilaya)
  cat("Wilayas disponibles:", paste(wilayas, collapse=", "), "\n")
  
  # Analyser chaque wilaya
  for(wilaya_choisie in wilayas) {
    
    cat("\n--- Analyse pour la wilaya:", wilaya_choisie, "---\n")
    
    data_wilaya <- series_temporelles %>%
      filter(Wilaya == wilaya_choisie) %>%
      arrange(Annee)
    
    if(nrow(data_wilaya) >= 20) {
      
      # Création de la série temporelle
      rendement_ts <- ts(data_wilaya$Rendement_bl_t_ha, 
                         start = min(data_wilaya$Annee), 
                         frequency = 1)
      
      # Visualisation
      p4 <- autoplot(rendement_ts) +
        labs(title = paste("Évolution des rendements -", wilaya_choisie),
             x = "Année", y = "Rendement (t/ha)") +
        theme_minimal()
      print(p4)
      
      # Test de stationnarité
      adf_test <- adf.test(rendement_ts)
      cat("Test de stationnarité ADF: p-value =", adf_test$p.value, "\n")
      
      # Modèle ARIMA
      modele_arima <- auto.arima(rendement_ts, seasonal = FALSE)
      cat("\nModèle ARIMA sélectionné:\n")
      print(summary(modele_arima))
      
      # Prévisions
      previsions <- forecast(modele_arima, h = 5)
      p5 <- autoplot(previsions) +
        labs(title = paste("Prévisions des rendements -", wilaya_choisie),
             x = "Année", y = "Rendement (t/ha)") +
        theme_minimal()
      print(p5)
      
      cat("\nPrévisions sur 5 ans:\n")
      print(previsions$mean)
      
      # ARIMA avec covariable pluviométrie
      if("Pluviometrie_mm" %in% names(series_temporelles)) {
        pluie_ts <- ts(data_wilaya$Pluviometrie_mm, 
                       start = min(data_wilaya$Annee), 
                       frequency = 1)
        modele_arima_xreg <- auto.arima(rendement_ts, xreg = pluie_ts)
        cat("\nARIMA avec covariable pluviométrie - AIC:", AIC(modele_arima_xreg), "\n")
      }
    }
  }
}

# ==============================================================================
# 9. COMPARAISON ET SÉLECTION DE MODÈLES
# ==============================================================================

afficher_section("COMPARAISON ET SÉLECTION DE MODÈLES")

if("Rendement_t_ha" %in% names(production_vegetale)) {
  
  # Modèles à comparer
  modeles_liste <- list()
  formules_liste <- list()
  
  if("Dose_N_kg_ha" %in% names(production_vegetale)) {
    modeles_liste[["N seulement"]] <- lm(Rendement_t_ha ~ Dose_N_kg_ha, data = production_vegetale)
  }
  
  if(all(c("Dose_N_kg_ha", "Pluie_mm") %in% names(production_vegetale))) {
    modeles_liste[["N + Pluie"]] <- lm(Rendement_t_ha ~ Dose_N_kg_ha + Pluie_mm, data = production_vegetale)
  }
  
  if(all(c("Dose_N_kg_ha", "Pluie_mm", "Temp_moy_C") %in% names(production_vegetale))) {
    modeles_liste[["N + Pluie + Temp"]] <- lm(Rendement_t_ha ~ Dose_N_kg_ha + Pluie_mm + Temp_moy_C, 
                                              data = production_vegetale)
  }
  
  if(all(c("Dose_N_kg_ha", "Pluie_mm", "Temp_moy_C", "Variete") %in% names(production_vegetale))) {
    modeles_liste[["Modèle complet"]] <- lm(Rendement_t_ha ~ Dose_N_kg_ha + Pluie_mm + Temp_moy_C + Variete,
                                            data = production_vegetale)
  }
  
  if(length(modeles_liste) > 0) {
    # Tableau comparatif
    comparaison_modeles <- data.frame(
      Modele = names(modeles_liste),
      AIC = sapply(modeles_liste, AIC),
      BIC = sapply(modeles_liste, BIC),
      R2_ajuste = sapply(modeles_liste, function(m) summary(m)$adj.r.squared)
    )
    
    print(comparaison_modeles)
    
    # Meilleur modèle selon AIC
    meilleur_idx <- which.min(comparaison_modeles$AIC)
    cat("\n--- MEILLEUR MODÈLE SELON AIC ---\n")
    print(comparaison_modeles[meilleur_idx, ])
  }
}

# ==============================================================================
# 10. VISUALISATIONS SUPPLÉMENTAIRES
# ==============================================================================

afficher_section("VISUALISATIONS SUPPLÉMENTAIRES")

# Matrice de corrélation environnementale
vars_cor <- c("pH_eau", "MO_pct", "N_total_pct", "P_Olsen_mg_kg", 
              "CEC_cmol_kg", "Argile_pct", "Nb_especes_veg", "Pollution_nitrate_mgL")
vars_cor <- vars_cor[vars_cor %in% names(environnement)]

if(length(vars_cor) >= 3) {
  cor_matrix <- cor(environnement[, vars_cor], use = "complete.obs")
  corrplot(cor_matrix, method = "circle", type = "upper", 
           tl.col = "black", tl.srt = 45,
           title = "Matrice de corrélation - Environnement")
}

# Distribution des rendements par année
if(all(c("Rendement_t_ha", "Annee", "Variete") %in% names(production_vegetale))) {
  p6 <- ggplot(production_vegetale, aes(x = factor(Annee), y = Rendement_t_ha, fill = Variete)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Distribution des rendements par année et variété",
         x = "Année", y = "Rendement (t/ha)") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p6)
}

# Relation pluie-rendement par site
if(all(c("Pluie_mm", "Rendement_t_ha", "Site") %in% names(production_vegetale))) {
  p7 <- ggplot(production_vegetale, aes(x = Pluie_mm, y = Rendement_t_ha, color = Site)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(title = "Relation pluviométrie-rendement par site",
         x = "Pluviométrie (mm)", y = "Rendement (t/ha)") +
    facet_wrap(~Site) +
    theme_minimal()
  print(p7)
}

# ==============================================================================
# 11. RAPPORT FINAL
# ==============================================================================

afficher_section("RAPPORT FINAL")

cat("\n📊 RÉSUMÉ DES ANALYSES EFFECTUÉES\n")
cat("─────────────────────────────────────────────────────────────────────────\n")

cat("\n🌾 PRODUCTION VÉGÉTALE\n")
cat("   • Nombre d'observations:", nrow(production_vegetale), "\n")
if(exists("modele_anova_variete")) {
  f_val <- summary(modele_anova_variete)[[1]]$`F value`[1]
  p_val <- summary(modele_anova_variete)[[1]]$`Pr(>F)`[1]
  cat("   • ANOVA variété: F =", round(f_val, 2), ", p =", format(p_val, scientific = TRUE, digits = 3), "\n")
}
if(exists("modele_reg_multiple")) {
  cat("   • R² régression multiple:", round(summary(modele_reg_multiple)$r.squared, 3), "\n")
}

cat("\n🐄 PRODUCTION ANIMALE\n")
cat("   • Nombre d'observations:", nrow(production_animale), "\n")
if(exists("modele_mixte_lait")) {
  cat("   • Modèle mixte - production laitière réalisé\n")
}
if(exists("icc_lait")) {
  cat("   • ICC:", round(icc_lait$ICC, 3), "\n")
}

cat("\n🌍 ENVIRONNEMENT\n")
cat("   • Nombre de parcelles:", nrow(environnement), "\n")
if(exists("res_acp")) {
  var_exp <- round(sum(res_acp$eig[1:2, "percentage of variance"]), 1)
  cat("   • Variance expliquée ACP (2 premières CP):", var_exp, "%\n")
}

cat("\n📈 SÉRIES TEMPORELLES\n")
cat("   • Wilayas analysées:", paste(unique(series_temporelles$Wilaya), collapse=", "), "\n")
if(exists("modele_arima")) {
  cat("   • Modèle ARIMA appliqué\n")
}

cat("\n✅ VARIABLES ANALYSÉES\n")
cat("   • Production végétale:", paste(names(production_vegetale), collapse=", "), "\n")
cat("   • Production animale:", paste(names(production_animale), collapse=", "), "\n")
cat("   • Environnement:", paste(names(environnement), collapse=", "), "\n")

cat("\n", rep("=", 80), "\n", sep="")
cat("                     FIN DU SCRIPT - ANALYSES COMPLÈTES\n")
cat(rep("=", 80), "\n", sep="")

