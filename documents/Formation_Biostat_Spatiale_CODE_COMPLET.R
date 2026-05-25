# =============================================================================
#   FORMATION : Modèles Linéaires & Logistiques en Santé / Agriculture
#               Corrélations Spatiales : NDVI, NDWI, SAVI, LST
#               Logiciel : RStudio
# =============================================================================
#   Auteur  : [Votre Nom]
#   Date    : 2025
#   Niveau  : Débutants en R
# =============================================================================

# ─────────────────────────────────────────────────────────────────────────────
# 0. INSTALLATION & CHARGEMENT DES PACKAGES
# ─────────────────────────────────────────────────────────────────────────────
# (à exécuter une seule fois si pas encore installés)
# install.packages(c("tidyverse", "terra", "sf", "ggplot2",
#                    "corrplot", "car", "lmtest", "pROC", "broom"))

library(tidyverse)   # Manipulation de données
library(ggplot2)     # Visualisation
library(terra)       # Données raster (images satellitaires)
library(corrplot)    # Matrice de corrélation graphique
library(car)         # VIF, tests diagnostics
library(pROC)        # Courbe ROC
library(broom)       # Mise en forme des résultats

cat("✅ Packages chargés avec succès !\n")


# =============================================================================
# MODULE 1 — BASES DE R (Rappels rapides)
# =============================================================================

# --- Objets de base ---
x <- 5                         # Numérique
nom <- "Paludisme"             # Caractère
est_malade <- TRUE             # Logique

# --- Vecteurs ---
ndvi_values <- c(0.2, 0.45, 0.67, 0.12, 0.80)
mean(ndvi_values)
summary(ndvi_values)

# --- Data frame ---
df_demo <- data.frame(
  village   = c("A", "B", "C", "D", "E"),
  ndvi      = ndvi_values,
  cas_palu  = c(3, 12, 25, 1, 40)
)
print(df_demo)
str(df_demo)

# --- Visualisation rapide ---
plot(df_demo$ndvi, df_demo$cas_palu,
     xlab = "NDVI", ylab = "Cas de paludisme",
     main = "NDVI vs Paludisme (données fictives)",
     pch = 16, col = "forestgreen")


# =============================================================================
# MODULE 2 — RÉGRESSION LINÉAIRE
#            Exemple : rendement agricole (maïs) ~ NDVI + Pluviométrie
# =============================================================================

cat("\n📊 MODULE 2 : Régression Linéaire\n")

# ─── 2.1  Simulation de données agricoles ────────────────────────────────────
set.seed(42)
n <- 150

ndvi_agri   <- runif(n, 0.15, 0.85)       # Indice de végétation
pluie       <- runif(n, 250, 900)          # Précipitations (mm/an)
fertilisant <- runif(n, 0, 100)            # Kg d'engrais/ha

# Relation réelle (on la connaît car on simule) :
# rendement = 1.5 + 3*ndvi + 0.004*pluie + 0.01*fertilisant + bruit
rendement <- 1.5 + 3 * ndvi_agri + 0.004 * pluie +
             0.01 * fertilisant + rnorm(n, 0, 0.6)

df_agri <- data.frame(rendement, ndvi = ndvi_agri, pluie, fertilisant)
head(df_agri)
summary(df_agri)

# ─── 2.2  Visualisation exploratoire ─────────────────────────────────────────
ggplot(df_agri, aes(x = ndvi, y = rendement)) +
  geom_point(alpha = 0.6, color = "#2C5F2D", size = 2) +
  geom_smooth(method = "lm", color = "#5BA85A", se = TRUE) +
  labs(
    title = "Rendement maïs en fonction du NDVI",
    subtitle = "Régression linéaire simple",
    x = "NDVI (Indice de végétation)",
    y = "Rendement (t/ha)"
  ) +
  theme_minimal()

# ─── 2.3  Modèle de régression linéaire multiple ─────────────────────────────
modele_lm <- lm(rendement ~ ndvi + pluie + fertilisant, data = df_agri)

# Résumé complet
summary(modele_lm)

# Interprétation :
# - (Intercept) : rendement si toutes variables = 0
# - ndvi        : +1 unité de NDVI → +β₁ t/ha de rendement
# - R²          : % de variance de rendement expliquée par le modèle
# - p-value     : < 0.05 → variable statistiquement significative

# ─── 2.4  Coefficients & Intervalles de Confiance ───────────────────────────
coef(modele_lm)                            # Coefficients
confint(modele_lm, level = 0.95)           # IC à 95%

# Mise en forme avec broom
tidy(modele_lm)                            # Tableau propre
glance(modele_lm)                          # Métriques globales (R², AIC…)

# ─── 2.5  Diagnostics du modèle ─────────────────────────────────────────────
par(mfrow = c(2, 2))                       # Afficher 4 graphiques
plot(modele_lm)                            # Résidus, QQ-plot, levier…
par(mfrow = c(1, 1))                       # Remettre en normal

# Vérification de la multi-colinéarité (VIF)
vif(modele_lm)
# VIF < 5 : acceptable | VIF > 10 : problème de colinéarité

# Test d'hétéroscédasticité (Breusch-Pagan)
library(lmtest)
bptest(modele_lm)
# p > 0.05 → homoscédasticité OK

# ─── 2.6  Prédiction ─────────────────────────────────────────────────────────
nouveaux_villages <- data.frame(
  ndvi        = c(0.3, 0.6, 0.75),
  pluie       = c(400, 600, 700),
  fertilisant = c(30, 50, 80)
)

predict(modele_lm, newdata = nouveaux_villages, interval = "confidence")


# =============================================================================
# MODULE 3 — RÉGRESSION LOGISTIQUE
#            Exemple : présence de paludisme ~ NDVI + NDWI + LST
# =============================================================================

cat("\n🦟 MODULE 3 : Régression Logistique\n")

# ─── 3.1  Simulation de données épidémiologiques ─────────────────────────────
set.seed(123)
n <- 300

ndvi_sante <- runif(n, 0.05, 0.90)        # Végétation (risque vectoriel)
ndwi_sante <- runif(n, -0.30, 0.55)       # Eau stagnante
lst_sante  <- runif(n, 22, 40)            # Température de surface (°C)

# Logit réel :
logit_p <- -4 + 5 * ndvi_sante + 4 * ndwi_sante - 0.05 * lst_sante

# Conversion en probabilité via fonction logistique
prob_palu   <- plogis(logit_p)
paludisme   <- rbinom(n, 1, prob_palu)    # 0 = sain, 1 = malade

df_sante <- data.frame(paludisme, ndvi = ndvi_sante,
                        ndwi = ndwi_sante, lst = lst_sante)

# Fréquence des cas
table(df_sante$paludisme)
prop.table(table(df_sante$paludisme)) * 100    # % de cas

# ─── 3.2  Modèle logistique ──────────────────────────────────────────────────
mod_log <- glm(paludisme ~ ndvi + ndwi + lst,
               family = binomial(link = "logit"),
               data = df_sante)

summary(mod_log)

# ─── 3.3  Odds Ratios & IC ───────────────────────────────────────────────────
OR <- exp(coef(mod_log))
IC <- exp(confint(mod_log))

cat("\n--- Odds Ratios ---\n")
print(round(cbind(OR = OR, IC), 3))

# Interprétation :
# OR > 1 → facteur de RISQUE (augmente les chances d'être malade)
# OR < 1 → facteur PROTECTEUR
# OR = 1 → pas d'effet

# ─── 3.4  Qualité du modèle ──────────────────────────────────────────────────
# 3.4a — AUC / Courbe ROC
roc_obj <- roc(df_sante$paludisme, fitted(mod_log))
auc_val <- auc(roc_obj)
cat(sprintf("\nAUC = %.3f\n", as.numeric(auc_val)))
# AUC > 0.70 = acceptable | > 0.80 = bon | > 0.90 = excellent

plot(roc_obj,
     col = "#1E6B9A", lwd = 2,
     main = paste0("Courbe ROC — Paludisme (AUC = ", round(auc_val, 3), ")"))

# 3.4b — Pseudo-R² de McFadden
ll_null <- logLik(glm(paludisme ~ 1, family = binomial, data = df_sante))
ll_mod  <- logLik(mod_log)
mcfadden <- 1 - (as.numeric(ll_mod) / as.numeric(ll_null))
cat(sprintf("McFadden R² = %.3f\n", mcfadden))

# ─── 3.5  Prédiction de probabilités ─────────────────────────────────────────
zones_test <- data.frame(
  ndvi = c(0.20, 0.55, 0.80),
  ndwi = c(-0.1, 0.20, 0.45),
  lst  = c(35,   30,   27  )
)
zones_test$prob_palu <- predict(mod_log, newdata = zones_test, type = "response")
print(zones_test)

# ─── 3.6  Visualisation probabilité prédite ──────────────────────────────────
df_sante$prob_pred <- fitted(mod_log)

ggplot(df_sante, aes(x = ndvi, y = prob_pred, color = factor(paludisme))) +
  geom_point(alpha = 0.5, size = 1.8) +
  scale_color_manual(values = c("0" = "#2C5F2D", "1" = "#C0392B"),
                     labels = c("Sain", "Malade"),
                     name = "Statut") +
  labs(
    title = "Probabilité prédite de paludisme selon NDVI",
    x = "NDVI", y = "Probabilité prédite"
  ) +
  theme_minimal()


# =============================================================================
# MODULE 4 — INDICES SPECTRAUX AVEC {terra}
#            NDVI · NDWI · SAVI · LST
# =============================================================================

cat("\n🛰 MODULE 4 : Indices Spectraux\n")

# ─── 4.1  Création d'une image satellite simulée ─────────────────────────────
set.seed(7)
img <- rast(nrows = 80, ncols = 80, nlyrs = 5)
ext(img) <- c(0, 1, 0, 1)                 # Extent (coordonnées fictives)

# Simulation de valeurs réflectance (0-1) par bande
values(img) <- matrix(
  c(runif(80*80, 0.02, 0.12),   # Bande Bleue (B)
    runif(80*80, 0.05, 0.20),   # Bande Verte (G)
    runif(80*80, 0.04, 0.18),   # Bande Rouge (R)
    runif(80*80, 0.15, 0.65),   # Proche Infrarouge (NIR)
    runif(80*80, 0.05, 0.30)),  # Infrarouge Moyen (SWIR)
  ncol = 5
)
names(img) <- c("Blue", "Green", "Red", "NIR", "SWIR")

# ─── 4.2  Calcul des indices ──────────────────────────────────────────────────

# NDVI : Normalized Difference Vegetation Index
ndvi_r <- (img$NIR - img$Red) / (img$NIR + img$Red)
names(ndvi_r) <- "NDVI"

# NDWI : Normalized Difference Water Index
ndwi_r <- (img$Green - img$NIR) / (img$Green + img$NIR)
names(ndwi_r) <- "NDWI"

# SAVI : Soil Adjusted Vegetation Index (L = 0.5 par défaut)
L <- 0.5
savi_r <- ((img$NIR - img$Red) / (img$NIR + img$Red + L)) * (1 + L)
names(savi_r) <- "SAVI"

# LST simulée (température de surface, en °C)
# En réalité : calculée depuis la bande thermique + émissivité
lst_r <- 25 + 10 * (1 - ndvi_r) + rnorm(ncell(ndvi_r), 0, 1)
names(lst_r) <- "LST"

cat("Résumé NDVI :\n"); print(global(ndvi_r, "mean"))
cat("Résumé NDWI :\n"); print(global(ndwi_r, "mean"))
cat("Résumé SAVI :\n"); print(global(savi_r, "mean"))
cat("Résumé LST  :\n"); print(global(lst_r, "mean"))

# ─── 4.3  Visualisation ───────────────────────────────────────────────────────
par(mfrow = c(2, 2))

plot(ndvi_r, main = "NDVI", col = colorRampPalette(c("brown","yellow","darkgreen"))(20))
plot(ndwi_r, main = "NDWI", col = colorRampPalette(c("white","skyblue","navy"))(20))
plot(savi_r, main = "SAVI", col = colorRampPalette(c("beige","orange","saddlebrown"))(20))
plot(lst_r,  main = "LST (°C)", col = colorRampPalette(c("blue","yellow","red"))(20))

par(mfrow = c(1, 1))

# ─── 4.4  Classification simple : niveau de risque vectoriel ─────────────────
risque_ndvi <- ifel(ndvi_r > 0.5, 3,           # NDVI élevé = risque élevé
                ifel(ndvi_r > 0.3, 2, 1))       # Moyen / Faible
risque_ndwi <- ifel(ndwi_r > 0.2, 3,
                ifel(ndwi_r > 0, 2, 1))

# Index composite (moyenne)
risque_composite <- (risque_ndvi + risque_ndwi) / 2

plot(risque_composite,
     main = "Index de Risque Vectoriel Composite (NDVI + NDWI)",
     col  = c("#2C5F2D", "#F39C12", "#C0392B"))


# =============================================================================
# MODULE 5 — CORRÉLATIONS SPATIALES
#            Maladies vectorielles ~ NDVI + NDWI + SAVI + LST
# =============================================================================

cat("\n🌍 MODULE 5 : Corrélations Spatiales\n")

# ─── 5.1  Extraction des valeurs raster par point (villages) ─────────────────
set.seed(55)
n_villages <- 120

# Points aléatoires sur notre image
coords <- data.frame(
  x = runif(n_villages, 0, 1),
  y = runif(n_villages, 0, 1)
)
pts <- vect(coords, geom = c("x", "y"))   # Objet terra SpatVector

# Extraction des valeurs d'indices à chaque village
val_ndvi <- extract(ndvi_r, pts)[, 2]
val_ndwi <- extract(ndwi_r, pts)[, 2]
val_savi <- extract(savi_r, pts)[, 2]
val_lst  <- extract(lst_r,  pts)[, 2]

# Simulation des cas de maladies vectorielles (paludisme)
cas_palu <- rpois(n_villages,
                  lambda = exp(0.5 + 2*val_ndvi + 1.5*val_ndwi - 0.03*val_lst))

df_spatial <- data.frame(
  cas_palu, ndvi = val_ndvi, ndwi = val_ndwi,
  savi = val_savi, lst = val_lst
)
df_spatial <- na.omit(df_spatial)           # Retirer les NA éventuels

cat(sprintf("Dataset spatial : %d villages\n", nrow(df_spatial)))
summary(df_spatial)

# ─── 5.2  Matrice de corrélation ─────────────────────────────────────────────
cor_mat <- cor(df_spatial, use = "complete.obs")
cat("\nMatrice de corrélation :\n")
print(round(cor_mat, 3))

corrplot(cor_mat,
         method  = "color",          # Couleur pour l'intensité
         type    = "upper",          # Moitié supérieure
         addCoef.col = "black",      # Afficher les coefficients
         tl.col  = "black",          # Labels en noir
         tl.srt  = 45,               # Rotation des labels
         title   = "Corrélations : Indices Spectraux ~ Cas de Paludisme",
         mar     = c(0,0,2,0))

# ─── 5.3  Modèle intégrant tous les indices ───────────────────────────────────
# Poisson (variable = comptage de cas)
mod_poisson <- glm(cas_palu ~ ndvi + ndwi + savi + lst,
                   family = poisson(link = "log"),
                   data = df_spatial)
summary(mod_poisson)

# Ratio d'incidence (Incidence Rate Ratio)
IRR <- exp(coef(mod_poisson))
cat("\nIncidence Rate Ratios :\n")
print(round(IRR, 3))

# ─── 5.4  Test de multi-colinéarité ──────────────────────────────────────────
# Sur un LM (VIF ne fonctionne pas directement sur GLM de base)
mod_lm_vif <- lm(cas_palu ~ ndvi + ndwi + savi + lst, data = df_spatial)
vif_vals <- vif(mod_lm_vif)
cat("\nValeurs VIF :\n")
print(round(vif_vals, 2))
# Règle : VIF < 5 = OK | 5-10 = vigilance | > 10 = colinéarité problématique

# ─── 5.5  Visualisation corrélations pairwise ─────────────────────────────────
pairs(df_spatial,
      pch    = 16, cex = 0.5,
      col    = ifelse(df_spatial$cas_palu > median(df_spatial$cas_palu),
                      "#C0392B", "#2C5F2D"),
      main   = "Scatter plot matrix — Indices & Cas de paludisme")

# ─── 5.6  Carte des cas superposée à NDVI ────────────────────────────────────
coords_df <- data.frame(coords[1:nrow(df_spatial),],
                        cas = df_spatial$cas_palu,
                        ndvi_val = df_spatial$ndvi)

ggplot(coords_df, aes(x = x, y = y, size = cas, color = ndvi_val)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "yellow", high = "darkgreen", name = "NDVI") +
  scale_size_continuous(name = "Cas paludisme", range = c(1, 8)) +
  labs(
    title = "Distribution spatiale des cas de paludisme",
    subtitle = "Taille = nombre de cas | Couleur = NDVI",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()


# =============================================================================
# MODULE 6 — EXERCICES PRATIQUES
# =============================================================================

cat("\n\n🎯 EXERCICES — Essayez vous-même !\n")
cat(strrep("─", 60), "\n")

cat("
EXERCICE 1 — Régression Linéaire Agricole
  Données : df_agri (rendement, ndvi, pluie, fertilisant)
  Tâche   : Comparer modele simple (ndvi seulement) vs modele complet
  Bonus   : Calculer l'ANOVA entre les deux modèles (anova())

EXERCICE 2 — Régression Logistique
  Données : df_sante (paludisme, ndvi, ndwi, lst)
  Tâche   : Tester avec seuil de classification = 0.4 et 0.6
  Bonus   : Calculer la matrice de confusion (table())

EXERCICE 3 — Indices Spectraux
  Tâche   : Calculer l'EVI = 2.5 * (NIR-R) / (NIR + 6*R - 7.5*B + 1)
  Bonus   : Comparer EVI vs NDVI avec plot(ndvi_r, evi_r)

EXERCICE 4 — Corrélations
  Données : df_spatial
  Tâche   : Ajouter une variable 'saison' (sèche/pluies) et tester
             l'interaction ndvi:saison dans le modèle
")

# =============================================================================
# FIN DU SCRIPT
# =============================================================================
cat("\n✅ Script terminé — Bonne formation !\n")
