# PLAN DE COURS DÉTAILLÉ — 2 HEURES
## Modèles Linéaires, Logistiques & Corrélations Spatiales en Santé / Agriculture
### Logiciel : RStudio | Niveau : Débutants

---

## ⏱ MINUTAGE GLOBAL

| # | Module | Durée |
|---|--------|-------|
| 0 | Accueil & objectifs | 5 min |
| 1 | Bases de R & RStudio | 10 min |
| 2 | Régression Linéaire | 25 min |
| 3 | Régression Logistique | 25 min |
| 4 | Indices Spectraux (NDVI, NDWI, SAVI, LST) | 25 min |
| 5 | Corrélations Spatiales & Maladies Vectorielles | 20 min |
| 6 | Exercices & Synthèse | 10 min |
| **Total** | | **120 min** |

---

## 🎯 MODULE 0 — ACCUEIL (5 min)

**Objectifs pédagogiques :**
- Comprendre la régression linéaire et logistique et les interpréter
- Calculer les indices NDVI, NDWI, SAVI, LST sous R
- Relier ces indices aux données épidémiologiques (maladies vectorielles)

**Question brise-glace :** *"Avez-vous déjà entendu parler du NDVI ? Qu'est-ce qu'on peut observer depuis un satellite ?"*

---

## 🖥 MODULE 1 — BASES DE R (10 min)

### 1.1 Interface RStudio (3 min)
- **Script** : zone de rédaction du code (Ctrl+Enter pour exécuter)
- **Console** : résultats immédiats
- **Environnement** : vos objets en mémoire
- **Plots / Packages** : graphiques et gestion des bibliothèques

### 1.2 Objets fondamentaux (4 min)
```r
x <- 5              # assignation
c(1, 2, 3)          # vecteur
data.frame(...)     # tableau de données
```

### 1.3 Installation des packages (3 min)
```r
install.packages(c("tidyverse", "terra", "sf", "corrplot", "car", "pROC"))
library(tidyverse)
```

**💡 Conseil pédagogique :** Faire exécuter ces lignes en direct avec les apprenants.

---

## 📊 MODULE 2 — RÉGRESSION LINÉAIRE (25 min)

### 2.1 Théorie (8 min)

**Formule :** Y = β₀ + β₁X₁ + β₂X₂ + ε

**Quand l'utiliser ?**
- Variable à expliquer (Y) **continue** : rendement (t/ha), taux de malnutrition, indice de végétation
- Variables explicatives (X) : continues ou catégorielles

**Exemples concrets :**
- **Agriculture :** Rendement du maïs ~ NDVI + Pluviométrie + Fertilisant
- **Santé :** Taux de malnutrition ~ accès à l'eau + densité végétale + LST

**Interpréter la sortie `summary()` :**

| Sortie R | Signification |
|----------|--------------|
| `Estimate` (β) | Effet d'une unité de X sur Y |
| `Pr(>|t|)` | p-valeur (< 0.05 = significatif) |
| `R-squared` | % de variance expliquée |
| `F-statistic` | Le modèle global est-il significatif ? |

### 2.2 Démonstration R (12 min)
> Exécuter les sections 2.1 à 2.6 du script R

**Points clés à souligner :**
1. `lm(Y ~ X1 + X2, data = df)` → formule de base
2. `summary(modele)` → tout y est !
3. `plot(modele)` → 4 graphiques diagnostics
4. `vif(modele)` → détecter la multi-colinéarité

### 2.3 Vérification des hypothèses (5 min)
- **Linéarité** : graphique résidus vs valeurs ajustées
- **Normalité des résidus** : QQ-plot
- **Homoscédasticité** : test Breusch-Pagan
- **Indépendance** : Durbin-Watson (non vu ici)

---

## 🦟 MODULE 3 — RÉGRESSION LOGISTIQUE (25 min)

### 3.1 Théorie (8 min)

**Quand l'utiliser ?**
- Variable à expliquer (Y) **binaire** : malade/sain, présence/absence d'un vecteur, 0/1

**Formule :**
```
P(Y=1) = 1 / (1 + e^−(β₀ + β₁X₁ + ...))
```

**Concepts clés :**

| Concept | Définition | Exemple |
|---------|-----------|---------|
| **Logit** | log(p / 1−p) | Transformation qui linéarise |
| **Odds Ratio** | exp(β) | OR = 2 → le risque double |
| **AUC** | Aire sous la courbe ROC | 0.85 = bon discriminant |
| **Pseudo R²** | McFadden R² | Analogie du R² linéaire |

**Interprétation des Odds Ratios :**
- OR > 1 → Facteur de **risque**
- OR < 1 → Facteur **protecteur**
- OR = 1 → Pas d'effet

**Lien avec santé :** NDVI élevé → plus de végétation → plus de gîtes larvaires → OR > 1 pour le paludisme

### 3.2 Démonstration R (12 min)
> Exécuter les sections 3.1 à 3.6 du script R

```r
mod_log <- glm(paludisme ~ ndvi + ndwi + lst,
               family = binomial(link = "logit"),
               data = df_sante)
exp(coef(mod_log))   # Odds Ratios
```

### 3.3 Évaluation du modèle (5 min)
- **Courbe ROC** : `roc()` → `plot(roc_obj)`
- **AUC** : > 0.70 acceptable, > 0.80 bon, > 0.90 excellent
- **Matrice de confusion** : sensibilité, spécificité

---

## 🛰 MODULE 4 — INDICES SPECTRAUX (25 min)

### 4.1 Qu'est-ce qu'un indice spectral ? (5 min)

Les satellites (Landsat, Sentinel, MODIS) mesurent la **réflectance** de la surface terrestre dans différentes **bandes spectrales**.

```
Bandes Landsat 8 :
  B2 = Bleu   | B3 = Vert  | B4 = Rouge
  B5 = PIR    | B6 = SWIR  | B10 = Thermique
```

### 4.2 Les 4 indices clés (10 min)

#### NDVI — Normalized Difference Vegetation Index
```
NDVI = (PIR − Rouge) / (PIR + Rouge)
```
- **Plage :** -1 à +1
- **< 0** : eau, neige | **0-0.2** : sol nu | **0.2-0.5** : végétation clairsemée | **> 0.5** : forêt dense
- **Lien santé :** zones de NDVI élevé = abris et gîtes larvaires pour les moustiques

#### NDWI — Normalized Difference Water Index
```
NDWI = (Vert − PIR) / (Vert + PIR)
```
- **Plage :** -1 à +1
- **> 0.3** : eau libre | **0 à 0.3** : sol humide | **< 0** : végétation/sol sec
- **Lien santé :** eau stagnante (NDWI > 0.2) = reproduction des moustiques

#### SAVI — Soil Adjusted Vegetation Index
```
SAVI = [(PIR − Rouge) / (PIR + Rouge + L)] × (1 + L)   avec L = 0.5
```
- Correction de l'effet du sol nu (zones arides, Sahel)
- Utile en agriculture semi-aride

#### LST — Land Surface Temperature
```
LST calculée à partir de la bande thermique + émissivité de surface
```
- Exprimée en °C
- **Lien santé :** accélère ou ralentit le cycle de développement des vecteurs

### 4.3 Démonstration R avec {terra} (10 min)
> Exécuter les sections 4.1 à 4.4 du script R

```r
library(terra)
img <- rast("landsat_scene.tif")
ndvi <- (img$NIR - img$Red) / (img$NIR + img$Red)
plot(ndvi, main = "NDVI", col = terrain.colors(20))
```

---

## 🌍 MODULE 5 — CORRÉLATIONS SPATIALES & MALADIES VECTORIELLES (20 min)

### 5.1 Chaîne épidémiologique spatiale (5 min)

```
SATELLITE (indices) → ENVIRONNEMENT → VECTEUR → MALADIE
NDVI, NDWI, SAVI, LST → végétation/eau/chaleur → moustique/tique → paludisme/dengue/bilharziose
```

**Maladies vectorielles clés et leurs indices prédicteurs :**

| Maladie | Vecteur | Indices pertinents |
|---------|---------|-------------------|
| Paludisme | *Anopheles* | NDVI, NDWI (eau stagnante) |
| Dengue | *Aedes* | LST, NDWI, densité urbaine |
| Bilharziose | Escargots | NDWI (eau douce), LST |
| Leishmaniose | Phlébotome | NDVI, LST, SAVI |

### 5.2 Corrélations attendues (5 min)

| Indice ↑ | Effet sur les vecteurs |
|----------|----------------------|
| NDVI ↑ | Abris + humidité → risque ↑ |
| NDWI ↑ | Eau stagnante → reproduction ↑ |
| SAVI ↑ | Végétation aride → risque modéré |
| LST ↑ jusqu'à ~35°C | Cycle vecteur accéléré → risque ↑ |
| LST > 40°C | Inhibition des vecteurs → risque ↓ |

### 5.3 Démonstration R (10 min)
> Exécuter les sections 5.1 à 5.6 du script R

```r
# Extraction des valeurs d'indices par point GPS
val_ndvi <- extract(ndvi_r, pts_villages)

# Matrice de corrélation
corrplot(cor(df_spatial), method = "color", addCoef.col = "black")

# Modèle de Poisson (cas = comptage)
mod_poisson <- glm(cas ~ ndvi + ndwi + lst,
                   family = poisson, data = df_spatial)
```

---

## 🎯 MODULE 6 — EXERCICES & SYNTHÈSE (10 min)

### Exercices proposés (5 min de travail autonome)

**Exercice 1 — Comparaison de modèles linéaires**
```r
# Comparer modèle simple vs complet
mod1 <- lm(rendement ~ ndvi, data = df_agri)
mod2 <- lm(rendement ~ ndvi + pluie + fertilisant, data = df_agri)
anova(mod1, mod2)  # Test F de comparaison
```

**Exercice 2 — Seuil de décision logistique**
```r
prob_pred <- fitted(mod_log)
pred_classe <- ifelse(prob_pred > 0.5, 1, 0)
table(Réel = df_sante$paludisme, Prédit = pred_classe)
```

**Exercice 3 — Indice EVI**
```r
# Enhanced Vegetation Index
evi <- 2.5 * (img$NIR - img$Red) / (img$NIR + 6*img$Red - 7.5*img$Blue + 1)
```

### Synthèse & Messages clés (5 min)

1. **Régression linéaire** → Y continu → `lm()` → interpréter β et R²
2. **Régression logistique** → Y binaire → `glm(family=binomial)` → interpréter OR et AUC
3. **NDVI/NDWI/SAVI/LST** → mesures satellite → `terra` → fenêtres temporelles et spatiales de risque
4. **Corrélations spatiales** → lier environnement et épidémiologie → cartographie du risque
5. **Multi-colinéarité** → toujours vérifier VIF avant d'interpréter les β

---

## 📚 RESSOURCES COMPLÉMENTAIRES

- **Livres :** *R for Data Science* (Wickham & Grolemund) — gratuit en ligne
- **Packages clés :** `terra`, `sf`, `tidyverse`, `pROC`, `corrplot`, `car`
- **Données satellites gratuites :** USGS Earth Explorer (Landsat), Copernicus (Sentinel)
- **Données santé :** WHO Malaria Atlas Project, GBIF (présence de vecteurs)
- **Tutos vidéo :** ESRI Learn, Geocomputation with R (gratuit en ligne)

---

*Formation réalisée avec RStudio · Package terra pour les données spatiales · ggplot2 pour les visualisations*
