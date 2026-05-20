# 1. Chargement des bibliothèques et importation des données
# Définit le répertoire de travail → dossier dans lequel R va chercher les fichiers et enregistrer les résultats
setwd("E:/GADA Solution/site clone/gada-training/documents")

# Charge la bibliothèque permettant de lire les fichiers Excel
library(readxl)

# Charge un package dédié à la manipulation des données (filtrage, sélection, transformation)
library(dplyr)

# Charge un package utilisé pour la visualisation graphique professionnelle des données
library(ggplot2)

# Importe le fichier Excel dans R et le stocke dans un objet nommé data
data <- read_excel("Donnees_sorgho_Benoit_kajora_complet.xlsx")

# Affiche la structure du jeu de données : types de variables, nombre de lignes/colonnes, aperçu des variables
str(data)

# Fournit un résumé statistique rapide : min, max, moyenne, médiane (variables numériques) et distribution (variables qualitatives)
summary(data)

# 2. Nettoyage des données
# 2.1 Identification des valeurs manquantes
# Permet de compter le nombre de valeurs manquantes (NA) dans chaque colonne
colSums(is.na(data))

# Supprime toutes les lignes contenant au moins une valeur manquante → obtention d'un jeu de données propre
data_clean <- na.omit(data)

# 2.2 Détection des valeurs aberrantes (exemple avec Hauteur)
# Calcule le premier quartile (25%)
Q1 <- quantile(data_clean$Hauteur, 0.25)

# Calcule le troisième quartile (75%)
Q3 <- quantile(data_clean$Hauteur, 0.75)

# Calcule l'intervalle interquartile (IQR), mesure de dispersion robuste
IQR_val <- Q3 - Q1

# Filtrage des valeurs aberrantes avec la règle : [Q1 - 1.5 × IQR ; Q3 + 1.5 × IQR]
data_clean <- data_clean %>%
  filter(Hauteur >= (Q1 - 1.5 * IQR_val) &
           Hauteur <= (Q3 + 1.5 * IQR_val))

# 2.3 Suppression des doublons → évite la surreprésentation de certaines observations et les biais dans les analyses statistiques
data_clean <- distinct(data_clean)

# Conversion du format de date : convertit la variable Dates du format texte (%d/%m/%Y) vers un format date reconnu par R
data_clean$Dates <- as.Date(data_clean$Dates, format = "%d/%m/%Y")

# 3. Structuration des données
# Conversion des variables catégorielles en facteurs (variables qualitatives) → utile pour les analyses statistiques (ANOVA, modèles expérimentaux)
data_clean <- data_clean %>%
  mutate(
    Semaines = as.factor(Semaines),
    Traitements = as.factor(Traitements),
    Blocs = as.factor(Blocs),
    Plants = as.factor(Plants)
  )

# Normalisation de la variable Hauteur : permet de ramener les variables à une même échelle
data_clean <- data_clean %>%
  mutate(Hauteur_norm = (Hauteur - min(Hauteur)) /
           (max(Hauteur) - min(Hauteur)))

# Création de la variable Surface foliaire
data_clean <- data_clean %>%
  mutate(Surface_Foliaire = Longeur * Largeur * F_Correct)

# 4. Statistiques descriptives
# Moyenne : somme des valeurs divisée par le nombre d'observations
mean(data_clean$Hauteur)

# Médiane : valeur centrale qui divise les données ordonnées en deux parties égales (50% des observations sont inférieures, 50% supérieures)
median(data_clean$Hauteur)
median((data_clean$Diametre))

# Fonction pour calculer le mode : valeur la plus fréquente dans un jeu de données
mode_func <- function(x) {
  ux <- unique(x)           # Extrait les valeurs uniques
  ux[which.max(tabulate(match(x, ux)))]  # Identifie la position de la valeur la plus fréquente
}
mode_func(data_clean$Hauteur)

# Variance : mesure l'écart moyen des observations par rapport à la moyenne, exprimé au carré
var(data_clean$Hauteur)

# Écart-type : racine carrée de la variance, exprimé dans la même unité que les données
sd(data_clean$Hauteur)

# Histogramme simple : visualise la distribution des données (asymétrie, présence d'outliers, forme normale ou non)
hist(data_clean$Hauteur)

# 5. Visualisation des données
# Histogramme avec ggplot2 : visualise la répartition des données en classes
ggplot(data_clean, aes(x = Hauteur)) +
  geom_histogram(bins = 20, fill = "blue", color = "black") +
  theme_minimal()  # Applique un style épuré et professionnel

# Boxplot par traitement : permet de comparer les hauteurs entre traitements
# La boîte représente l'intervalle interquartile (Q1 à Q3), la ligne centrale est la médiane
ggplot(data_clean, aes(x = Traitements, y = Hauteur)) +
  geom_boxplot() +
  theme_minimal()

# Relation entre diamètre et hauteur : nuage de points avec droite de régression linéaire
# Permet d'étudier la corrélation entre deux variables biophysiques
ggplot(data_clean, aes(x = Diametre, y = Hauteur)) +
  geom_point() +                      # Nuage de points
  geom_smooth(method = "lm") +        # Ajoute une droite de régression linéaire
  theme_minimal()
