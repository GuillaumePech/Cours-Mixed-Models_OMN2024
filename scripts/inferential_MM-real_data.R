# ============================================================================
# INITIALISATION ET CHARGEMENT DES DONNÉES
# ============================================================================

# Nettoyer l'environnement pour éviter les conflits avec des objets existants
rm(list = ls())

# Définir le répertoire de travail (remplacez par votre propre chemin)
setwd("C:/Users/mfbpe/OneDrive/Cognitive_Science/class/inferential_MM/gitfolder/data")

# Installer les librairies nécessaires (décommentez et exécutez si c'est la première fois)
# install.packages(c("lmerTest", "afex", "MASS", "emmeans", "dplyr"))

# Charger les librairies nécessaires pour l'analyse statistique
library(emmeans)    # Pour extraire les estimations des modèles
library(lmerTest)   # Pour effectuer des régressions linéaires mixtes
library(tidyverse)      # Pour manipuler les données et les visualiser
library(afex)       # Pour les analyses de variance (ANOVA) avancées
library(MASS)       # Pour des modèles statistiques supplémentaires

# Importer les données depuis un fichier CSV
df_anova <- read.csv("long_format_example1.csv", sep = ",", dec = '.', header = TRUE)

# Explorer la structure des données pour vérifier les colonnes disponibles
str(df_anova)

# ============================================================================
# PRÉPARATION DES DONNÉES
# ============================================================================

# Convertir certaines colonnes en facteurs (catégories)
df_anova$subj <- factor(df_anova$subj)  # Identifier chaque sujet comme un facteur
df_anova$euro_cent <- factor(df_anova$euro_cent, levels = c("euro", "cent"))  # Catégoriser les stimuli (euro ou cent)
df_anova$durations <- factor(df_anova$durations, levels = c("100", "50", "17"))  # Convertir les durées en facteur

# ============================================================================
# TESTS T DE STUDENT PAIRÉS POUR COMPARER LES DIFFÉRENCES ENTRE CONDITIONS
# ============================================================================

# Comparaison des pics (peak) entre les conditions "euro" et "cent" pour chaque durée

# Effectuer un test t pairé pour la durée de 17 ms
t.test(peak ~ euro_cent, data = subset(df_anova, durations == "17"), paired = TRUE)

# Durée = 50 ms
t.test(peak ~ euro_cent, data = subset(df_anova, durations == "50"), paired = TRUE)

# Durée = 100 ms
t.test(peak ~ euro_cent, data = subset(df_anova, durations == "100"), paired = TRUE)

# ============================================================================
# ANALYSE DE VARIANCE (ANOVA) SUR LES DONNÉES
# ============================================================================

# Ajuster un modèle ANOVA avec interaction entre "euro_cent" et "durations"
m_anova_force <- aov_4(peak ~ (euro_cent * durations | subj), data = df_anova)

# Afficher le résumé des résultats de l'ANOVA
summary(m_anova_force)

# ============================================================================
# RÉSUMÉ DES DONNÉES ET COMPARAISONS POST-HOC
# ============================================================================

# Comparaisons post-hoc pour examiner les interactions entre les conditions
post_hoc_anova <- emmeans(m_anova_force, ~euro_cent:durations)  # Comparer les moyennes estimées
post_hoc_anova  # Afficher les résultats des post-hoc

# Résumer les données : calcul des moyennes et des intervalles de confiance
post_hoc_brut <- df_anova %>%
  group_by(durations, euro_cent) %>%
  summarise(
    average = mean(peak, na.rm = TRUE),  # Calculer la moyenne des pics
    SE = sd(peak, na.rm = TRUE) / sqrt(n()),  # Calculer l'erreur standard
    df = n() - 1,  # Degrés de liberté
    lower.CL = average - qt(0.975, df) * SE,  # Limite inférieure de l'intervalle de confiance
    upper.CL = average + qt(0.975, df) * SE   # Limite supérieure de l'intervalle de confiance
  ) %>%
  mutate(durations = paste0("X", durations))  # Renommer les durées pour les graphiques

post_hoc_brut
# Définir les contrastes pour comparer les conditions spécifiques
euro_100ms = c(1, 0, 0, 0, 0, 0)  # Contraste pour euro à 100 ms
cent_100ms = c(0, 1, 0, 0, 0, 0)  # Contraste pour cent à 100 ms
euro_50ms = c(0, 0, 1, 0, 0, 0)  # Contraste pour euro à 50 ms
cent_50ms = c(0, 0, 0, 1, 0, 0)  # Contraste pour cent à 50 ms
euro_17ms = c(0, 0, 0, 0, 1, 0)  # Contraste pour euro à 17 ms
cent_17ms = c(0, 0, 0, 0, 0, 1)  # Contraste pour cent à 17 ms

# Liste des contrastes définis
list_contrast <- list(
  "euro_17ms - cent_17ms" = euro_17ms - cent_17ms,
  "euro_50ms - cent_50ms" = euro_50ms - cent_50ms,
  "euro_100ms - cent_100ms" = euro_100ms - cent_100ms
)

# Appliquer les contrastes et afficher les résultats
contrast(post_hoc_anova, method = list_contrast, adjust = "none")  # Sans correction
confint(contrast(post_hoc_anova, method = list_contrast, adjust = "none"))  # Intervalle de confiance

# Appliquer la correction de Bonferroni pour les tests multiples
contrast(post_hoc_anova, method = list_contrast, adjust = "bonferroni")
confint(contrast(post_hoc_anova, method = list_contrast, adjust = "bonferroni"))
confint(contrast(post_hoc_anova, method = list_contrast, adjust = "bonferroni"), level = 0.99)

# ============================================================================
# MODÈLES LINÉAIRES MIXTES (LMM) 
# ============================================================================

# Importer un jeu de données brut pour les analyses LMM
df_lmm <- read.csv("raw_format_example1.csv", sep = ",", dec = '.', header = TRUE)

# Vérifier la structure des données pour s'assurer que tout est correctement formaté
str(df_lmm)

# Préparer les données en convertissant certaines colonnes en facteurs
df_lmm$subj <- factor(df_lmm$subj)  # Sujet comme facteur
df_lmm$stim_type <- factor(df_lmm$stim_type, levels = c("euro", "cent"))  # Type de stimulus
df_lmm$duration <- factor(df_lmm$duration, levels = c("0.1", "0.05", "0.017"))  # Durée comme facteur
df_lmm$conscious <- factor(df_lmm$conscious, levels = c("0", "1"))  # Conscience comme facteur


contrasts(df_lmm$stim_type) <- contr.sdif(2)
contrasts(df_lmm$duration) <- contr.sdif(3)
# Ajuster les modèles mixtes avec interactions
m_regression_force <- lmer(force_max ~ stim_type * duration + (stim_type * duration | subj), data = df_lmm, REML = TRUE)

# Résumé des effets fixes du modèle mixte
summary(m_regression_force)

# Comparaisons post-hoc pour les modèles mixtes
post_hoc_lmm <- emmeans(m_regression_force, ~stim_type:duration)  # Calculer les moyennes marginales pour chaque interaction (stimulus * durée)
contrast(post_hoc_lmm, method = list_contrast, adjust = "bonferroni")  # Comparaisons avec correction de Bonferroni
confint(contrast(post_hoc_lmm, method = list_contrast, adjust = "bonferroni"))  # Calcul des intervalles de confiance

# Effectuer à nouveau les comparaisons post-hoc avec une limite d'estimation augmentée
post_hoc_lmm <- emmeans(m_regression_force, ~stim_type:duration, lmerTest.limit = 6000)
contrast(post_hoc_lmm, method = list_contrast, adjust = "bonferroni")  # Comparaisons avec correction de Bonferroni
confint(contrast(post_hoc_lmm, method = list_contrast, adjust = "bonferroni"))  # Calcul des intervalles de confiance






# ============================================================================
# DONNEES ABERRANTES ET CONVERGENCE 
# ============================================================================

# Visualisation des données brutes avec ggplot2
ggplot(df_lmm, aes(subj, force_max, color = subj)) +
  geom_boxplot() +  # Ajouter des boîtes pour résumer les données par sujet
  geom_jitter(position = position_dodge(width = 1))  # Ajouter des points pour visualiser la dispersion des données


# Function to detect outliers in data
outlier_mad <- function(dat) {
  # Outlier detection using median and median absolute deviation (MAD)
  outlierp <- median(dat, na.rm = TRUE) + (3 * mad(dat, na.rm = TRUE))
  outlierm <- median(dat, na.rm = TRUE) - (3 * mad(dat, na.rm = TRUE))
  idx <- which(dat <= outlierm | dat >= outlierp)
  dat <- replace(dat, idx, NA)
  
  return(dat)
}

# Nettoyage des données pour les outliers en intra-sujet
df_lmm_clean <- df_lmm %>%
  group_by(subj) %>%  # Grouper les données par sujet
  mutate(force_max = outlier_mad(force_max)) %>%  # Remplacer les valeurs aberrantes par NA
  ungroup()

# Visualisation des données nettoyées
ggplot(df_lmm_clean, aes(subj, force_max, color = subj)) +
  geom_boxplot() +  # Ajouter des boîtes pour les données nettoyées
  geom_jitter(position = position_dodge(width = 1))  # Visualiser la dispersion des données nettoyées

# Ajuster un modèle mixte avec les données nettoyées
m_regression_force_clean <- lmer(force_max ~ stim_type * duration + (stim_type * duration | subj), data = df_lmm_clean, REML = TRUE)
summary(rePCA(m_regression_force_clean))  # Résumé de l'analyse principale des composantes aléatoires
ranef(m_regression_force_clean)  # Effets aléatoires pour chaque sujet

# Modifier la structure du modèle en séparant les termes d'interaction
m_regression_force_clean <- lmer(force_max ~ stim_type * duration + (stim_type + duration | subj), data = df_lmm_clean, REML = TRUE)
summary(m_regression_force_clean)  # Résumé du modèle ajusté

# Comparaisons post-hoc pour le modèle nettoyé
post_hoc_lmm_clean <- emmeans(m_regression_force_clean, ~stim_type:duration, lmerTest.limit = Inf)
contrast(post_hoc_lmm_clean, method = list_contrast, adjust = "bonferroni")  # Comparaisons avec correction Bonferroni
confint(contrast(post_hoc_lmm_clean, method = list_contrast, adjust = "bonferroni"))  # Calcul des intervalles de confiance

# Nettoyage des données pour les outliers en inter-sujet

# Visualisation des effets aléatoires par sujet
plot((ranef(m_regression_force_clean)$subj[, 2]) + fixef(m_regression_force_clean)[2])  # Effets fixes et aléatoires combinés
ranef(m_regression_force_clean)$subj[is.na(outlier_mad((ranef(m_regression_force_clean)$subj[, 4]))), ]  # Identifier les sujets avec des valeurs aberrantes

# Réajuster le modèle en excluant certains sujets avec des valeurs aberrantes
m_regression_force_conscious_clean <- lmer(force_max ~ stim_type * duration + (stim_type + duration | subj), 
                                           data = df_lmm_clean %>% filter(!(subj %in% c(7,12))), REML = TRUE)

# Comparaisons post-hoc pour le modèle nettoyé
post_hoc_lmm_clean <- emmeans(m_regression_force_clean, ~stim_type:duration, lmerTest.limit = Inf)
contrast(post_hoc_lmm_clean, method = list_contrast, adjust = "bonferroni")  # Comparaisons avec correction Bonferroni
confint(contrast(post_hoc_lmm_clean, method = list_contrast, adjust = "bonferroni"))  # Calcul des intervalles de confiance



# ============================================================================
# MODÈLES GÉNÉRALISÉS LINÉAIRES MIXTES (GLMM) POUR ANALYSES PLUS AVANCÉES
# ============================================================================




# Charger un autre jeu de données pour les temps de réaction (RT)
df_lmm <- read.csv("raw_format_example2.csv", sep = ",", dec = '.', header = TRUE)

# Vérifier la structure des données
str(df_lmm)

# Convertir les colonnes pertinentes en facteurs
df_lmm$Participant <- factor(df_lmm$Participant)  # Identifier chaque participant
df_lmm$Face_group <- factor(df_lmm$Face_group, levels = c("Ingroup", "Outgroup"))  # Groupe d'appartenance

# Spécifier les contrastes pour les groupes faciaux
contrasts(df_lmm$Face_group) <- c(-.5, .5)

# Ajuster un modèle mixte pour les temps de réaction
m_rt <- lmer(RT ~ Face_group + (Face_group | Participant), data = df_lmm, REML = TRUE)
summary(m_rt)  # Résumé des résultats du modèle mixte

# Visualiser les résidus pour vérifier l'ajustement du modèle
hist(residuals(m_rt))  # Histogramme des résidus
plot(qqnorm(residuals(m_rt)))  # Graphique QQ-plot des résidus
qqline(residuals(m_rt))  # Ajouter une ligne de référence au QQ-plot

# Ajuster un modèle généralisé mixte (GLMM) avec une distribution Gamma
m_rt_v2 <- glmer(RT ~ Face_group + (Face_group | Participant), data = df_lmm, family = Gamma("identity"))
summary(m_rt_v2)  # Résumé du modèle GLMM

# Visualisation des résidus du GLMM
hist(residuals(m_rt_v2))  # Histogramme des résidus
plot(qqnorm(residuals(m_rt_v2)))  # Graphique QQ-plot des résidus
qqline(residuals(m_rt_v2))  # Ajouter une ligne de référence au QQ-plot
