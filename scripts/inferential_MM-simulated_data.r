# Ouverture d'une fenêtre graphique pour afficher les graphiques
# (uniquement nécessaire sur certains systèmes, comme Windows)
windows()

# GÉNÉRATION DES DONNÉES ET PREMIÈRE RÉGRESSION LINÉAIRE

set.seed(13) # Fixer une graine pour garantir la reproductibilité des résultats
plaisir <- rnorm(100, mean = 50, sd = 10) # Génère 100 valeurs aléatoires pour "plaisir", avec une moyenne de 50 et un écart-type de 10
capacite <- rnorm(100, mean = 30, sd = 10) + 0.8 * plaisir # Génère "capacite" en ajoutant une dépendance linéaire de 0.8 sur "plaisir"

# Visualisation des données générées
plot(plaisir, capacite, main = "Relation capacité et plaisir perçu lié aux statistiques", 
     xlab = "Plaisir", ylab = "Capacité") # Création d'un nuage de points
abline(lm(capacite ~ plaisir)) # Ajout de la droite de régression linéaire
lm(capacite ~ plaisir) # Ajustement du modèle linéaire et affichage des coefficients

# GÉNÉRATION D'UN NOUVEAU JEU DE DONNÉES ET DEUXIÈME RÉGRESSION

set.seed(84) # Fixer une autre graine pour générer un nouveau jeu de données
plaisir <- rnorm(100, mean = 50, sd = 10) # Génère un nouveau "plaisir"
capacite <- rnorm(100, mean = 30, sd = 10) + 0.8 * plaisir # Génère "capacite" avec la même dépendance linéaire

# Visualisation des nouvelles données
plot(plaisir, capacite, main = "Relation capacité et plaisir perçu lié aux statistiques", 
     xlab = "Plaisir", ylab = "Capacité")
abline(lm(capacite ~ plaisir)) # Ajout de la droite de régression linéaire
lm(capacite ~ plaisir) # Ajustement du modèle linéaire et affichage des coefficients




# ========================================================================
## pourquoi centrer les données
# ========================================================================

# Définir le nombre d'observations
set.seed(21)  # Pour rendre les résultats reproductibles
n <- 100  # Nombre d'individus

# Simuler des poids (en kg) avec une distribution normale
poids <- rnorm(n, mean = 70, sd = 10)  # Moyenne de 70 kg, écart-type de 15 kg

# Simuler des tailles (en cm) en fonction du poids
# Taille = une composante liée au poids + une composante aléatoire
taille <- 0.8 * poids + rnorm(n, mean = 115, sd = 5)  # Corrélation avec le poids


# Visualisation des nouvelles données
plot(poids, taille, main = "Relation entre le poids et la taille", 
     xlab = "Poids", ylab = "Taille")
abline(lm(taille ~ poids)) # Ajout de la droite de régression linéaire
lm(taille ~ poids) # Ajustement du modèle linéaire et affichage des coefficients

poids_centre <- scale(poids, scale=F)
plot(poids_centre, taille, main = "Relation entre le poids centré et la taille", 
     xlab = "Poids Centre", ylab = "Taille")
abline(lm(taille ~ poids_centre)) # Ajout de la droite de régression linéaire
lm(taille ~ poids_centre) # Ajustement du modèle linéaire et affichage des coefficients




# ========================================================================
# ANALYSE DE RÉGRESSION LINÉAIRE MIXTE
# ========================================================================


# Chargement des bibliothèques nécessaires
library(ggplot2)
library(emmeans)
library(lmerTest)

# GÉNÉRATION DE DONNÉES SIMULÉES POUR UNE ANALYSE MIXTE
set.seed(31) # Graine pour reproductibilité

# Création d'une liste de 100 étudiants
etudiant.e.s <- seq(100)

# Moyennes individuelles des heures de travail et des notes
heures_moyennes_par_etudiant.e.s <- rnorm(100, 6, 2) # Heures de travail : moyenne de 6h avec écart-type de 2
notes_moyennes_par_etudiant.e.s <- rnorm(100, 7, 2) # Notes moyennes : moyenne de 7, écart-type de 2

# Liste des examens (10 matières)
examens <- c("Langugage", "Cognition-numerique", "OMN", "Pathologie", 
             "Developpement", "Reeducation", "Philosophie", "Sommeil", 
             "Apprentissage", "Neuroimagerie")

# Initialisation des vecteurs pour les heures de travail et les notes
heures_travail_promo <- c() # Toutes les heures de travail pour l'ensemble des étudiants et examens
notes_promo <- c() # Toutes les notes pour l'ensemble des étudiants et examens

# Simulation des heures de travail et des notes pour chaque étudiant
for (i in etudiant.e.s) {
    # Heures de travail individuelles pour les examens
    heures_travail_individuel <- heures_moyennes_par_etudiant.e.s[i] + rnorm(length(examens), 0, 0.5)
    heures_travail_promo <- c(heures_travail_promo, heures_travail_individuel)
    
    # Notes pour chaque examen, dépendant des heures de travail (relation linéaire avec une pente de 0.6)
    notes_promo <- c(notes_promo, runif(length(examens), notes_moyennes_par_etudiant.e.s[i] - 1, 
                   notes_moyennes_par_etudiant.e.s[i] + 1) + rnorm(1, 0.6, 0.3) * heures_travail_individuel)
}

# Création d'un dataframe pour regrouper les données simulées
df <- data.frame(
  etudiant.e.s = rep(etudiant.e.s, each = length(examens)), # Identifiant étudiant répété pour chaque examen
  examens = rep(examens, length(etudiant.e.s)), # Examens correspondants
  notes = notes_promo, # Notes obtenues
  heures_travail = heures_travail_promo # Heures de travail pour chaque examen
)

# VISUALISATION DE LA RELATION HEURES DE TRAVAIL / NOTES
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0.6, col = "green", lwd = 4) # Droite théorique basée sur les données simulées

# RÉGRESSION LINÉAIRE SIMPLE
model_lm_no_rdm_intercept_no_rdm_slope <- lm(notes ~ heures_travail, data = df)
summary(model_lm_no_rdm_intercept_no_rdm_slope) # Résumé du modèle linéaire simple

# COMPARAISON DES DROITES DE RÉGRESSION
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0.6, col = "green", lwd = 4) # Droite théorique
abline(6.37982, 0.69125, col = "red", lwd = 4) # Droite basée sur la régression simple ajustée

# RÉGRESSION MIXTE : INTERCEPT ALÉATOIRE
model_lmer_rdm_intercept_no_rdm_slope <- lmer(notes ~ heures_travail + (1 | etudiant.e.s), data = df, REML = FALSE)
summary(model_lmer_rdm_intercept_no_rdm_slope) # Résumé du modèle mixte avec intercept aléatoire

plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0.6, col = "green", lwd = 4) # Droite théorique
abline(6.37982, 0.69125, col = "red", lwd = 4) # Modèle linéaire
abline(7.08512, 0.57292, col = "blue", lwd = 4) # Modèle mixte (intercept aléatoire)

# VISUALISATION DES INTERCEPTS ALÉATOIRES
random_intercepts <- ranef(model_lmer_rdm_intercept_no_rdm_slope)$etudiant.e.s[, 1] + 7.08512
plot(random_intercepts, main = "Intercepts aléatoires par étudiant", 
     ylab = "Valeur de l'intercept", xlab = "Étudiants")



# MODÈLE MIXTE AVEC PENTE ET INTERCEPTS ALÉATOIRES
# Ici, nous permettons à la relation entre heures de travail et notes (la pente) de varier d'un étudiant à l'autre.
model_lmer_rdm_intercept_rdm_slope <- lmer(notes ~ heures_travail + (heures_travail | etudiant.e.s), data = df, REML = FALSE)
summary(model_lmer_rdm_intercept_rdm_slope) # Résumé du modèle mixte avec pente et intercepts aléatoires

# VISUALISATION DES DIFFÉRENTES DROITES DE RÉGRESSION
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0.6, col = "green", lwd = 4) # Droite théorique (relation simulée)
abline(6.37982, 0.69125, col = "red", lwd = 4) # Modèle linéaire simple
abline(7.08512, 0.57292, col = "blue", lwd = 4) # Modèle mixte (intercept aléatoire)
abline(7.04180, 0.57540, col = "#00ffea", lwd = 4) # Modèle mixte (intercept + pente aléatoires)

# EXTRACTION DES INTERCEPTS ET PENTES ALÉATOIRES POUR CHAQUE ÉTUDIANT
# Intercepts aléatoires par étudiant
random_intercepts <- ranef(model_lmer_rdm_intercept_rdm_slope)$etudiant.e.s[, 1] + 7.04180 # Ajout de l'intercept moyen pour aligner les valeurs
plot(random_intercepts, main = "Intercepts aléatoires par étudiant", 
     ylab = "Intercept", xlab = "Étudiants")

# Pentes aléatoires par étudiant
random_slopes <- ranef(model_lmer_rdm_intercept_rdm_slope)$etudiant.e.s[, 2] + 0.57540 # Ajout de la pente moyenne
plot(random_slopes, main = "Pentes aléatoires par étudiant", 
     ylab = "Pente", xlab = "Étudiants")

# COMPARAISON DES PENTES AJUSTÉES
# Visualisation pour comparer les pentes théoriques, ajustées par le modèle linéaire simple,
# et ajustées par les deux modèles mixtes (avec ou sans pente aléatoire).
plot(c(0.57540, 0.57292, 0.69125, 0.6), c(1, 2, 3, 4), 
     pch = 19, col = c("#00ffea", "blue", "red", "green"), lwd = 5, 
     xlim = c(0.50, 0.75), ylab = NA, xlab = NA, yaxt = "n", 
     main = "Comparaison des pentes ajustées")
lines(c(0.6, 0.6), c(0, 5), lty = 2) # Ligne verticale indiquant la pente théorique
lines(c(0.69125 - 0.04367, 0.69125 + 0.04367), c(3, 3), col = "red") # Intervalle de confiance pour le modèle linéaire
lines(c(0.57292 - 0.03794, 0.57292 + 0.03794), c(2, 2), col = "blue") # Intervalle de confiance pour le modèle mixte (intercept aléatoire)
lines(c(0.57540 - 0.04658, 0.57540 + 0.04658), c(1, 1), col = "#00ffea") # Intervalle de confiance pour le modèle mixte (intercept + pente aléatoires)

# AJOUT DES ÉTIQUETTES POUR L'AXE Y
axis(2, at = c(1, 2, 3, 4), labels = c("Intercept & pente", "Intercept seul", "LM", "Théorique"))

# RÉSUMÉS DES MODÈLES POUR ANALYSE COMPARATIVE
# Ces commandes affichent les coefficients, les écarts-types et les tests de significativité pour chaque modèle.
summary(model_lm_no_rdm_intercept_no_rdm_slope) # Résumé du modèle linéaire simple
summary(model_lmer_rdm_intercept_no_rdm_slope) # Résumé du modèle mixte avec intercept aléatoire
summary(model_lmer_rdm_intercept_rdm_slope) # Résumé du modèle mixte avec intercept + pente aléatoires




# ========================================================================
# ANALYSE DE RÉGRESSION LINÉAIRE MIXTE version 2
# ========================================================================

# GÉNÉRATION DE DONNÉES SIMULÉES POUR UNE ANALYSE MIXTE
set.seed(31) # Graine pour reproductibilité

# Initialisation des vecteurs pour les heures de travail et les notes
heures_travail_promo <- c() # Toutes les heures de travail pour l'ensemble des étudiants et examens
notes_promo <- c() # Toutes les notes pour l'ensemble des étudiants et examens

# Simulation des heures de travail et des notes pour chaque étudiant
for (i in etudiant.e.s) {
  # Heures de travail individuelles pour les examens
  heures_travail_individuel <- heures_moyennes_par_etudiant.e.s[i] + rnorm(length(examens), 0, 0.5)
  heures_travail_promo <- c(heures_travail_promo, heures_travail_individuel)
  
  # Notes pour chaque examen, dépendant des heures de travail (relation linéaire avec une pente de 0.6)
  notes_promo <- c(notes_promo, runif(length(examens), notes_moyennes_par_etudiant.e.s[i] - 1, 
                                      notes_moyennes_par_etudiant.e.s[i] + 1) + rnorm(1, 0, 0.3) * heures_travail_individuel)
}

# Création d'un dataframe pour regrouper les données simulées
df <- data.frame(
  etudiant.e.s = rep(etudiant.e.s, each = length(examens)), # Identifiant étudiant répété pour chaque examen
  examens = rep(examens, length(etudiant.e.s)), # Examens correspondants
  notes = notes_promo, # Notes obtenues
  heures_travail = heures_travail_promo # Heures de travail pour chaque examen
)

# VISUALISATION DE LA RELATION HEURES DE TRAVAIL / NOTES
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0, col = "green", lwd = 4) # Droite théorique basée sur les données simulées

# RÉGRESSION LINÉAIRE SIMPLE
model_lm_no_rdm_intercept_no_rdm_slope <- lm(notes ~ heures_travail, data = df)
summary(model_lm_no_rdm_intercept_no_rdm_slope) # Résumé du modèle linéaire simple

# COMPARAISON DES DROITES DE RÉGRESSION
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0, col = "green", lwd = 4) # Droite théorique
abline(6.37982, 0.09125, col = "red", lwd = 4) # Droite basée sur la régression simple ajustée

# RÉGRESSION MIXTE : INTERCEPT ALÉATOIRE
model_lmer_rdm_intercept_no_rdm_slope <- lmer(notes ~ heures_travail + (1 | etudiant.e.s), data = df, REML = FALSE)
summary(model_lmer_rdm_intercept_no_rdm_slope) # Résumé du modèle mixte avec intercept aléatoire

# AJOUT DES DROITES DIFFÉRENTES AU GRAPHIQUE
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0, col = "green", lwd = 4) # Droite théorique
abline(6.37982, 0.09125, col = "red", lwd = 4) # Modèle linéaire
abline(fixef(model_lmer_rdm_intercept_no_rdm_slope), col = "blue", lwd = 4) # Modèle mixte (intercept aléatoire)

# MODÈLE MIXTE AVEC PENTE ET INTERCEPTS ALÉATOIRES
# Ici, nous permettons à la relation entre heures de travail et notes (la pente) de varier d'un étudiant à l'autre.
model_lmer_rdm_intercept_rdm_slope <- lmer(notes ~ heures_travail + (heures_travail | etudiant.e.s), data = df, REML = FALSE)
summary(model_lmer_rdm_intercept_rdm_slope) # Résumé du modèle mixte avec pente et intercepts aléatoires

# VISUALISATION DES DIFFÉRENTES DROITES DE RÉGRESSION
plot(df$notes ~ df$heures_travail, main = "Relation heures de travail et notes à un examen", 
     xlab = "Heures de travail", ylab = "Note examen")
abline(7, 0, col = "green", lwd = 4) # Droite théorique (relation simulée)
abline(6.37982, 0.09125, col = "red", lwd = 4) # Modèle linéaire simple
abline(fixef(model_lmer_rdm_intercept_no_rdm_slope), col = "blue", lwd = 4) # Modèle mixte (intercept aléatoire)
abline(fixef(model_lmer_rdm_intercept_rdm_slope), col = "#00ffea", lwd = 4) # Modèle mixte (intercept + pente aléatoires)

# COMPARAISON DES PENTES AJUSTÉES
# Visualisation pour comparer les pentes théoriques, ajustées par le modèle linéaire simple,
# et ajustées par les deux modèles mixtes (avec ou sans pente aléatoire).
plot(c(-0.02460326 , -0.02707948 , 0.09125, 0), c(1, 2, 3, 4), 
     pch = 19, col = c("#00ffea", "blue", "red", "green"), lwd = 5, 
     xlim = c(-0.08, 0.15), ylab = NA, xlab = NA, yaxt = "n", 
     main = "Comparaison des pentes ajustées")
lines(c(0.0, 0.0), c(0, 5), lty = 2) # Ligne verticale indiquant la pente théorique
lines(c(0.09125 - 0.04367, 0.09125 + 0.04367), c(3, 3), col = "red") # Intervalle de confiance pour le modèle linéaire
lines(c(-0.02707948  - 0.03794, -0.02707948  + 0.03794), c(2, 2), col = "blue") # Intervalle de confiance pour le modèle mixte (intercept aléatoire)
lines(c(-0.02460326 - 0.04658, -0.02460326 + 0.04658), c(1, 1), col = "#00ffea") # Intervalle de confiance pour le modèle mixte (intercept + pente aléatoires)

# AJOUT DES ÉTIQUETTES POUR L'AXE Y
axis(2, at = c(1, 2, 3, 4), labels = c("Intercept & pente", "Intercept seul", "LM", "Théorique"))

# RÉSUMÉS DES MODÈLES POUR ANALYSE COMPARATIVE
# Ces commandes affichent les coefficients, les écarts-types et les tests de significativité pour chaque modèle.
summary(model_lm_no_rdm_intercept_no_rdm_slope) # Résumé du modèle linéaire simple
summary(model_lmer_rdm_intercept_no_rdm_slope) # Résumé du modèle mixte avec intercept aléatoire
summary(model_lmer_rdm_intercept_rdm_slope) # Résumé du modèle mixte avec intercept + pente aléatoires



# ========================================================================
# Résultat significatif
# ========================================================================


# la distribution de student
# Set up the plot for the normal distribution
set.seed(15)
plot(
  density(rnorm(1000, mean = 0, sd = 1), adjust = 10),
  xlim = c(-10, 10), 
  lwd = 3, 
  col = "black", 
  main = "Density Comparison: Normal vs t-Distributions",
  xlab = "Value",
  ylab = "Density"
)

# Add t-distributions with varying degrees of freedom
lines(density(rt(1000, df = 2), adjust = 10), col = "#ad4fb5", lwd = 3, lty = 2)
lines(density(rt(1000, df = 5), adjust = 10), col = "#b54f8a", lwd = 3, lty = 3)
lines(density(rt(1000, df = 10), adjust = 10), col = "#b54f57", lwd = 3, lty = 4)
lines(density(rt(1000, df = 10000), adjust = 10), col = "#b5ad4f", lwd = 3, lty = 5)

# Add a legend for clarity
legend(
  "topright", 
  legend = c("Normal (mean=0, sd=1)", "t-distribution (df=2)", 
             "t-distribution (df=5)", "t-distribution (df=10)", "t-distribution (df=10000)"),
  col = c("black","#ad4fb5", "#b54f8a", "#b54f57", "#b5ad4f"), 
  lwd = 3, 
  lty = c(1, 2, 3, 4),
  cex = 0.8, 
  bg = "white"
)

qt(0.975,df=2)
qt(0.975,df=10)
qt(0.975,df=100000)
qnorm(0.975)












