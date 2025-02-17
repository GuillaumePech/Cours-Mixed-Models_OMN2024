
# Packages
library(tidyverse)
library(magrittr)

## Variable catégorielle ---------------------------------------------

# Création d'un jeu de données

N = 200 # 200 participant.es
sujets <- seq(1, N) 
plaisir_stats_dichotomic <- sort(rbinom(N, 1, 0.5)) # 1 pour plaisir et 0 pour non plaisir
notes_gr1 <- rnorm(N/2, mean=10, sd=2)
notes_gr2 <- rnorm(N/2, mean=14, sd=2)

mean1 <- mean(notes_gr1)
mean2 <- mean(notes_gr2)
sd1 <- sd(notes_gr1)
sd2 <- sd(notes_gr2)

data <- data.frame(sujets = sujets, 
                   plaisir_stats = plaisir_stats_dichotomic,
                   notes = c(notes_gr1, notes_gr2))

# Distributions

# un groupe
fig <- data %>% 
  ggplot(aes(x = notes)) +
  geom_histogram(binwidth = 0.3, fill =  'gray', color = 'black', alpha = 0.4) +
  xlab('Notes') +
  ylab('Nombre de personnes') + 
  xlim(0, 20)
ggsave(file="./notes.png", plot=fig, width=7, height=4)

# deux groupes
fig <- data %>% 
  ggplot(aes(x = notes, fill = )) +
  geom_histogram(data=subset(data, plaisir_stats == 0), binwidth = 0.3, fill =  'blue', color = 'black', alpha = 0.4) +
  geom_histogram(data=subset(data, plaisir_stats == 1), binwidth = 0.3, fill =  'green', color = 'black', alpha = 0.4) +
  geom_vline(xintercept = mean1, color = "blue", size = 0.7) + 
  geom_vline(xintercept = mean2, color = "green", size = 0.7) + 
  xlab('Notes') +
  ylab('Nombre de personnes') + 
  xlim(0, 20)
ggsave(file="./notes.png", plot=fig, width=7, height=4)


# Représentation graphique

data %<>% 
  mutate(plaisir_label = ifelse(plaisir_stats == 0, "Non", "Oui"),
         plaisir_stats = ifelse(plaisir_label == 'Non', -0.5, 0.5)) 
data %>% 
  group_by(plaisir_label) %>%
  summarise(VD = mean(notes),
            sd = sd(notes),
            se = sd/sqrt(N),
            CI = se * qt(.975, n() - 1)) %>%
  ggplot(aes(x = plaisir_label, y = VD)) +
  geom_point(data = data,
             aes(x = plaisir_label, y = notes),
             size = 1.5, alpha = 0.2, 
             color = 'blue',
             position = position_jitter(0.1)) +
  geom_point(size = 3, color = 'black', position = position_dodge(0)) +
  geom_errorbar(aes(ymin = VD - CI, ymax = VD + CI), width = 0.01, linewidth = 1, color = 'black', position = position_dodge(0)) +
  xlab("Plaisir à faire des statistiques") +
  ylab("Performances en statistiques")


# Test t pour échantillons indépendants
result <- t.test(data$notes ~ data$plaisir_stats, var.equal=TRUE)
result

# ANOVA univariée
result <- aov(data$notes ~ data$plaisir_stats)
summary(result)

# Régression linéaire
reg <- lm(notes ~ plaisir_stats, data=data)
summary(reg)


## Variable continue -----------------------------------------

# Création d'un jeu de données

plaisir_stats_continue <- rnorm(N, mean=50, sd=10) 
notes <-  rnorm(100, mean = 8, sd = 2) + 0.06  * plaisir_stats_continue 
data2 <- data.frame(sujets = sujets, 
                    plaisir_stats = plaisir_stats_continue,
                    notes = notes)

# Représentation graphique

data2 %>% 
  ggplot(aes(x = plaisir_stats_continue, y = notes)) +
  geom_point(size = 1.5, alpha = 0.5, color = 'blue') +
  geom_smooth(method = "lm", se = 0, color = 'black', size = 1) +
  xlab("Plaisir à faire des statistiques") +
  ylab("Performances en statistiques")

# Régression linéaire

reg <- lm(notes ~ plaisir_stats_continue, data=data2)
summary(reg)

