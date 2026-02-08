############################################
#  Partie Israe  EDA 
##########################################


# Nettoyage de l'environnement
rm(list = ls())

# Chargement des librairies nécessaires
library(readxl)     # Lire les fichiers Excel
library(dplyr)      # Manipulation de données
library(ggplot2)    # Visualisation
library(MASS)       # GLM Gamma
library(car)        # Diagnostics



# Importation des données
data <- read_excel(file.choose())


# Aperçu général
str(data)
summary(data)


# Renommage des variables (bonne pratique professionnelle)
data <- data %>%
  rename(
    booking_id = `Booking_ID`,
    adults = `Nb d’adultes`,
    children = `Nb d’enfants`,
    weekend_nights = `Nb de nuits de week-end`,
    weekday_nights = `Nb de nuits en semaine`,
    meal_plan = `Type de formule de repas`,
    parking = `Place de parking requise`,
    room_type = `Type de chambre réservée`,
    lead_time = `Délai de réservation (jours avant l’arrivée)`,
    arrival_month = `Mois d’arrivée`,
    market_segment = `Segment de marché`,
    repeated_guest = `Client récurrent`,
    prev_cancellations = `Nb d’annulations précédentes`,
    prev_not_canceled = `Nb de réservations précédentes non annulées`,
    adr = `Prix moyen par chambre (ADR)`,
    special_requests = `Nb de demandes spéciales`,
    reservation_status = `Statut de la réservation`
  )


# Table de fréquences
table(data$reservation_status)


# Création de la variable binaire is_canceled
data$is_canceled <- ifelse(
  data$reservation_status == "Canceled",
  1,
  0
)

#tableau de frequence
table(data$is_canceled)
prop.table(table(data$is_canceled))

#visualization avec nbre d'obs
ggplot(data, aes(x = factor(is_canceled))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Répartition des annulations de réservation",
    x = "Statut (0 = Non annulée, 1 = Annulée)",
    y = "Nombre de réservations"
  )

#visualization avec pourcentage
ggplot(data, aes(x = factor(is_canceled), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion des annulations de réservation",
    x = "Statut (0 = Non annulée, 1 = Annulée)",
    y = "Proportion"
  )


# Conversion des variables qualitatives en facteurs
data <- data %>%
  mutate(
    meal_plan = as.factor(meal_plan),
    room_type = as.factor(room_type),
    market_segment = as.factor(market_segment),
    reservation_status = as.factor(reservation_status)
  )


#histogramme de delai (normalite) : 
ggplot(data, aes(x = lead_time)) +
  geom_histogram(
    bins = 50,
    fill = "darkorange",
    color = "black"
  ) +
  labs(
    title = "Distribution du délai de réservation",
    x = "Délai (jours avant l’arrivée)",
    y = "Fréquence"
  )

#boxplot (outliers):
ggplot(data, aes(y = lead_time)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot du délai de réservation",
    y = "Délai (jours)"
  )

#moyenne vs median: 
mean_lead <- mean(data$lead_time)
median_lead <- median(data$lead_time)

mean_lead
median_lead

#densite : 
ggplot(data, aes(x = lead_time)) +
  geom_density(fill = "orange", alpha = 0.4) +
  labs(
    title = "Densité du délai de réservation",
    x = "Délai (jours)",
    y = "Densité"
  )

#boxplot delai selon statut de reservation : 
ggplot(data, aes(x = factor(is_canceled), y = lead_time)) +
  geom_boxplot(fill = c("lightgreen", "salmon")) +
  labs(title = "Délai de réservation selon le statut",
       x = "Annulation",
       y = "Délai (jours)")


#heatmap correlation : 
library(ggcorrplot)
# Sélection des variables numériques explicitement avec dplyr
num_vars <- dplyr::select(data, lead_time, adr, adults, children, special_requests)
str(num_vars)
cor_matrix <- cor(num_vars, use = "complete.obs") # ignore les NAs
ggcorrplot(cor_matrix, lab = TRUE, colors = c("blue", "white", "red"))



#================================
# Partie fatima raouane
#================================

#########################################################################
## AXE 3 : ÉTUDE DU DÉLAI DE RÉSERVATION (MODÈLE GAMMA) 
#########################################################################

# 0. PRÉPARATION DES DONNÉES

# On filtre les réservations non-annulées
data_gamma <- data %>% filter(reservation_status == "Not_Canceled")

# Filtrage des outliers (Méthode IQR)
Q1 <- quantile(data_gamma$lead_time, 0.25)
Q3 <- quantile(data_gamma$lead_time, 0.75)
IQR_val <- Q3 - Q1
limite_sup <- Q3 + 1.5 * IQR_val

str(data_gamma)
summary(data_gamma)

# La loi Gamma exige y > 0. Correction pour les lead_time nuls
data_gamma$lead_time_gamma <- ifelse(data_gamma$lead_time == 0, 0.5, data_gamma$lead_time)

# 1. ANALYSE DESCRIPTIVE (Justification du choix de la loi Gamma)
summary(data_gamma$lead_time_gamma)

# Visualisation de l'asymétrie 
hist(data_gamma$lead_time_gamma, breaks = 40, prob = TRUE, 
     main = "Distribution du Délai", xlab = "Jours", col = "steelblue")
lines(density(data_gamma$lead_time_gamma), col = "red", lwd = 2)

# Test de normalité (Q-Q Plot)
qqnorm(data_gamma$lead_time_gamma)
qqline(data_gamma$lead_time_gamma, col = "red")

# Boxplot par segment (Mise en évidence de l'hétéroscédasticité)
boxplot(lead_time_gamma ~ market_segment, data = data_gamma, 
        col = terrain.colors(5), main = "Dispersion du délai par segment",
        las = 2, cex.axis = 0.7)

# Suppression des outliers identifiés
data_gamma <- data_gamma %>% filter(lead_time_gamma <= limite_sup)

# Vérifie la nouvelle taille de ton échantillon
nrow(data_gamma)


# 2. ÉTAPE A : LE MODÈLE INITIAL "COMPLET" (Exploratoire)
# On inclut beaucoup de variables pour tester leur pertinence
options(scipen = 999)

modele_complet <- glm(
  lead_time_gamma ~ adr + market_segment + adults + children + 
    room_type + weekend_nights + weekday_nights + 
    special_requests + arrival_month, 
  family = Gamma(link = "log"), 
  data = data_gamma
)

# On affiche les résultats du modèle complet
# C'est ici qu'on remarque que certaines variables ne sont pas significatives (p-value élevée)
summary(modele_complet)

# Calcul des impacts (Odds-Ratios équivalents) du modèle complet pour comparaison
exp(coef(modele_complet))

# 3. ÉTAPE B : SÉLECTION AUTOMATIQUE (L'Arbitrage par l'AIC)
#  utiliser l'AIC pour simplifier le modèle
modele_aic <- step(modele_complet, direction = "backward",k = 2)
summary(modele_aic)

# Sélection par le BIC (on change k pour log(n))
n <- nrow(data_gamma)
modele_bic <- step(modele_complet, direction = "backward", k = log(n))
summary(modele_bic)

# Comparaison des deux modèles
AIC(modele_aic, modele_bic)
BIC(modele_aic, modele_bic)

# 4. ÉTAPE C : LE MODÈLE FINAL (Optimisé)
# 
summary(modele_bic)

# Significativité globale du modèle final
anova(glm(lead_time_gamma ~ 1, family = Gamma(link = "log"), data = data_gamma),
      modele_bic, 
      test = "Chisq")

Anova(modele_bic, type = "III", test.statistic = "Wald")

# Calcul du VIF (Variance Inflation Factor)
vif(modele_bic)

# 5. INTERPRÉTATION FINALE DES PARAMÈTRES
# Impacts multiplicatifs définitifs sur le délai moyen
exp(coef(modele_bic))

# Intervalles de confiance à 95%
exp(confint(modele_bic, level = 0.95))

# 6. QUALITÉ D'AJUSTEMENT ET DIAGNOSTICS 
# Calcul du Pseudo-R2 de McFadden
library(pscl)
round(pR2(modele_bic), 3)

# Analyse de la dispersion (phi_hat)
# Pour Gamma, phi = 1/nu. R l'estime via la déviance/degrés de liberté
summary(modele_bic)$dispersion

# Diagnostic des résidus de Pearson
# 1. Calcul des résidus de Pearson standardisés pour le modèle final
residus_std <- rstandard(modele_bic)
valeurs_ajustees <- predict(modele_bic, type = "response")

# 2. Graphique de diagnostic 
plot(valeurs_ajustees, residus_std,
     xlab = "Délais prédits (jours)",
     ylab = "Résidus de Pearson standardisés",
     main = "Diagnostic du Modèle Final (Gamma)",
     pch = 20, col = rgb(0, 0, 1, 0.5))

# Ajouter les lignes de seuil à -2, 0 et 2
abline(h = c(-2, 0, 2), lty = 2, col = "red", lwd = 2)

# 3. Test de la distribution des résidus
hist(residus_std, breaks = 30, main = "Distribution des résidus", col = "lightgrey")












####################################################################
# Partie Youssefi Nouhaila inspire du livre Springer Texts in Statistics
# Generalized Linear Models With Examples in R 
####################################################################
# ===============================================
# Diagnostics GLM Gamma pour lead_time_gamma
# ===============================================

library(ggplot2)
library(gridExtra)
library(statmod)   # pour quantile residuals
library(MASS)      # pour fonctions glm.nb si besoin
library(car)       # pour vif()

# Modèle final BIC (simplifié)
modele <- modele_bic


# -------------------------------
# 2. Vérification des densités
# -------------------------------

 # Calculer les densites

dens_obs <- density(data_gamma$lead_time_gamma)
dens_fitted <- density(predict(modele, type='response'))

# def limite Y en prenant le max des deux densites
ymax <- max(dens_obs$y, dens_fitted$y) * 1.1 

# plot avec axes ajustes

plot(dens_obs, main="Density of observed lead_time_gamma",
     ylim=c(0, ymax))
lines(dens_fitted, col='red', lwd=2)
legend("topright", legend=c("Observed", "Fitted"), 
       col=c("black","red"), lwd=2)

#plot(predict(modele, type='response'), rstandard(modele, type='deviance'))

# 1. Résidus de base
resid_response <- resid(modele, type='response')
resid_pearson  <- resid(modele, type='pearson')
resid_dev      <- resid(modele, type='deviance')
resid_std      <- rstandard(modele, type='deviance')
resid_quant    <- qresid(modele)  # nécessite statmod
# Les plots
plot(density(resid_response), main="Response residuals")
plot(density(resid_pearson), main="Pearson residuals")
plot(density(resid_dev), main="Deviance residuals")

# -------------------------------
# 3. Indépendance des observations
# -------------------------------
plot(1:nrow(data_gamma), resid_std, col='gray',
     main="Independence check",
     xlab="Observation order", 
     ylab="Standardized deviance residuals")

lines(lowess(resid_std), col="red")

# -------------------------------
# 4. Résidus vs valeurs ajustées
# -------------------------------
fitted_vals <- predict(modele, type='response')
plot(fitted_vals, resid_std, main="Residuals vs Fitted",
     xlab="Fitted values", ylab="Standardized deviance residuals")

abline(h=0, lty=2, col='blue')
lines(lowess(fitted_vals, resid_std), col='red', lwd=2)

# -------------------------------
# 5. Résidus vs prédicteurs
# -------------------------------
predictors <- c("adr","adults","weekend_nights","weekday_nights",
                "market_segment")
par(mfrow=c(3,2))
for(pred in predictors){
  if(is.factor(data_gamma[[pred]])){
    boxplot(resid_std ~ data_gamma[[pred]], main=paste("Residuals vs", pred))
  } else{
    plot(data_gamma[[pred]], resid_std, main=paste("Residuals vs", pred),
         xlab=pred, ylab="Std deviance residuals")
    lines(lowess(data_gamma[[pred]], resid_std), col='red', lwd=2)
  }
}

predic<-"market_segment"
boxplot(resid_std ~ data_gamma[[predic]], main=paste("Residuals vs", pred))


# -------------------------------
# 6. Vérification de la fonction lien
# -------------------------------

par(mfrow=c(1,2))
# Résidus travaillés (working residuals) vs linear predictor
plot(modele$linear.predictors, resid_std,
     main="Standardized residuals vs Linear predictor",
     xlab="Linear predictor", ylab="Standardized deviance residuals")
abline(h=0, lty=2, col="blue")
lines(lowess(modele$linear.predictors, resid_std), col='red')


resid_working <- resid(modele, type = "working")

plot(modele$linear.predictors, resid_working,
     main="Working residuals vs Linear predictor",
     xlab="Linear predictor", ylab="Working residuals")
abline(h=0, lty=2, col="blue")
lines(lowess(modele$linear.predictors, resid_working), col='red')


# -------------------------------
# 7. Vérification de la distribution (Gamma)
# -------------------------------
qqnorm(resid_quant, main="QQ plot of quantile residuals")
qqline(resid_quant, col='red')

# -------------------------------
# 8. Outliers et observations influentes
# -------------------------------
par(mfrow=c(1,3))
# Cook's distance
cooksd <- cooks.distance(modele)
plot(cooksd, type='h', main="Cook's distance")
abline(h = 2*mean(cooksd, na.rm=TRUE), col='red', lty=2)


plot( dffits(modele), type="h", las=1, ylab="DFFITS")
plot( hatvalues(modele), type="h", las=1 ) 
# Points à haute influence
high_influence <- which(cooksd > 2*mean(cooksd, na.rm=TRUE))
summary(data_gamma[high_influence, "lead_time_gamma"])
length(high_influence)






#modele_sans <- glm(..., data = data_gamma[-high_influence, ])
data_sans_influence <- data_gamma[-high_influence, ]

modele_sans <- glm(
  lead_time_gamma ~ adr + adults + weekend_nights + weekday_nights 
  + market_segment,
  family = Gamma(link = "log"),
  data = data_sans_influence
)

coef(modele)
coef(modele_sans)

comparison <- cbind(
  Complet = coef(modele),
  Sans_Influents = coef(modele_sans)
)

comparison



fitted_complet <- fitted(modele)
fitted_sans <- fitted(modele_sans)

summary(fitted_complet)
summary(fitted_sans)

plot(density(fitted_complet),
     main = "modèle complet vs sans observations influentes",
     xlab = "Lead time ajusté",
     lwd = 2)

lines(density(fitted_sans),col = "red",lwd = 2)

legend("topright",
       legend = c("Modèle complet", "Sans influents"),
       col = c("black", "red"),lwd = 2)




# -------------------------------
# 9. Colinéarité
# -------------------------------

vif_values <- vif(modele_sans)
vif_values


# -------------------------------
# 10. Pseudo R2 (McFadden)
# -------------------------------
null_dev <- modele_sans$null.deviance
res_dev  <- modele_sans$deviance
pseudoR2 <- 1 - res_dev/null_dev
pseudoR2











