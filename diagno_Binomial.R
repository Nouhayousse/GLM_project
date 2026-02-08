library(readxl)
library(tidyverse)
library(corrplot) # Pour les matrices de corrélation
library(caret)    # Pour la division train/test et matrice de confusion
library(pROC)     # Pour la courbe ROC


# Liste des feuilles
excel_sheets("Projet_GLM_IDSI2.xlsx")

# Charger une seule feuille
data <- read_excel(file.choose())
head(data)
str(data)




#renommage
colnames(data) <- c("Booking_ID", "Nb_Adultes", "Nb_Enfants", "Nuits_Weekend", 
                    "Nuits_Semaine", "Repas", "Parking", "Type_Chambre", 
                    "Delai_Reservation", "Mois", "Segment", "Recurrent", 
                    "Nb_Annulations", "Nb_Resa_NonAnnulees", "Prix_Moyen", 
                    "Demandes_Speciales", "Statut")

table(data$Statut)


#  Création de la variable cible binaire

data$Y <- ifelse(data$Statut == "Canceled", 1, 0)
table(data$Y)
#creation des factor 
data$Repas     <- as.factor(data$Repas)
data$Parking         <- as.factor(data$Parking)
data$Type_Chambre    <- as.factor(data$Type_Chambre)
data$Mois  <- as.factor(data$Mois)
data$Segment <- as.factor(data$Segment)
data$Recurrent<- as.factor(data$Recurrent)
str(data)
# --- PARTIE 2 : Modélisation GLM Binomial (Risque d'annulation) ---

model_full <- glm(Y ~ Nb_Adultes +
                    Nb_Enfants +
                    Nuits_Weekend +
                    Nuits_Semaine +
                    Repas +
                    Parking +
                    Type_Chambre +
                    Delai_Reservation +
                    Mois +
                    Segment +
                    Recurrent +
                    Nb_Annulations +
                    Nb_Resa_NonAnnulees +
                    Prix_Moyen +
                    Demandes_Speciales,
                  family = binomial(link = "logit"),
                  data = data)



summary(model_full)

exp(coef(model_full))
summary(model_full)$coefficients

library(performance)
r2(model_full)
library(car)
vif(model_full)

library(pscl)
round(pR2(model_full),3)
step(model_full, direction = "backward", k = 2)
step(model_full, direction = "backward", k = log(nrow(data)))
#roc / auc/suille optimal 

data$proba_pred <- predict(model_full, type = "response")

library(pROC)
roc_obj <- roc(data$Y, data$proba_pred)
plot(roc_obj, legacy.axes = TRUE)
auc(roc_obj)
coords(roc_obj,
       x = "best",
       best.method = "youden",
       ret = c("threshold", "sensitivity", "specificity"))

#matrice de confusion 
s <-   #seuil touver 
  pred <- ifelse(data$proba_pred >= s, 1, 0)

table(Observé = data$Y,
      Prédit = pred)

rps = rstandard(model_full)
plot(fitted(model_full), rps)



anova(glm(Y ~ 1, family = binomial, data = data),
      model_full,
      test = "Chisq")


# --- comparaison de modele
#decision tree

install.packages("rpart")
install.packages("rpart.plot")
install.packages("pROC")

library(rpart)
library(rpart.plot)
library(pROC)

data$Y <- as.factor(data$Y)

tree_model <- rpart(
  Y ~ . - Booking_ID - Statut,
  data = data,
  method = "class"
)

rpart.plot(tree_model, type = 2, extra = 104)

#-probabilite predite 
proba_tree <- predict(tree_model, type = "prob")[,2]
# courbe ROC &CAUC
roc_tree <- roc(data$Y, proba_tree)
plot(roc_tree, legacy.axes = TRUE, main = "ROC Decision Tree")
auc(roc_tree)

#choisir meilleur seuille 
coords(
  roc_tree,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

# matrice de confusion
s <-  0.3312297
pred_tree <- ifelse(proba_tree >= s, 1, 0)

data$Y <- ifelse(data$Statut == "Canceled", 1, 0)

table(Observé = data$Y,
      Prédit = pred_tree)





model_final <- glm(Y ~ Nuits_Weekend + Repas + Parking + Delai_Reservation + Mois +
                     Segment + Recurrent + Nb_Annulations + Prix_Moyen + Demandes_Speciales,
                   data = data, family = binomial(link = "logit"))

summary(model_final)

library(performance)
r2(model_final)
library(car)
vif(model_final)



data$proba_pred <- predict(model_final, type = "response")

library(pROC)
roc_obj <- roc(data$Y, data$proba_pred)
plot(roc_obj, legacy.axes = TRUE)
auc(roc_obj)
coords(roc_obj,
       x = "best",
       best.method = "youden",
       ret = c("threshold", "sensitivity", "specificity"))

#matrice de confusion 
s <-   0.2477062 
pred <- ifelse(data$proba_pred >= s, 1, 0)

table(Observé = data$Y,
      Prédit = pred)

rps = rstandard(model_final)
plot(fitted(model_final), rps)













###############################################################
#DIAGNOSTICS POUR BINOMIAL

###############################################################
model <- model_final

# Résidus standardisés (déviance)
resid_std <- rstandard(model)

# Valeurs ajustées (probabilités)
fitted_vals <- fitted(model)




plot(fitted_vals, resid_std,
     xlab = "Probabilités ajustées",
     ylab = "Résidus de déviance standardisés",
     main = "Résidus vs valeurs ajustées")
abline(h = 0, lty = 2, col = "blue")
lines(lowess(fitted_vals, resid_std), col = "red", lwd = 2)






plot(seq_along(resid_std), resid_std,
     xlab = "Index des observations",
     ylab = "Résidus de déviance standardisés",
     main = "Résidus vs ordre des observations",
     col = "gray")
abline(h = 0, lty = 2, col = "blue")
lines(lowess(seq_along(resid_std), resid_std), col = "red", lwd = 2)





vars_cont <- c("Nuits_Weekend",
               "Delai_Reservation",
               "Nb_Annulations",
               "Prix_Moyen",
               "Demandes_Speciales")

par(mfrow = c(3,2))
for (v in vars_cont) {
  plot(data[[v]], resid_std,
       xlab = v,
       ylab = "Résidus de déviance standardisés",
       main = paste("Résidus vs", v))
  abline(h = 0, lty = 2, col = "blue")
  lines(lowess(data[[v]], resid_std), col = "red", lwd = 2)
}
par(mfrow = c(1,3))





cooks <- cooks.distance(model)

plot(cooks, type = "h",
     main = "Cook's distance",
     ylab = "Cook's distance")
abline(h = 4/length(cooks), col = "red", lty = 2)


hat_vals <- hatvalues(model)

plot(hat_vals, type = "h",
     main = "Leverage (hat values)",
     ylab = "Leverage")
abline(h = 2*mean(hat_vals), col = "red", lty = 2)



dff <- dffits(model)

plot(dff, type = "h",
     main = "DFFITS",
     ylab = "DFFITS")
abline(h = 2*sqrt(length(coef(model))/nrow(data)),
       col = "red", lty = 2)








high_influence <- which(cooksd > 2*mean(cooksd, na.rm=TRUE))
data_sans_influence <- data[-high_influence, ]
summary(data_sans_influence)



model_bin_sans <- glm(Y ~ Nb_Adultes + Nb_Enfants + Nuits_Weekend + Nuits_Semaine +
                        Type_Chambre + Segment + Recurrent + Mois,
                      family = binomial, data = data_sans_influence)



coef(model)
coef(model_bin_sans)

comparison <- cbind(
  Complet = coef(model),
  Sans_Influents = coef(model_bin_sans)
)

comparison







fitted_complet <- fitted(model)
fitted_sans <- fitted(model_bin_sans)

summary(fitted_complet)
summary(fitted_sans)


par(mfrow = c(1,2))
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

vif_values <- vif(model_bin_sans)
vif_values


# -------------------------------
# 10. Pseudo R2 (McFadden)
# -------------------------------
null_dev <- model$null.deviance
res_dev  <- model$deviance
pseudoR2 <- 1 - res_dev/null_dev
pseudoR2












