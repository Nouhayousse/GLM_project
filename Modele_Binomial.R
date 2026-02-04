library(readxl)
library(tidyverse)
library(corrplot) # Pour les matrices de corrélation
library(caret)    # Pour la division train/test et matrice de confusion
library(pROC)     # Pour la courbe ROC


# Liste des feuilles
excel_sheets("Projet_GLM_IDSI2.xlsx")

# Charger une seule feuille
data <- read_excel("Projet_GLM_IDSI2.xlsx", sheet = "G4")
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

model_bin <- glm(
  Y ~ Nb_Adultes + Nb_Enfants + Nuits_Weekend + Nuits_Semaine +
    Type_Chambre + Segment + Recurrent + Mois,
  data = data,
  family = binomial
)

summary(model_bin)
exp(coef(model_bin))
summary(model_bin)$coefficients


anova(glm(Y ~ 1, family = binomial, data = data),
      model_bin,
      test = "Chisq")

exp(coef(model_bin))
summary(model_bin)$coefficients


#pseudo R2
library(performance)
r2(model_bin)

library(pscl)
round(pR2(model_bin),3)
step(model_bin, direction = "backward", k = 2)
step(model_bin, direction = "backward", k = log(nrow(data)))
#roc / auc/suille optimal 

data$proba_pred <- predict(model_bin, type = "response")

library(pROC)
roc_obj <- roc(data$Y, data$proba_pred)
plot(roc_obj, legacy.axes = TRUE)
auc(roc_obj)

coords(roc_obj,
       x = "best",
       best.method = "youden",
       ret = c("threshold", "sensitivity", "specificity"))


#matrice de confusion 
s <- 0.3605458  #seuil touver 
  pred <- ifelse(data$proba_pred >= s, 1, 0)

table(Observé = data$Y,
      Prédit = pred)

rps = rstandard(model_bin)
plot(fitted(model_bin), rps)


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


