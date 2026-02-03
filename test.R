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

