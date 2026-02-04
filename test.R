
# Nettoyage de l'environnement

rm(list = ls())


# Chargement des librairies

library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(ggcorrplot)



# Importation des données

data <- read_excel(file.choose())


#  Aperçu général

str(data)
summary(data)


# Renommage des variables

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


# Variable binaire d'annulation

data$is_canceled <- ifelse(data$reservation_status == "Canceled", 1, 0)


# Tableaux de fréquences

table(data$reservation_status)
table(data$is_canceled)
prop.table(table(data$is_canceled))


# VISUALISATIONS ORIGINALES (CONSERVÉES)


# Barplot nombre d'observations
ggplot(data, aes(x = factor(is_canceled))) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Répartition des annulations de réservation",
    x = "Statut (0 = Non annulée, 1 = Annulée)",
    y = "Nombre de réservations"
  )


# Barplot pourcentage
ggplot(data, aes(x = factor(is_canceled), y = after_stat(prop), group = 1)) +
  geom_bar(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion des annulations de réservation",
    x = "Statut",
    y = "Proportion"
  )



# Conversion en facteurs

data <- data %>%
  mutate(
    meal_plan = as.factor(meal_plan),
    room_type = as.factor(room_type),
    market_segment = as.factor(market_segment),
    reservation_status = as.factor(reservation_status)
  )



# Graphes EDA du délai 



# Histogramme
ggplot(data, aes(x = lead_time)) +
  geom_histogram(bins = 50, fill = "darkorange", color = "black") +
  labs(
    title = "Distribution du délai de réservation",
    x = "Délai (jours)",
    y = "Fréquence"
  )



# Boxplot outliers
ggplot(data, aes(y = lead_time)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Boxplot du délai de réservation",
    y = "Délai (jours)"
  )



# Moyenne vs médiane
mean(data$lead_time, na.rm = TRUE)
median(data$lead_time, na.rm = TRUE)


# Densité
ggplot(data, aes(x = lead_time)) +
  geom_density(fill = "orange", alpha = 0.4) +
  labs(
    title = "Densité du délai de réservation",
    x = "Délai (jours)",
    y = "Densité"
  )


# Boxplot délai selon statut
ggplot(data, aes(x = factor(is_canceled), y = lead_time)) +
  geom_boxplot(fill = c("lightgreen", "salmon")) +
  labs(
    title = "Délai de réservation selon le statut",
    x = "Annulation",
    y = "Délai (jours)"
  )





# Test de Welch


# Statistiques descriptives
data %>%
  group_by(is_canceled) %>%
  summarise(
    moyenne = mean(lead_time),
    mediane = median(lead_time),
    ecart_type = sd(lead_time)
  )

# Test de Welch
t.test(
  lead_time ~ is_canceled,
  data,
  var.equal = FALSE
)


