# -----------------------------
# Load required libraries
# -----------------------------
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)

# -----------------------------
# Load datasets
# -----------------------------
players <- read.csv("players.csv")
sessions <- read.csv("sessions.csv")

# -----------------------------
# Data wrangling
# -----------------------------

# Convert signup_date to Date type
players$signup_date <- as.Date(players$signup_date)

# Create experience groups
players <- players %>%
  mutate(
    experience_group = case_when(
      signup_date < as.Date("2023-01-01") ~ "Veteran",
      signup_date >= as.Date("2023-01-01") & signup_date <= as.Date("2023-12-31") ~ "Intermediate",
      signup_date > as.Date("2023-12-31") ~ "New",
      TRUE ~ NA_character_
    )
  )


