# -----------------------------------------------------------
# PART 4: Age Group Behaviour Comparison
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr)
library(kableExtra)
library(ggplot2)

# -----------------------------
# Load datasets
# -----------------------------
players <- read.csv("players.csv")
sessions <- read.csv("sessions.csv")

# -----------------------------
# Handle NA values
# -----------------------------
players <- players %>% filter(!is.na(age))
sessions <- sessions %>% filter(!is.na(play_time_minutes), !is.na(score))

# -----------------------------
# Create age groups
# -----------------------------
players <- players %>%
  mutate(
    age_group = case_when(
      age >= 16 & age <= 25 ~ "16-25",
      age >= 26 & age <= 35 ~ "26-35",
      age >= 36 & age <= 45 ~ "36-45",
      age >= 46 ~ "46+",
      TRUE ~ NA_character_
    )
  )
