# -----------------------------------------------------------
# PART 2: Game Genre Analysis
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr)
library(kableExtra)
library(ggplot2)

# -----------------------------
# Load datasets
# -----------------------------
games <- read.csv("games.csv")
sessions <- read.csv("sessions.csv")

# -----------------------------
# Merge sessions with games
# -----------------------------
session_genre <- sessions %>%
  left_join(games, by = "game_id")

