# -----------------------------------------------------------
# PART 3: Top Players and Their Behaviour
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr)
library(kableExtra)
library(ggplot2)

# -----------------------------
# Load dataset
# -----------------------------
sessions <- read.csv("sessions.csv")

# -----------------------------
# Handle NA values
# -----------------------------
sessions <- sessions %>%
  filter(!is.na(play_time_minutes), !is.na(score))

# -----------------------------
# Identify top 10 players by total play time
# -----------------------------
top_players <- sessions %>%
  group_by(player_id) %>%
  summarise(total_play_time = sum(play_time_minutes, na.rm = TRUE)) %>%
  arrange(desc(total_play_time)) %>%
  slice_head(n = 10)


