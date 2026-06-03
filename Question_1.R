# -----------------------------------------------------------
# PART 1: Player Engagement Analysis by Experience Level
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

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
# Convert signup_date to Date
# -----------------------------
players$signup_date <- as.Date(players$signup_date)

# -----------------------------
# Create experience groups
# -----------------------------
players <- players %>%
  mutate(
    experience_group = case_when(
      signup_date < as.Date("2023-01-01") ~ "Veteran",
      signup_date >= as.Date("2023-01-01") & signup_date <= as.Date("2023-12-31") ~ "Intermediate",
      signup_date > as.Date("2023-12-31") ~ "New",
      TRUE ~ NA_character_
    )
  )

# -----------------------------
# Merge players with sessions
# -----------------------------
player_sessions <- sessions %>%
  left_join(players, by = "player_id")

# -----------------------------
# Handle NA values
# Remove rows with missing play_time, score, or experience group
# -----------------------------
player_sessions <- player_sessions %>%
  filter(!is.na(play_time_minutes),
         !is.na(score),
         !is.na(experience_group))

# -----------------------------
# Compute summary statistics
# -----------------------------
engagement_summary <- player_sessions %>%
  group_by(experience_group) %>%
  summarise(
    number_of_players = n_distinct(player_id),
    avg_play_time = mean(play_time_minutes, na.rm = TRUE),
    avg_score = mean(score, na.rm = TRUE)
  ) %>%
  arrange(experience_group)

# -----------------------------
# Display table using kable
# -----------------------------
engagement_summary %>%
  kbl(caption = "Player Engagement by Experience Level") %>%
  kable_classic(full_width = FALSE)

# -----------------------------
# Visualise Average Play Time
# -----------------------------
ggplot(engagement_summary,
       aes(x = experience_group, y = avg_play_time, fill = experience_group)) +
  geom_col() +
  labs(
    title = "Average Play Time by Experience Level",
    x = "Experience Group",
    y = "Average Play Time (minutes)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

  //Completed & Tested

