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

# -----------------------------
# Analyse behaviour of top players
# -----------------------------
top_player_stats <- sessions %>%
  filter(player_id %in% top_players$player_id) %>%
  group_by(player_id) %>%
  summarise(
    total_sessions = n(),
    avg_play_time = mean(play_time_minutes, na.rm = TRUE),
    avg_score = mean(score, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_play_time))

# -----------------------------
# Display table using kable
# -----------------------------
top_player_stats %>%
  kbl(caption = "Top 10 Players and Their Behaviour") %>%
  kable_classic(full_width = FALSE)

# -----------------------------
# Visualise score distribution using boxplot
# -----------------------------
ggplot(sessions %>% filter(player_id %in% top_players$player_id),
       aes(x = factor(player_id), y = score, fill = factor(player_id))) +
  geom_boxplot() +
  labs(
    title = "Score Distribution of Top 10 Players",
    x = "Player ID",
    y = "Score"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
