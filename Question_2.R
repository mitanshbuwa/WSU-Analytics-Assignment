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


# -----------------------------
# Handle NA values
# Remove rows with missing play_time, score, or genre
# -----------------------------
session_genre <- session_genre %>%
  filter(!is.na(play_time_minutes),
         !is.na(score),
         !is.na(genre))

# -----------------------------
# Compute summary statistics by genre
# -----------------------------
genre_summary <- session_genre %>%
  group_by(genre) %>%
  summarise(
    avg_play_time = mean(play_time_minutes, na.rm = TRUE),
    avg_score = mean(score, na.rm = TRUE),
    number_of_sessions = n(),
    number_of_unique_players = n_distinct(player_id)
  ) %>%
  arrange(desc(number_of_unique_players))

  # -----------------------------
# Display table using kable
# -----------------------------
genre_summary %>%
  kbl(caption = "Game Genre Engagement Summary") %>%
  kable_classic(full_width = FALSE)

# -----------------------------
# Visualise Number of Unique Players by Genre
# -----------------------------
ggplot(genre_summary,
       aes(x = reorder(genre, -number_of_unique_players),
           y = number_of_unique_players,
           fill = genre)) +
  geom_col() +
  labs(
    title = "Number of Unique Players by Game Genre",
    x = "Genre",
    y = "Number of Unique Players"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

  
  //Completed & Tested


