# -----------------------------------------------------------
# PART 2: Game Genre Analysis
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr) # Importing the library
library(kableExtra) # Importing the library
library(ggplot2) # Importing the library

# -----------------------------
# Load datasets
# -----------------------------
games <- read.csv("games.csv") # Loading the games dataset
sessions <- read.csv("sessions.csv") # Loading the game dataset

# -----------------------------
# Merge sessions with games
# -----------------------------
session_genre <- sessions %>% #Combining the dataset
  left_join(games, by = "game_id") # Fetching the game id


# -----------------------------
# Handle NA values
# Remove rows with missing play_time, score, or genre
# -----------------------------
session_genre <- session_genre %>% # Reducing the redundant data
  filter(!is.na(play_time_minutes), # For play time
         !is.na(score), # For the score
         !is.na(genre)) # For the genre

# -----------------------------
# Compute summary statistics by genre
# -----------------------------
genre_summary <- session_genre %>% # Calculating the data
  group_by(genre) %>%
  summarise(
    avg_play_time = mean(play_time_minutes, na.rm = TRUE), #Average
    avg_score = mean(score, na.rm = TRUE), #Average
    number_of_sessions = n(),
    number_of_unique_players = n_distinct(player_id)
  ) %>%
  arrange(desc(number_of_unique_players))

  # -----------------------------
# Display table using kable
# -----------------------------
genre_summary %>% #Visulation of the genre summary in table
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
  theme(legend.position = "none") # Visulation of the data 

#Completed & Tested - Final Version ready to submit


