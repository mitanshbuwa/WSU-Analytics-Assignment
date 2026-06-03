# -----------------------------------------------------------
# PART 1: Player Engagement Analysis by Experience Level
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr) #importing the library
library(lubridate) #importing the library
library(kableExtra) #importing the library
library(ggplot2) #importing the library

# -----------------------------
# Load datasets
# -----------------------------
players <- read.csv("players.csv") #Loading the player dataset
sessions <- read.csv("sessions.csv") #Loading the sessions dataset

# -----------------------------
# Convert signup_date to Date
# -----------------------------
players$signup_date <- as.Date(players$signup_date) #converting the data

# -----------------------------
# Create experience groups
# -----------------------------
players <- players %>% #Expereince group creation
  mutate(
    experience_group = case_when(
      signup_date < as.Date("2023-01-01") ~ "Veteran", #Veterans group creation
      signup_date >= as.Date("2023-01-01") & signup_date <= as.Date("2023-12-31") ~ "Intermediate", #Intermediate group creation
      signup_date > as.Date("2023-12-31") ~ "New", #New group creation
      TRUE ~ NA_character_
    )
  )

# -----------------------------
# Merge players with sessions
# -----------------------------
player_sessions <- sessions %>% #Merging the dataset
  left_join(players, by = "player_id") #Fetching playerid dataset

# -----------------------------
# Handle NA values
# Remove rows with missing play_time, score, or experience group
# -----------------------------
player_sessions <- player_sessions %>% #Reducing the redundant data
  filter(!is.na(play_time_minutes), # For play time
         !is.na(score), # For the score
         !is.na(experience_group)) #for the experience group

# -----------------------------
# Compute summary statistics
# -----------------------------
engagement_summary <- player_sessions %>% #Calculating the data sort of
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
engagement_summary %>% #Visualation of the data in table
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
  theme(legend.position = "none") #Visulation of the data for the average time played

#Completed & Tested - Final Version ready to submit

