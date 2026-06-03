# -----------------------------------------------------------
# PART 4: Age Group Behaviour Comparison
# COMP1013 Analytics Programming - Assignment
# Author: Mitansh Buwa
# -----------------------------------------------------------

library(dplyr) # Importing the library
library(kableExtra) # Importing the library
library(ggplot2)# Improting the library

# -----------------------------
# Load datasets
# -----------------------------
players <- read.csv("players.csv") # Loading the dataset
sessions <- read.csv("sessions.csv") # Loading the dataset

# -----------------------------
# Handle NA values
# -----------------------------
players <- players %>% filter(!is.na(age)) # Reducing the redundant data
sessions <- sessions %>% filter(!is.na(play_time_minutes), !is.na(score)) # For the play time & For the score

# -----------------------------
# Create age groups
# -----------------------------
players <- players %>% # Creating the specific age groupd
  mutate(
    age_group = case_when(
      age >= 16 & age <= 25 ~ "16-25", # For 16 to 25
      age >= 26 & age <= 35 ~ "26-35", # For 26 to 35
      age >= 36 & age <= 45 ~ "36-45", # For 36 - 45
      age >= 46 ~ "46+", # For anyone above 46
      TRUE ~ NA_character_
    )
  )

  
# -----------------------------
# Merge players with sessions
# -----------------------------
age_sessions <- sessions %>% # Merging the dataset
  left_join(players, by = "player_id") %>% # Using the player_id
  filter(!is.na(age_group))

# -----------------------------
# Compute summary statistics
# -----------------------------
age_summary <- age_sessions %>% # Calculating the data
  group_by(age_group) %>%
  summarise(
    avg_play_time = mean(play_time_minutes, na.rm = TRUE), # Average
    avg_score = mean(score, na.rm = TRUE), # Average
    number_of_sessions = n()
  ) %>%
  arrange(age_group)

# -----------------------------
# Display table using kable
# -----------------------------
age_summary %>% # Visulation of the data in table
  kbl(caption = "Age Group Behaviour Summary") %>%
  kable_classic(full_width = FALSE)

# -----------------------------
# Visualise Average Play Time by Age Group
# -----------------------------
ggplot(age_summary,
       aes(x = age_group, y = avg_play_time, fill = age_group)) +
  geom_col() +
  labs(
    title = "Average Play Time by Age Group",
    x = "Age Group",
    y = "Average Play Time (minutes)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") # Visulation of the data format



#Completed & Tested - Final Version ready to submit
