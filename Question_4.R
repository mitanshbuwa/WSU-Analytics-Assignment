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

  
# -----------------------------
# Merge players with sessions
# -----------------------------
age_sessions <- sessions %>%
  left_join(players, by = "player_id") %>%
  filter(!is.na(age_group))

# -----------------------------
# Compute summary statistics
# -----------------------------
age_summary <- age_sessions %>%
  group_by(age_group) %>%
  summarise(
    avg_play_time = mean(play_time_minutes, na.rm = TRUE),
    avg_score = mean(score, na.rm = TRUE),
    number_of_sessions = n()
  ) %>%
  arrange(age_group)

# -----------------------------
# Display table using kable
# -----------------------------
age_summary %>%
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
  theme(legend.position = "none")



// Completed & Tested
