library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
library(dplyr)

# Problem parameters
num_teams <- 16   # Number of teams
num_days <- 2     # Number of days (1 for midweek, 2 for weekend)
num_weeks <- 6    # Number of weeks
num_games_home <- 6
num_games_away <- 6
num_midweek_games_per_week <- 1
num_weekend_games_per_week <- 1

coefficients <- matrix(c(
  0, 4, 10, 6, 7, 8, 4, 2, 2, 5, 5, 2, 5, 3, 5, 2, 
  3, 0, 6, 7, 9, 4, 9, 8, 8, 9, 5, 4, 5, 4, 7, 4, 
  9, 7, 0, 10, 8, 5, 5, 4, 4, 6, 4, 2, 6, 2, 5, 2,
  7, 7, 10, 0, 8, 6, 6, 6, 5, 5, 4, 3, 5, 3, 4, 3,
  6, 9, 10, 8, 0, 6, 7, 7, 4, 5, 7, 6, 3, 4, 3, 4, 
  10, 6, 7, 6, 7, 0, 4, 3, 3, 3, 5, 5, 5, 6, 6, 5,
  3, 8, 5, 4, 8, 4, 0, 10, 6, 7, 4, 7, 4, 5, 3, 4,
  2, 8, 4, 5, 8, 5, 10, 0, 7, 8, 6, 7, 4, 7, 5, 5,
  3, 7, 3, 2, 6, 4, 7, 7, 0, 10, 6, 5, 9, 5, 8, 6,
  2, 7, 5, 4, 4, 3, 6, 6, 9, 0, 9, 4, 5, 4, 7, 5,
  4, 4, 3, 3, 6, 4, 5, 5, 5, 8, 0, 8, 8, 9, 9, 7,
  2, 3, 2, 2, 7, 5, 7, 5, 4, 4, 10, 0, 6, 9, 4, 6,
  2, 4, 5, 3, 4, 4, 5, 5, 9, 7, 8, 5, 0, 6, 9, 8,
  3, 4, 3, 3, 4, 5, 5, 5, 5, 5, 9, 8, 7, 0, 5, 6,
  3, 7, 4, 3, 4, 4, 5, 4, 7, 7, 6, 5, 10, 5, 0, 6,
  2, 4, 3, 3, 5, 4, 4, 4, 5, 5, 6, 5, 10, 6, 7, 0
), byrow = TRUE, nrow = 16)

division_teams <- list(
  1:4,    # Division 1 teams
  5:8,    # Division 2 teams
  9:12,   # Division 3 teams
  13:16   # Division 4 teams
)

get_non_divisional_teams <- function(team, division_teams) {
  # Find which division the team belongs to
  division <- which(sapply(division_teams, function(x) team %in% x))
  # Return all teams not in that division
  return(setdiff(1:num_teams, division_teams[[division]]))
}

# Initialize the model
model <- MIPModel()

# Add decision variables
model <- model %>%
  add_variable(x[i, j, k, l], i = 1:num_teams, j = 1:num_teams, k = 1:num_days, l = 1:num_weeks, type = "binary")

# Objective function
model <- model %>%
  set_objective(sum_over(coefficients[i, j] * x[i, j, k, l], i = 1:num_teams, j = 1:num_teams, k = 1:num_days, l = 1:num_weeks), "max")

# No team can play themselves
for (i in 1:num_teams) {
  model <- model %>%
    add_constraint(sum_over(x[i, i, k, l], k = 1:num_days, l = 1:num_weeks) == 0)
}

# Each team plays 6 home and 6 away games
for (i in 1:num_teams) {
  model <- model %>%
    add_constraint(sum_over(x[i, j, k, l], j = 1:num_teams, k = 1:num_days, l = 1:num_weeks) == num_games_home) %>%
    add_constraint(sum_over(x[j, i, k, l], j = 1:num_teams, k = 1:num_days, l = 1:num_weeks) == num_games_away)
}

# Each team plays 6 mid-week and 6 weekend games
for (i in 1:num_teams) {
  model <- model %>%
    add_constraint(sum_over(x[i, j, 1, l] + x[j, i, 1, l], j = 1:num_teams, l = 1:num_weeks) == 6) %>%
    add_constraint(sum_over(x[i, j, 2, l] + x[j, i, 2, l], j = 1:num_teams, l = 1:num_weeks) == 6)
}

# Each team plays every team in its division exactly once at home, and once away
for (division in 1:length(division_teams)) {
  for (i in division_teams[[division]]) {
    for (j in division_teams[[division]]) {
      if (i != j) { # Ensure we don't match a team with itself
        model <- model %>%
          add_constraint(sum_over(x[i, j, k, l], k = 1:num_days, l = 1:num_weeks) == 1) %>% # Team i plays team j at home once
          add_constraint(sum_over(x[j, i, k, l], k = 1:num_days, l = 1:num_weeks) == 1) # Team i plays team j away once
      }
    }
  }
}

# Each team plays six games against non-divisional teams
for (i in 1:num_teams) {
  non_divisional_teams <- get_non_divisional_teams(i, division_teams)
  model <- model %>%
    add_constraint(sum_over(x[i, j, k, l], j = non_divisional_teams, k = 1:num_days, l = 1:num_weeks) == 3) %>%
    add_constraint(sum_over(x[j, i, k, l], j = non_divisional_teams, k = 1:num_days, l = 1:num_weeks) == 3)
}

# Constraint for one week-day game per week
for (i in 1:num_teams) {
  for (l in 1:num_weeks) {
    model <- model %>%
      add_constraint(sum_over(x[i, j, 1, l], j = 1:num_teams) + 
                       sum_over(x[j, i, 1, l], j = 1:num_teams) == num_midweek_games_per_week)
  }
}

# Constraint for one weekend game per week
for (i in 1:num_teams) {
  for (l in 1:num_weeks) {
    model <- model %>%
      add_constraint(sum_over(x[i, j, 2, l], j = 1:num_teams) + 
                       sum_over(x[j, i, 2, l], j = 1:num_teams) == num_weekend_games_per_week)
  }
}

# Each team cannot play a non-divisional team more than once
for (i in 1:num_teams) {
  non_divisional_teams <- get_non_divisional_teams(i, division_teams)
  for (j in non_divisional_teams) {
    model <- model %>%
      add_constraint(sum_over(x[i, j, k, l], k = 1:num_days, l=1:num_weeks) + 
                       sum_over(x[j, i, k, l], k = 1:num_days, l=1:num_weeks) <= 1)
  }
}

result <- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE))

solution <- result %>%
  get_solution(x[i, j, k, l]) %>%
  filter(value > 0) %>%
  select(i, j, k, l)

objective_value <- objective_value(result)
cat('Objective function value: ', objective_value, "\n")

solution <- solution %>% mutate(i = case_when(i == 1 ~ "Oklahoma", i == 2 ~ "LSU", i == 3 ~ "Texas", i == 4 ~ "Texas A&M", 
                                              i == 5 ~ "Arkansas", i == 6 ~ "Missouri", i == 7 ~ "Mississippi", i == 8 ~ "Mississippi State", 
                                              i == 9 ~ "Auburn", i == 10 ~ "Alabama", i == 11 ~ "Tennessee", i == 12 ~ "Vanderbilt", 
                                              i == 13 ~ "Georgia", i == 14 ~ "Kentucky", i == 15 ~ "Florida", i == 16 ~ "South Carolina"),
                                j = case_when(j == 1 ~ "Oklahoma", j == 2 ~ "LSU", j == 3 ~ "Texas", j == 4 ~ "Texas A&M", 
                                              j == 5 ~ "Arkansas", j == 6 ~ "Missouri", j == 7 ~ "Mississippi", j == 8 ~ "Mississippi State", 
                                              j == 9 ~ "Auburn", j == 10 ~ "Alabama", j == 11 ~ "Tennessee", j == 12 ~ "Vanderbilt", 
                                              j == 13 ~ "Georgia", j == 14 ~ "Kentucky", j == 15 ~ "Florida", j == 16 ~ "South Carolina"))

# Southeast Division Schedules
floridaSchedule <- solution %>% filter(i == "Florida" | j == "Florida")
georgiaSchedule <- solution %>% filter(i == "Georgia" | j == "Georgia")
uscSchedule <- solution %>% filter(i == "South Carolina" | j == "South Carolina")
kentuckySchedule <- solution %>% filter(i == "Kentucky" | j == "Kentucky")

# Central Division Schedules
auburnSchedule <- solution %>% filter(i == "Auburn" | j == "Auburn")
alabamaSchedule <- solution %>% filter(i == "Alabama" | j == "Alabama")
tennesseeSchedule <- solution %>% filter(i == "Tennessee" | j == "Tennessee")
vandySchedule <- solution %>% filter(i == "Vanderbilt" | j == "Vanderbilt")

# Southwest Division Schedules
arkansasSchedule <- solution %>% filter(i == "Arkansas" | j == "Arkansas")
missouriSchedule <- solution %>% filter(i == "Missouri" | j == "Missouri")
oleMissSchedule <- solution %>% filter(i == "Mississippi" | j == "Mississippi")
mistStSchedule <- solution %>% filter(i == "Mississippi State" | j == "Mississippi State")

# West Division Schedules
oklahomaSchedule <- solution %>% filter(i == "Oklahoma" | j == "Oklahoma")
lsuSchedule <- solution %>% filter(i == "LSU" | j == "LSU")
texasSchedule <- solution %>% filter(i == "Texas" | j == "Texas")
aggiesSchedule <- solution %>% filter(i == "Texas A&M" | j == "Texas A&M")


solution
