# Data source
# https://www.kaggle.com/datasets/tobycrabtree/nfl-scores-and-betting-data
library(tidyverse)
library(arrow)

# Read and process ####
groups <- c("home", "away")
week_names  <- c(str_pad(as.character(1:18), width = 2, side = "left", pad = "0"),
                 "P1W", "P2D", "P3C", "SB")
  

team_names <- read_csv("nfl_teams.csv")
nfl_data_raw <- read_csv("spreadspoke_scores.csv")

nfl_data <- 
  nfl_data_raw %>% 
  select(1:8) %>% 
  mutate(winner = ifelse(score_home > score_away, "home", "away"),
         point_dif = abs(score_home - score_away),
         schedule_week = case_when(
           schedule_week %in% c("Wildcard", "WildCard") ~ "P1W",
           schedule_week == "Division" ~ "P2D",
           schedule_week == "Conference" ~ "P3C",
           schedule_week %in% c("Superbowl", "SuperBowl") ~ "SB",
           is.character(schedule_week) ~ schedule_week
         ),
         schedule_week = str_pad(schedule_week, width = 2, side = "left", 
                                 pad = "0")
         )

nfl_df <- 
  map_df(groups, function(x) {
    nfl_data %>% 
      select(
        matches("schedule"), 
        "winner",
        "team"  = matches(paste0("team_", x)), 
        "score" = matches(paste0("score_", x)),
        "point_dif"
      ) %>% 
      mutate(status = x)
  })

nfl_scores_no_id <- 
  nfl_df %>% 
  mutate(
    victory = ifelse(winner == status, TRUE, FALSE),
    point_dif = ifelse(victory, point_dif, -point_dif),
    schedule_week = factor(schedule_week, ordered = TRUE,
                           levels = week_names)
  ) %>% 
  arrange(schedule_season, schedule_week) %>% 
  group_by(schedule_season, team) %>% 
  mutate(cumulative = cumsum(victory),
         status = ifelse(status == "away", "Away", "Home")) %>% 
  ungroup()

nfl_scores <-  
  left_join(nfl_scores_no_id, 
            select(team_names, "team" = "team_name", "team_id", 
                   "team_division", "team_conference"), 
            by = c("team")) %>% 
  mutate(
    decade = floor(schedule_season / 10) * 10,
    year = as.numeric(substr(schedule_season, 4, 4))
  )


# Plot funs ####
plot_decades <- function(team_sel, scores = nfl_scores) {
  scores_sel <- 
    scores %>% 
    filter(str_detect(team, team_sel)) %>% 
    mutate(
      decade = floor(schedule_season / 10) * 10,
      year = as.numeric(substr(schedule_season, 4, 4))
    ) %>% 
    na.omit()
  
  team_title <- paste(unique(scores_sel$team), collapse = "/")
  
  ggplot(scores_sel) +
    aes(schedule_week, cumulative, color = year)  +
    geom_line(aes(group = schedule_season)) +
    geom_point() +
    scale_color_gradient(name = "Year of\ndecade", 
                         low = "#7b3294", high = "#7fbf7b", breaks = 0:10) +
    scale_y_continuous(breaks = seq(0, 16, by = 4)) +
    facet_wrap("decade") +
    theme_minimal() +
    theme(
      panel.grid.minor.x = element_blank()
    ) + 
    labs(x = "Week", y = "Wins", title = team_title,
         subtitle = "Cumulative wins per week by decade")  
}


plot_win_ratio <- function(team_sel, scores = nfl_scores) {
  scores_sel <- 
    scores %>% 
    filter(str_detect(team, team_sel)) %>% 
    group_by(team, schedule_season) %>% 
    summarise(win_rate = mean(victory, na.rm = T)) 
  
  team_title <- paste(unique(scores_sel$team), collapse = "/")
  
  ggplot(scores_sel) +
    aes(schedule_season, win_rate) +
    geom_hline(yintercept = .5, lty = 2, color = "#aa00aa", size = .5) +
    geom_point(alpha = .3, color = "#aa00aa") +
    geom_smooth(alpha = .1, color = "#7b3294", 
                method = "loess", formula = "y~x") +
    scale_x_continuous(breaks = 1966:2022) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = .6),
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = "Season", y = "Win ratio",
         title = team_title,
         subtitle = "Win ratio by season")
}


plot_win_ratio_status <- function(team_sel, scores = nfl_scores) {
  scores_sel <- 
    scores %>% 
    filter(str_detect(team, team_sel)) %>% 
    group_by(team, schedule_season, status) %>% 
    summarise(win_rate = mean(victory, na.rm = T))
  
  team_title <- paste(unique(scores_sel$team), collapse = "/")
  
  ggplot(scores_sel) +
    aes(schedule_season, win_rate, color = status, fill = status) +
    geom_hline(yintercept = .5, lty = 2, color = "#aa00aa", size = .5) +
    geom_point(alpha = .3) +
    geom_smooth(alpha = .1, 
                method = "loess", formula = "y~x") +
    scale_color_manual(name = "Status", 
                       values = c("#7b3294", "#7fbf7b")) +
    scale_fill_manual(name = "Status", values = c("#7b3294", "#7fbf7b")) +
    scale_x_continuous(breaks = 1966:2022) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = .6),
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = "Season", y = "Win ratio",
         title = team_title,
         subtitle = "Win ratio by season and Home/Away status")
}


plot_pointdif <- function(team_sel, scores = nfl_scores) {
  scores_sel <- 
    scores %>% 
    filter(str_detect(team, team_sel)) 
  
  team_title <- paste(unique(scores_sel$team), collapse = "/")
  
  ggplot(scores_sel) +
    aes(factor(schedule_season), point_dif) +
    geom_boxplot(alpha = .5, color = "#7fbf7b") +
    geom_hline(yintercept = 0, lty = 2, color = "#aa00aa", size = .5) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = .6),
      panel.grid.major.x = element_blank()
    ) + 
    labs(x = "Season", y = "Point difference",
         title = team_title,
         subtitle = "Spread of point difference per game by season")
}


# Test ####
my_team <- "Oilers|Titans"
plot_decades(my_team)
plot_win_ratio(my_team)
plot_win_ratio_status(my_team)
plot_pointdif(my_team)


# Exports ####
write_csv(nfl_scores, "nfl_scores.csv")
write_parquet(nfl_scores, "nfl_scores.parquet")
