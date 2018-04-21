library(RSQLite)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)

con <- dbConnect(drv =RSQLite::SQLite(), dbname="./draftkings/statcast.db")

statcast_df <- dbGetQuery(con, "Select * from statcast")


# Wondering why Kipnis can't get a hit this year...
kipnis <- statcast_df %>%
  filter(game_year > 2014) %>%
  filter(player_name == "Jason Kipnis") %>%
  select(launch_angle, launch_speed, description, babip_value, game_year) %>%
  filter(!is.na(launch_speed)) %>%
  filter(!(description == "foul"| description == "ball" | description == "blocked_ball" | 
             description == "called_strike" | description == "foul_bunt" | 
             description == "hit_by_pitch"| description == "swinging_strike"  ))

kip_med <- kipnis %>%
  group_by(game_year) %>%
  summarise(med_speed = median(launch_speed), med_angle = median(launch_angle))
  

ggplot(kipnis, aes(x = launch_speed, y = launch_angle)) +
  geom_point(aes(col = description)) +
  geom_smooth(method = "lm", se = F, aes(col = description)) +
  facet_grid(~game_year) +
  geom_vline(aes(xintercept = med_speed), kip_med) +
  geom_text(aes(med_speed+10,-50, label = med_speed), kip_med) +
  geom_hline(aes(yintercept = med_angle), kip_med) +
  geom_text(aes(40,med_angle + 5, label = med_angle), kip_med)

kipnis <- kipnis %>%
  mutate(hard_hit = ifelse(launch_speed > 95, T, F))

kip_hard <- kipnis %>%
  filter(hard_hit == T) %>%
  group_by(game_year) %>%
  summarise(hard_avg = sum(as.numeric(babip_value)))

ggplot(kip_hard, aes(x = game_year, y = hard_avg)) +
  geom_bar(stat = "identity") +
  ggtitle("Kipnis avg on balls hit harder than 95 mph")


