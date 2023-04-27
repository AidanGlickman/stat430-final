library(tidyverse)

sc <- read.csv('data/statcast.csv')

sc_selected <- sc |> select(pitch_type, zone, type, bb_type, effective_speed, pitcher, batter, bat_score, fld_score, balls, strikes, outs_when_up, inning, inning_topbot, events)

write.csv(sc_selected, 'data/sc_selected.csv')

sc_down <- sc_selected |>
  mutate(fld_winning = (fld_score > bat_score),
         pitcher = as.integer(pitcher)) |>
  group_by(pitcher, fld_winning, inning, inning_topbot, outs_when_up, balls, strikes)

pitcher_pitchtypes <- sc_down |>
  drop_na(pitch_type) |>
  count(pitch_type)
  