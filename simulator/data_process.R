library(tidyverse)

sc <- read.csv("data/statcast.csv")

sc_selected <- sc |>
  select(pitch_type, zone, type, bb_type, effective_speed, pitcher, batter, bat_score, fld_score, balls, strikes, outs_when_up, inning, inning_topbot, events) |>
  mutate(
    fld_winning = (fld_score > bat_score),
    pitcher = as.integer(pitcher)
  )

write.csv(sc_selected, "data/sc_selected.csv")

pitcher_pitchtypes <- sc_selected |>
  # make a column for each pitch type
  mutate(pitch_type = as.character(pitch_type)) |>
  group_by(pitcher, fld_winning, inning, inning_topbot, outs_when_up, balls, strikes) |>
  drop_na() |>
  count(pitch_type) |>
  pivot_wider(names_from = pitch_type, names_prefix = "n_", values_from = n) |>
  # fill in missing values for columns starting with n_
  mutate(across(starts_with("n_"), ~ ifelse(is.na(.x), 0, .x))) |>
  # make a variable n_total that is the total of all columns prefixed with n_
  mutate(n_total = rowSums(across(starts_with("n_"))))

write.csv(pitcher_pitchtypes, "data/pitcher_pitchtypes.csv")

# assuming a normal distribution for pitch speed by type, independent of other factors
pitcher_speed <- sc_selected |>
  group_by(pitcher, pitch_type) |>
  drop_na(pitch_type, effective_speed) |>
  summarize(
    speed_mean = mean(effective_speed),
    speed_std_dev = sd(effective_speed)
  )

write.csv(pitcher_speed, "data/pitcher_speed.csv")

# make a dataframe containing only batter, pitch type, speed, and result
batter_selected <- sc_selected |>
  mutate(result = ifelse(type=='X', events, type)) |>
  select(batter, pitch_type, effective_speed, result) |>
  drop_na()