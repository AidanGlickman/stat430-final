library(tidyverse)

sc <- read.csv('data/statcast.csv')

sc_selected <- sc |> select(pitch_type, zone, type, bb_type, effective_speed, pitcher, batter, bat_score, fld_score, balls, strikes, outs_when_up, inning, inning_topbot)

write.csv(sc_selected, 'data/sc_selected.csv')
