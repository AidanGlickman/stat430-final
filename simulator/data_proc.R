# Optimally, this file should only really be run once. 
# It should be used to construct pitcher_expectancy and batter_expectancy and then not touched again.
library(tidyverse)
library(devtools)
library(R.utils)
source_gist("https://gist.github.com/bayesball/8892981",
    filename = "parse.retrosheet2.pbp.R"
)

RETROYEARMIN <- 1914
RETROYEARMAX <- 2022

retro_all <- data.frame()
for (year in RETROYEARMIN:RETROYEARMAX) {
    print(year)
    # Read in the play-by-play data
    pbp <- parse.retrosheet2.pbp(year)
}

fields <- read_csv("download.folder/unzipped/fields.csv")

all_data <- read_csv(paste0("download.folder/unzipped/all", RETROYEARMIN, ".csv"),col_names = pull(fields, Header), na = character())|>
  select(INN_CT, OUTS_CT, BALLS_CT, STRIKES_CT, PITCH_SEQ_TX, AWAY_SCORE_CT, HOME_SCORE_CT, BAT_ID, PIT_ID, LEADOFF_FL, EVENT_ID)
for (year in RETROYEARMIN+1:RETROYEARMAX) {
    print(year)
    curr <- read_csv(paste0("download.folder/unzipped/all", year, ".csv"),
                     col_names = pull(fields, Header), na = character())|>
      select(INN_CT, OUTS_CT, BALLS_CT, STRIKES_CT, PITCH_SEQ_TX, AWAY_SCORE_CT, HOME_SCORE_CT, BAT_ID, PIT_ID, LEADOFF_FL, EVENT_ID)
    all_data <- rbind(all_data, curr)
}
