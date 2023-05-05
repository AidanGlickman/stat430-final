library(tidyverse)

pitcher_pitchtypes <- read.csv("data/pitcher_pitchtypes.csv")
pitcher_speed <- read.csv("data/pitcher_speed.csv")
batter_selected <- read.csv("data/batter_selected.csv")

choosePitch <- function(pitcherID, fld_winning, inning, outs_when_up, balls, strikes) {
    pitch <- "FF"
    # if a pitcher has faced this situation in game, randomly sample from those situations
    pitches <- pitcher_pitchtypes |> filter(pitcher == pitcherID & fld_winning == fld_winning & inning == inning & outs_when_up == outs_when_up & balls == balls & strikes == strikes)
    if(nrow(pitches) > 0){
        pitch <- (pitches |> select(pitch_type) |> sample_n(1))
    } else {
        # otherwise, try again but ignore the inning and fld_winning columns
        pitches <- pitcher_pitchtypes |> filter(outs_when_up == outs_when_up & balls == balls & strikes == strikes)
        pitch <- pitches |> select(pitch_type) |> sample_n(1)
    }
    return(pitch[[1]])
}

# Simulates a single pitch between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "ball", "strike", "foul", "single", "double", "triple", "home run", or "out"
# - pitch: (character) the pitch thrown
# ex: simPitch(112526, 430832, TRUE, 1, 0, 0, 0)
simPitch <- function(pitcherID, batterID, fld_winning, inning, outs_when_up, balls, strikes, randomSeed = NULL) {
    if (!is.null(randomSeed)) {
        set.seed(randomSeed)
    } else {
        set.seed(Sys.time())
    }
    pitchtype <- choosePitch(pitcherID, fld_winning, inning, outs_when_up, balls, strikes)
    print(pitchtype)
    row <- pitcher_speed |> filter(pitcher == pitcherID & pitch_type == pitchtype)
    speed <- rnorm(1, mean = row$speed_mean, sd = row$speed_std_dev)

    # Now, we simulate how the batter does against this pitch.
    batter_results <- batter_selected |>
        filter(batter == batterID & pitch_type == pitchtype) |>
        # select the 30 pitches with the closest speeds
        mutate(speed_diff = abs(effective_speed - speed)) |>
        arrange(speed_diff) |>
        slice(1:30) |>
        select(result)
    # for some extra randomness, add one each of S, B, out, single, double, triple, home_run
    batter_results <- batter_results |>
        add_row(result = c("S", "B", "out", "single", "double", "triple", "home_run"))
    result <- batter_results |> sample_n(1)
    print(paste(pitchtype, speed, result))
    return(result)
}

# Simulates a single at-bat between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "strikeout", "walk", "single", "double", "triple", "home run", or "out"
# - pitches: (character vector) sequence of pitches thrown
simAtBat <- function(pitcherID, batterID, fld_winning, inning, outs_when_up, randomSeed = NULL) {
    # TODO
}

# Simulates a game between two teams
# Returns a list with the following elements:
# - winner: (character) "home" or "away"
# - homeScore: (integer) number of runs scored by the home team
# - awayScore: (integer) number of runs scored by the away team
# - report: (character vector) sequence of at-bats
simGame <- function(homeTeamID, awayTeamID, randomSeed = NULL) {
    # TODO
}

# Simulates a round-robin tournament between a set of teams
# Each repeat represents a home-away series between each pair of teams
# Returns a dataframe with results for each game
simTournament <- function(teamIDs, repeats = 2, randomSeed = NULL) {
    # TODO
}
