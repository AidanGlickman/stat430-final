library(tidyverse)
library(dplyr)

# pitcher_pitchtypes <- read.csv("data/pitcher_pitchtypes.csv")
# pitcher_speed <- read.csv("data/pitcher_speed.csv")
# batter_selected <- read.csv("data/batter_selected.csv")

# Chooses what pitch will be thrown in a given situation.
# Ex. Shohei pitching, losing, 2nd inning, 0 outs, 2 balls, 1 strike
# choosePitch(660271, FALSE, 2, 0, 2, 1)
choosePitch <- function(pitcherID, fld_winning, inning, outs_when_up, balls, strikes) {
    pitch <- "FF"
    # if a pitcher has faced this situation in game, randomly sample from those situations
    pitches <- pitcher_pitchtypes |> filter(pitcher == pitcherID & fld_winning == fld_winning & inning == inning & outs_when_up == outs_when_up & balls == balls & strikes == strikes)
    if (nrow(pitches) > 0) {
        pitch <- (pitches |> select(pitch_type) |> sample_n(1))
    } else {
        # otherwise, try again but ignore the inning and fld_winning columns
        pitches <- pitcher_pitchtypes |> filter(outs_when_up == outs_when_up & balls == balls & strikes == strikes)
        pitch <- pitches |>
            select(pitch_type) |>
            sample_n(1)
    }
    return(pitch[[1]])
}

# Simulates a single pitch between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "ball", "strike", "foul", "single", "double", "triple", "home run", or "out"
# - pitch: (character) the pitch thrown
# ex: simPitch(660271, 405395, FALSE, 1, 1, 1, 0)
simPitch <- function(pitcherID, batterID, fld_winning, inning, outs_when_up, balls, strikes) {
    # if (!is.null(randomSeed)) {
    #     set.seed(randomSeed)
    # } else {
    #     set.seed(Sys.time())
    # }
    pitchtype <- choosePitch(pitcherID, fld_winning, inning, outs_when_up, balls, strikes)
    row <- pitcher_speed |> filter(pitcher == pitcherID & pitch_type == pitchtype)
    speed <- rnorm(1, mean = row$speed_mean, sd = row$speed_std_dev)
    # print(paste("Threw a", pitchtype, "at", speed, "MPH"))
    # Now, we simulate how the batter does against this pitch.
    batter_results <- batter_selected |>
        filter(batter == batterID & pitch_type == pitchtype) |>
        # select the 30 pitches with the closest speeds
        mutate(speed_diff = abs(effective_speed - speed)) |>
        arrange(speed_diff) |>
        slice(1:30) |>
        select(result)
    # for some extra randomness, add some extras
    batter_results <- batter_results |>
        add_row(result = c("S", "B", "single", "double"))
    result <- batter_results |> sample_n(1)
    return(result)
}

# Simulates a single at-bat between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "strikeout", "walk", "single", "double", "triple", "home run", or "out"
# - pitches: (character vector) sequence of pitches thrown
# ex. simAtBat(660271, 405395, TRUE, 4, 2)
simAtBat <- function(pitcherID, batterID, fld_winning, inning, outs_when_up) {
    balls <- 0
    strikes <- 0
    pitches <- list()
    while (TRUE) {
        result <- simPitch(pitcherID, batterID, fld_winning, inning, outs_when_up, balls, strikes)
        # print(result)
        pitches <- append(pitches, result)
        if (result == "S") {
            strikes <- strikes + 1
        } else if (result == "B") {
            balls <- balls + 1
        } else {
            return(c(result, pitches))
        }
        if (balls >= 4) {
            return(c("walk", pitches))
        } else if (strikes >= 3) {
            return(c("strikeout", pitches))
        }
    }
}

# runners will only advance on a force, wii sports style
advanceRunners <- function(bases, num) {
    first <- bases[[1]]
    second <- bases[[2]]
    third <- bases[[3]]
    score <- 0
    home <- TRUE
    i <- 0
    while (i < num) {
        if (home | i > 0) {
            if (first | i > 1) {
                if (second | i > 2) {
                    if (third | i > 3) {
                        score <- score + 1
                        if (third) {
                            third <- FALSE
                        }
                    }
                    if (second) {
                        third <- TRUE
                        second <- FALSE
                    }
                }
                if (first) {
                    second <- TRUE
                    first <- FALSE
                }
            }
            if (home) {
                first <- TRUE
                home <- FALSE
            }
        }
        i <- i + 1
    }
    return(list(c(first, second, third), score))
}

# simInningHalf(660271, 405395, 7, 4, 3)
simInningHalf <- function(pitcherID, batterID, inning, fldScore, batScore) {
    outs <- 0
    score <- batScore
    bases <- c(FALSE, FALSE, FALSE)
    while (outs < 3) {
        # print(paste("State:", bases[[1]], bases[[2]], bases[[3]], "Outs:", outs))
        result <- simAtBat(pitcherID, batterID, fldScore >= score, inning, outs)[[1]]
        # print(paste("At Bat Result: ", result))
        if (result == "out" | result == "strikeout") {
            outs <- outs + 1
        } else {
            advance <- 0
            if (result == "single" | result == "walk") {
                advance <- 1
            }
            if (result == "double") {
                advance <- 2
            }
            if (result == "triple") {
                advance <- 3
            }
            if (result == "home_run") {
                advance <- 4
            }
            advanced <- advanceRunners(bases, advance)
            bases <- advanced[[1]]
            score <- score + advanced[[2]]
        }
    }
    return(score)
}

# Simulates a game between two teams
# Returns a list with the following elements:
# - homeScore: (integer) number of runs scored by the home team
# - awayScore: (integer) number of runs scored by the away team
# - report: (character vector) sequence of at-bats
# Game Between Shohei, Shohei and Gallen, Pujols
# simGame(660271,660271,668678,405395)
simGame <- function(homePitcherID, homeBatterID, awayPitcherID, awayBatterID) {
    inning <- 1
    homeScore <- 0
    awayScore <- 0
    while (TRUE) {
        if (inning > 9 & homeScore != awayScore) {
            return(c(homeScore, awayScore))
        }
        # print(paste("Top of", inning, "Home:", homeScore, "Away:", awayScore))
        awayScore <- simInningHalf(homePitcherID, awayBatterID, inning, homeScore, awayScore)
        # print(paste("Bottom of", inning, "Home:", homeScore, "Away:", awayScore))
        homeScore <- simInningHalf(awayPitcherID, homeBatterID, inning, awayScore, homeScore)
        inning <- inning + 1
    }
    print("Done!")
    return(c(homeScore, awayScore))
}

# Simulates a round-robin tournament between a set of teams
# Each repeat represents a home-away pair between each pair of teams
# Returns a dataframe with results for each game
# Takes in a dataframe of teamID,pitcherID,batterID
simTournament <- function(teams, repeats = 1) {
    # cartesian product of teamIDs
    teamIDs <- teams$team_id
    matchups <- cross_df(list(team1 = teamIDs, team2 = teamIDs)) |> filter(team1 != team2)
    # repeat matchups
    games <- as.data.frame(lapply(matchups, rep, repeats))
    # for each game, simulate the game. add columns for home/away score
    gameResults <- games |>
        mutate(homeScore = 0, awayScore = 0) |>
        # set homePitcherID, homeBatterID, awayPitcherID, awayBatterID to their respective values in teams where teamID matches
        left_join(teams, by = c("team1" = "team_id")) |>
        rename(homePitcherID = pitcher_id, homeBatterID = batter_id) |>
        left_join(teams, by = c("team2" = "team_id")) |>
        rename(awayPitcherID = pitcher_id, awayBatterID = batter_id) |>
        rowwise() |>
        mutate(result = list(simGame(homePitcherID, homeBatterID, awayPitcherID, awayBatterID))) |>
        mutate(homeScore = result[[1]], awayScore = result[[2]]) |>
        select(-result)
    return(gameResults)
}
