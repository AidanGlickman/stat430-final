library(tidyverse)

# Simulates a single pitch between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "ball", "strike", "foul", "single", "double", "triple", "home run", or "out"
# - pitch: (character) the pitch thrown
simPitch <- function(pitcherID, batterID, gameState, atBatState, randomSeed = NULL) {
    if (!is.null(randomSeed)) {
        set.seed(randomSeed)
    } else {
        set.seed(Sys.time())
    }
    # TODO
}

# Simulates a single at-bat between a pitcher and a batter.
# Returns a list with the following elements:
# - result: (character) "strikeout", "walk", "single", "double", "triple", "home run", or "out"
# - pitches: (character vector) sequence of pitches thrown
simAtBat <- function(pitcherID, batterID, gameState, randomSeed = NULL) {
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
