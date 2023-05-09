# The Eck Baseball League (EBL) is a 2v2 baseball league with teams spanning all eras.
# source library.R

source("simulator/library.R")

# The EBL is played in a round-robin tournament format. Each team plays every other team 2 times.
teams <- read.csv("data/ebl_teams.csv")
results <- simTournament(teams, repeats = 1)

write.csv(results, 'data/ebl_results.csv')