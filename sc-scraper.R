library(bbd)

helper = function(year) {
  bbd::statcast(
    start = paste0(year, "-03-01"),
    end = paste0(year, "-12-01"),
    process = TRUE,
    names = TRUE,
    verbose = TRUE
  )
}

library(future.apply)
plan(multisession) ## Run in parallel on local computer, mostly automagic

results = do.call(rbind, future_lapply(
  X = 2018:2022,
  FUN = helper
))

nrow(results)

write_csv(results, file = "data/statcast.csv")