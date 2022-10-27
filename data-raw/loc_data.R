git_path <- "https://raw.githubusercontent.com/midas-network/"
smh_locs <- readr::read_csv(paste0(
  git_path,
  "covid19-scenario-modeling-hub/master/data-locations/locations.csv"))
number2location <- setNames(smh_locs$location_name, smh_locs$location)
number2abbr <- setNames(smh_locs$abbreviation, smh_locs$location)

usethis::use_data(number2abbr, number2location, internal = TRUE)
