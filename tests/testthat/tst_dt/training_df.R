# Clean environment
rm(list = ls())

##########
#
#    COVID-19
#
##########

# System and library
library(dplyr)

# Create training data (sysnthetic designed for testing validation code)

target_name <- c("inc death", "inc hosp", "inc case", "cum case", "cum hosp",
                 "cum death")
quantiles <- c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
               0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
               0.99, 1, NA)

# Round 12: US Value
scen_id <- c("A-2022-01-09", "B-2022-01-09", "C-2022-01-09", "D-2022-01-09")
mproj_date <- as.Date("2022-01-09")

us_base_df <- expand.grid(location =  c("US"),
                          output_type_id = quantiles,
                          scenario_id = scen_id,
                          origin_date = mproj_date,
                          target = target_name,
                          horizon = seq(1:12)) %>%
  mutate(output_type = ifelse(is.na(output_type_id), "median", "quantile"),
         value = case_when(grepl("inc case", target) & output_type_id == 0.5 ~
                             5000000 * horizon,
                           grepl("cum case", target) & output_type_id == 0.5 ~
                             65000000 * horizon,
                           grepl("inc death", target) & output_type_id == 0.5 ~
                             12000 * horizon,
                           grepl("cum death", target) & output_type_id == 0.5 ~
                             850000 * horizon,
                           grepl("inc hosp", target) & output_type_id == 0.5 ~
                             150000 * horizon,
                           grepl("cum hosp", target) & output_type_id == 0.5 ~
                             1 * horizon),
         value = ifelse(horizon > 1 & horizon < 7, value / 2, value),
         value = ifelse(horizon > 7, value * 0.1, value),
         value = ifelse(scenario_id == "B-2022-01-09", value * 1.05, value),
         value = ifelse(scenario_id == "C-2022-01-09", value * 1.1, value),
         value = ifelse(scenario_id == "D-2022-01-09", value * 1.15, value))

lst_df <- split(us_base_df, list(us_base_df$location, us_base_df$scenario_id,
                                 us_base_df$target))

us_base_df <- lapply(lst_df, function(x) {
  for (i in seq_len(12)[-1]) {
    if (grepl("cum ", unique(x$target))) {
      x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] <-
        x[which(x$output_type_id == 0.5 & x$horizon == (i - 1)), "value"] +
        (x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] / 10)
    }
    x
  }
  x
}) %>%
  bind_rows()

lst_df <- split(us_base_df, list(us_base_df$scenario_id, us_base_df$location,
                                 us_base_df$target, us_base_df$horizon))
us_base_df <- lapply(lst_df, function(x) {
  df <- mutate(x, value = ifelse(output_type == "median",
                                 x[which(output_type_id == 0.5), "value"],
                                 value))
  sort_type_id <- sort(na.omit(unique(df$output_type_id)))
  for (i in seq_len(length(sort_type_id))) {
    if (is.na(df[which(df$output_type_id == sort_type_id[i]), "value"])) {
      if (sort_type_id[i] < 0.5 & sort_type_id[i] > 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / sort_type_id[i]
      } else if (sort_type_id[i] < 0.5 & sort_type_id[i] == 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / 0.005
      } else {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          sort_type_id[i] * df[which(df$output_type_id == 0.5), "value"] +
          df[which(df$output_type_id == 0.5), "value"]
      }
    }
    df
  }
  df
}) %>%
  bind_rows() %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate(value = ifelse(value > 320000000, 320000000, value))

rm(lst_df)

# Round 10: Alaska
scen_id <- c("A-2021-11-09", "B-2021-11-09", "C-2021-11-09", "D-2021-11-09")
mproj_date <- as.Date("2021-11-14")

ak_base_df <- expand.grid(location =  c("02"),
                          output_type_id = quantiles,
                          scenario_id = scen_id,
                          origin_date = mproj_date,
                          target = target_name,
                          horizon = seq(1:26)) %>%
  mutate(output_type = ifelse(is.na(output_type_id), "median", "quantile"),
         value = case_when(grepl("inc case", target) & output_type_id == 0.5 ~
                             2700 * horizon,
                           grepl("cum case", target) & output_type_id == 0.5 ~
                             141000 * horizon,
                           grepl("inc death", target) & output_type_id == 0.5 ~
                             30 * horizon,
                           grepl("cum death", target) & output_type_id == 0.5 ~
                             855 * horizon,
                           grepl("inc hosp", target) & output_type_id == 0.5 ~
                             100 * horizon,
                           grepl("cum hosp", target) & output_type_id == 0.5 ~
                             1 * horizon),
         value = ifelse(horizon > 1 & horizon < 7, value / 2, value),
         value = ifelse(horizon > 7, value * 0.1, value),
         value = ifelse(scenario_id == "B-2021-11-09", value * 1.05, value),
         value = ifelse(scenario_id == "C-2021-11-09", value * 1.1, value),
         value = ifelse(scenario_id == "D-2021-11-09", value * 1.15, value))

lst_df <- split(ak_base_df, list(ak_base_df$location, ak_base_df$scenario_id,
                                 ak_base_df$target))

ak_base_df <- lapply(lst_df, function(x) {
  for (i in seq_len(26)[-1]) {
    if (grepl("cum ", unique(x$target))) {
      x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] <-
        x[which(x$output_type_id == 0.5 & x$horizon == (i - 1)), "value"] +
        (x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] / 10)
    }
    x
  }
  x
}) %>% bind_rows()

lst_df <- split(ak_base_df, list(ak_base_df$scenario_id, ak_base_df$location,
                                 ak_base_df$target, ak_base_df$horizon))
ak_base_df <- lapply(lst_df, function(x) {
  df <- mutate(x, value = ifelse(output_type == "median",
                                 x[which(output_type_id == 0.5), "value"],
                                 value))
  sort_type_id <- sort(na.omit(unique(df$output_type_id)))
  for (i in seq_length(length(sort_type_id))) {
    if (is.na(df[which(df$output_type_id == sort_type_id[i]), "value"])) {
      if (sort_type_id[i] < 0.5 & sort_type_id[i] > 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / sort_type_id[i]
      } else if (sort_type_id[i] < 0.5 & sort_type_id[i] == 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / 0.005
      } else {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          sort_type_id[i] * df[which(df$output_type_id == 0.5), "value"] +
          df[which(df$output_type_id == 0.5), "value"]
      }
    }
    df
  }
  df
}) %>%
  bind_rows() %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate(value = ifelse(value > 730000, 730000, value))

rm(lst_df)

# Round 8: Alabama
scen_id <- c("A-2021-08-17", "B-2021-08-17", "C-2021-08-17", "D-2021-08-17")
mproj_date <- as.Date("2021-08-15")
quantiles <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
               0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
               0.99, NA)

al_base_df <- expand.grid(location =  c("01"),
                          output_type_id = quantiles,
                          scenario_id = scen_id,
                          origin_date = mproj_date,
                          target = target_name,
                          horizon = seq(1:27)) %>%
  mutate(output_type = ifelse(is.na(output_type_id), "median", "quantile"),
         value = case_when(grepl("inc case", target) & output_type_id == 0.5 ~
                             9000 * horizon,
                           grepl("cum case", target) & output_type_id == 0.5 ~
                             700000 * horizon,
                           grepl("inc death", target) & output_type_id == 0.5 ~
                             4600 * horizon,
                           grepl("cum death", target) & output_type_id == 0.5 ~
                             62100 * horizon,
                           grepl("inc hosp", target) & output_type_id == 0.5 ~
                             8000 * horizon,
                           grepl("cum hosp", target) & output_type_id == 0.5 ~
                             1 * horizon),
         value = ifelse(horizon > 1 & horizon < 7, value / 2, value),
         value = ifelse(horizon > 7, value * 0.1, value),
         value = ifelse(scenario_id == "B-2021-08-17", value * 1.05, value),
         value = ifelse(scenario_id == "C-2021-08-17", value * 1.1, value),
         value = ifelse(scenario_id == "D-2021-08-17", value * 1.125, value))

lst_df <- split(al_base_df, list(al_base_df$location, al_base_df$scenario_id,
                                 al_base_df$target))

al_base_df <- lapply(lst_df, function(x) {
  for (i in seq_len(27)[-1]) {
    if (grepl("cum ", unique(x$target))) {
      x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] <-
        x[which(x$output_type_id == 0.5 & x$horizon == (i - 1)), "value"] +
        (x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] / 10)
    }
    x
  }
  x
}) %>% bind_rows()

lst_df <- split(al_base_df, list(al_base_df$scenario_id, al_base_df$location,
                                 al_base_df$target, al_base_df$horizon))
al_base_df <- lapply(lst_df, function(x) {
  df <- mutate(x, value = ifelse(output_type == "median",
                                 x[which(output_type_id == 0.5), "value"],
                                 value))
  sort_type_id <- sort(na.omit(unique(df$output_type_id)))
  for (i in seq_length(length(sort_type_id))) {
    if (is.na(df[which(df$output_type_id == sort_type_id[i]), "value"])) {
      if (sort_type_id[i] < 0.5 & sort_type_id[i] > 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / sort_type_id[i]
      } else if (sort_type_id[i] < 0.5 & sort_type_id[i] == 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / 0.005
      } else {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          sort_type_id[i] * df[which(df$output_type_id == 0.5), "value"] +
          df[which(df$output_type_id == 0.5), "value"]
      }
    }
    df
  }
  df
}) %>%
  bind_rows() %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate(value = ifelse(value > 4900000, 4900000, value))

rm(lst_df)

# Link all data frame together
base_df <- rbind(us_base_df, ak_base_df, al_base_df)


# Write df:
write.csv(base_df %>% filter(location == "US"),
          "tests/testthat/tst_dt/2022-01-09_no_error.csv", row.names = FALSE)
write.csv(base_df %>% filter(location == "US") %>% .[-100, ],
          "tests/testthat/tst_dt/2022-01-09_missingrow.csv", row.names = FALSE)
# col test
write.csv(base_df %>%
            filter(location == "US", is.na(output_type_id)) %>%
            rename(fips = location),
          "tests/testthat/tst_dt/2022-01-09_colname.csv", row.names = FALSE)
arrow::write_parquet(base_df %>%
                       filter(location == "US") %>%
                       mutate(row = seq_len(nrow(.))),
                     "tests/testthat/tst_dt/2022-01-09_addcol.pqt")
# scenario
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(scenario_id = gsub("A-2022", "A-2020", scenario_id)),
          "tests/testthat/tst_dt/2022-01-09_badidscen.csv", row.names = FALSE)
# Origin date
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(origin_date = rep(c("2022-01-09", "2022-01-11"),
                                     each = nrow(.) / 2)),
          "tests/testthat/tst_dt/2022-01-09_multimpd.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(origin_date = as.Date("2922-01-09")),
          "tests/testthat/tst_dt/2022-01-09_mpd_error.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(origin_date = "01/09/2022"),
          "tests/testthat/tst_dt/2022-01-09_mpd_format.csv", row.names = FALSE)
# output_type_ids
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(output_type_id = gsub(0.01, 0.02, output_type_id)),
          "tests/testthat/tst_dt/2022-01-09_badquant.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            filter(!grepl("0\\.5$", output_type_id)),
          "tests/testthat/tst_dt/2022-01-09_missquant.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            filter(!grepl("^0$|^1$", output_type_id)),
          "tests/testthat/tst_dt/2022-01-09_missoptquant.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    output_type_id == 1 & location == "US" &
                                    target == "inc death", 0, value)),
          "tests/testthat/tst_dt/2022-01-09_valquant.csv", row.names = FALSE)
# NA for median value and value decreasing for last quantiles

# value
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(output_type = ifelse(is.na(output_type_id) &
                                          location == "US", "pnt",
                                        output_type)),
          "tests/testthat/tst_dt/2022-01-09_badpointtype.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(output_type_id = ifelse(grepl("median", output_type) &
                                             location == "US", 0.5,
                                           output_type_id)),
          "tests/testthat/tst_dt/2022-01-09_badpointquant.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    grepl("^0$", output_type_id) &
                                    location == "US" & target == "inc death",
                                  -10, value)),
          "tests/testthat/tst_dt/2022-01-09_negvalue.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            rbind(filter(., scenario_id == "A-2022-01-09" & location == "US" &
                           target == "inc death")),
          "tests/testthat/tst_dt/2022-01-09_doublepoint.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            rbind(filter(., scenario_id == "A-2022-01-09" & location == "US" &
                           target == "inc death", output_type_id == 0)),
          "tests/testthat/tst_dt/2022-01-09_doublequantzero.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    grepl("^1$", output_type_id) &
                                    location == "US" & target == "inc death",
                                  1e9, value)),
          "tests/testthat/tst_dt/2022-01-09_highvalue.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    location == "US" &
                                    grepl("inc death", target), 1, value)),
          "tests/testthat/tst_dt/2022-01-09_uniquevalue.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    location == "US" & target == "cum case",
                                  10, value)),
          "tests/testthat/tst_dt/2022-01-09_lowcumcase.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(value = ifelse(scenario_id == "A-2022-01-09" &
                                    location == "US" & target == "cum death",
                                  1, value)),
          "tests/testthat/tst_dt/2022-01-09_lowcumdeath.csv", row.names = FALSE)
# target
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(target = gsub("inc case", "inccase", target)),
          "tests/testthat/tst_dt/2022-01-09_badnametarget.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            filter(!grepl(12, horizon)),
          "tests/testthat/tst_dt/2022-01-09_misswk.csv",
          row.names = FALSE)
gz_df <- base_df %>% filter(location == "01")
gz1 <- gzfile("tests/testthat/tst_dt/2021-08-15_morewk.gz", "w")
write.csv(gz_df, gz1, row.names = FALSE)
close(gz1)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(scenario_id = case_when(scenario_id == "A-2022-01-09" ~
                                             "A-2022-02-25",
                                           scenario_id == "B-2022-01-09" ~
                                             "B-2022-02-25",
                                           scenario_id == "C-2022-01-09" ~
                                             "C-2022-02-25",
                                           scenario_id == "D-2022-01-09" ~
                                             "D-2022-02-25"),
                   origin_date = "2022-03-13"),
          "tests/testthat/tst_dt/2022-03-13_round13_missingweek.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(scenario_id = case_when(scenario_id == "A-2022-01-09" ~
                                             "A-2022-02-25",
                                           scenario_id == "B-2022-01-09" ~
                                             "B-2022-02-25",
                                           scenario_id == "C-2022-01-09" ~
                                             "C-2022-02-25",
                                           scenario_id == "D-2022-01-09" ~
                                             "D-2022-02-25"),
                   origin_date = "2022-03-13",
                   value = ifelse(grepl("cum case", target) & horizon > 7,
                                  value / 1e6, value)),
          "tests/testthat/tst_dt/2022-03-13_round13_err.csv", row.names = FALSE)
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(scenario_id = case_when(scenario_id == "A-2022-01-09" ~
                                             "A-2022-05-09",
                                           scenario_id == "B-2022-01-09" ~
                                             "B-2022-05-09",
                                           scenario_id == "C-2022-01-09" ~
                                             "C-2022-05-09",
                                           scenario_id == "D-2022-01-09" ~
                                             "D-2022-05-09"),
                   origin_date = "2022-06-05",
                   target = gsub("inccase|inc case", "inc inf", target)),
          "tests/testthat/tst_dt/2022-06-05_round14_misswk_targ.csv",
          row.names = FALSE)
# location
write.csv(base_df %>%
            filter(location == "US") %>%
            mutate(location = gsub("US", "0202", location)),
          "tests/testthat/tst_dt/2022-01-09_badlocation.csv",
          row.names = FALSE)
write.csv(base_df %>%
            filter(location == "02" & !(output_type_id %in% c(0, 1))) %>%
            mutate(location = gsub("02", "2", location)),
          "tests/testthat/tst_dt/2021-11-14_no0location.csv", row.names = FALSE)
zip("tests/testthat/tst_dt/2021-11-14_no0location",
    "tests/testthat/tst_dt/2021-11-14_no0location.csv")
file.remove("tests/testthat/tst_dt/2021-11-14_no0location.csv")

###########
##
##   FLU
##
###########

# System and library
library(dplyr)

# Create training data (synthetic designed for testing validation code)

target_name <- c("inc death", "inc hosp", "cum hosp", "cum death",
                 "peak time hosp", "peak size hosp")
quantiles <- c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
               0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
               0.99, 1, NA)

# Round 1: US Value
scen_id <- c("A-2022-08-14", "B-2022-08-14", "C-2022-08-14", "D-2022-08-14")
mproj_date <- as.Date("2022-08-14")

us_base_df <- expand.grid(location =  c("US"),
                          output_type_id = quantiles,
                          scenario_id = scen_id,
                          origin_date = mproj_date,
                          target = target_name,
                          horizon = seq(1, 42)) %>%
  mutate(output_type = ifelse(is.na(output_type_id), "median", "quantile"),
         value = case_when(grepl("peak time", target) & output_type_id == 0.5 ~
                             0.1 * horizon - 3.2,
                           grepl("peak size", target) & output_type_id == 0.5 ~
                             65000000,
                           grepl("inc death", target) & output_type_id == 0.5 ~
                             12000 * horizon,
                           grepl("cum death", target) & output_type_id == 0.5 ~
                             1 * horizon,
                           grepl("inc hosp", target) & output_type_id == 0.5 ~
                             150000 * horizon,
                           grepl("cum hosp", target) & output_type_id == 0.5 ~
                             1000000 * horizon),
         value = ifelse(!grepl("size", target) & horizon > 1 & horizon < 7,
                        value / 2, value),
         value = ifelse(!grepl("size", target) & horizon > 7,
                        value * 0.1, value),
         value = ifelse(scenario_id == "B-2022-08-14", value * 1.05, value),
         value = ifelse(scenario_id == "C-2022-08-14", value * 1.1, value),
         value = ifelse(scenario_id == "D-2022-08-14", value * 1.15, value))

lst_df <- split(us_base_df, list(us_base_df$location, us_base_df$scenario_id,
                                 us_base_df$target))

us_base_df <- lapply(lst_df, function(x) {
  for (i in seq_len(42)[-1]) {
    if (grepl("cum ", unique(x$target))) {
      x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] <-
        x[which(x$output_type_id == 0.5 & x$horizon == (i - 1)), "value"] +
        (x[which(x$output_type_id == 0.5 & x$horizon == i), "value"] / 10)
    }
    x
  }
  x
}) %>%
  bind_rows()

lst_df <- split(us_base_df, list(us_base_df$scenario_id, us_base_df$location,
                                 us_base_df$target, us_base_df$horizon))
us_base_df <- lapply(lst_df, function(x) {
  df <- mutate(x, value = ifelse(output_type == "median",
                                 x[which(output_type_id == 0.5), "value"],
                                 value))
  sort_type_id <- sort(na.omit(unique(df$output_type_id)))
  for (i in seq_length(length(sort_type_id))) {
    if (is.na(df[which(df$output_type_id == sort_type_id[i]), "value"])) {
      if (sort_type_id[i] < 0.5 & sort_type_id[i] > 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / sort_type_id[i]
      } else if (sort_type_id[i] < 0.5 & sort_type_id[i] == 0) {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          df[which(df$output_type_id == 0.5), "value"] - 1 / 0.005
      } else {
        df[which(df$output_type_id == sort_type_id[i]), "value"] <-
          sort_type_id[i] * df[which(df$output_type_id == 0.5), "value"] +
          df[which(df$output_type_id == 0.5), "value"]
      }
    }
    df
  }
  df
}) %>% bind_rows()

rm(lst_df)

us_base_df <- filter(us_base_df, !(grepl("peak time", target) &
                                     grepl("quantile", output_type))) %>%
  filter(!(grepl("peak size", target) & horizon > 1)) %>%
  mutate(value = ifelse(value < 0, 0, value)) %>%
  mutate(value = ifelse(grepl("time", target), value + 0.9, value)) %>%
  mutate(value = ifelse(grepl("time", target) & value >= 1, 1, value)) %>%
  mutate(value = ifelse(grepl("time", target) & value <= 0, 0, value),
         age_group = "0-130") %>%
  mutate(output_type = ifelse(grepl("time", target), "cdf", output_type)) %>%
  mutate(output_type_id = ifelse(grepl("cdf", output_type), NA,
                                 output_type_id)) %>%
  mutate(horizon = ifelse(grepl("size", target), NA, horizon)) %>%
  distinct()

al_base_df <- filter(us_base_df, grepl("hosp", target)) %>%
  mutate(value = value * 0.00001, location = "02")  %>%
  mutate(value = ifelse(grepl("time", target) & horizon > 1, value + 0.5,
                        value)) %>%
  mutate(value = ifelse(grepl("time", target) & horizon > 20, value + 0.05,
                        value)) %>%
  mutate(value = ifelse(grepl("time", target) & horizon > 40, value + 1,
                        value)) %>%
  mutate(value = ifelse(grepl("time", target) & value >= 1, 1, value)) %>%
  mutate(value = ifelse(grepl("time", target) & value <= 0, 0, value))

tot <- rbind(us_base_df, al_base_df) %>%
  mutate(output_type_id = ifelse(target == "peak time hosp",
                                 as.character(as.Date((origin_date + 7 *
                                                         horizon) - 1)),
                                 as.character(output_type_id)))

peak_time_date <- tot[which(tot$target == "peak time hosp"), "output_type_id"]
peak_time_val <- MMWRweek::MMWRweek(peak_time_date) %>%
  mutate(MMWRweek = ifelse(nchar(MMWRweek) < 2, paste0(0, MMWRweek),
                           MMWRweek)) %>%
  tidyr::unite("output_type_id", MMWRyear, MMWRweek, sep = "")

tot[which(tot$target == "peak time hosp"), "output_type_id"] <-
  paste("EW", peak_time_val[["output_type_id"]], sep = "")

# Sample format

### For sample format
target_name <- c("inc death", "inc hosp")
sample <- seq(1, 100)
scen_id <- c("A-2022-08-14", "B-2022-08-14", "C-2022-08-14", "D-2022-08-14")
mproj_date <- as.Date("2022-08-14")

us_samp_df <- expand.grid(location =  c("US"),
                          output_type = "sample",
                          output_type_id = sample,
                          scenario_id = scen_id,
                          origin_date = mproj_date,
                          target = target_name,
                          horizon = seq(1, 42),
                          age_group = "0-130")  %>%
  mutate(value = case_when(grepl("inc death", target) & output_type_id == 1 ~
                             12000 * horizon,
                           grepl("inc hosp", target) & output_type_id == 1 ~
                             150000 * horizon),
         value = ifelse(scenario_id == "B-2022-08-14", value * 1.05, value),
         value = ifelse(scenario_id == "C-2022-08-14", value * 1.1, value),
         value = ifelse(scenario_id == "D-2022-08-14", value * 1.15, value))

lst_df <- split(us_samp_df, list(us_samp_df$location, us_samp_df$target,
                                 us_samp_df$scenario_id, us_samp_df$horizon))
us_samp_df <- lapply(lst_df, function(x) {
  val <- runif(100, min = unique(na.omit(x$value)) - 100,
               max = unique(na.omit(x$value)) + 100)
  x$value <- val
  x
}) %>%
  bind_rows()

tot_s <- rbind(tot, us_samp_df)

# Write df:
write.csv(tot, "tests/testthat/tst_dt/2022-08-14_flu_no_error.csv",
          row.names = FALSE)
write.csv(tot[!(grepl("median", tot$output_type) &
                  !grepl("time", tot$target)), ],
          "tests/testthat/tst_dt/2022-08-14_flu_nopoint_noerror.csv",
          row.names = FALSE)
write.csv(tot[!grepl("time", tot$target), ],
          "tests/testthat/tst_dt/2022-08-14_flu_misstarget.csv",
          row.names = FALSE)
write.csv(rbind(tot,
                dplyr::mutate(filter(us_base_df, grepl("death", target)),
                              location = "02")),
          "tests/testthat/tst_dt/2022-08-14_flu_addloc.csv",
          row.names = FALSE)
write.csv(dplyr::mutate(tot, age_group = "130-17"),
          "tests/testthat/tst_dt/2022-08-14_flu_missage.csv",
          row.names = FALSE)
write.csv(dplyr::mutate(tot, age_group = ifelse(target == "peak size hosp",
                                                "00_12", age_group)),
          "tests/testthat/tst_dt/2022-08-14_flu_badage.csv", row.names = FALSE)
write.csv(dplyr::select(tot, -age_group),
          "tests/testthat/tst_dt/2022-08-14_flu_noage.csv", row.names = FALSE)
write.csv(dplyr::mutate(tot, horizon = ifelse(grepl("size", target),
                                              1, horizon)),
          "tests/testthat/tst_dt/2022-08-14_badhorizon.csv", row.names = FALSE)

write.csv(tot_s, "tests/testthat/tst_dt/2022-08-14_flu_sample.csv",
          row.names = FALSE)
write.csv(dplyr::filter(tot_s, output_type_id < 90,
                        !grepl("inc death", target)) %>%
            dplyr::mutate(output_type_id = ifelse(output_type_id > 85, 104.5,
                                                  output_type_id)),
          "tests/testthat/tst_dt/2022-08-14_flu_badsample.csv",
          row.names = FALSE)

# Clean environment
rm(list = ls())
