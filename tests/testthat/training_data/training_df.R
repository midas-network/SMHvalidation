## System and library
#library(dplyr)
#
## Create training data (sysnthetic designed for testing validation code)
#
#target_name <- c("inc death", "inc hosp", "inc case", "cum case", "cum hosp",
#                 "cum death")
#quantiles <- c(0, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
#               0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
#               0.99, 1, NA)
#
## Round 12: US Value
#scen_id <- c("A-2022-01-09", "B-2022-01-09", "C-2022-01-09", "D-2022-01-09")
#mproj_date <- as.Date("2022-01-09")
#target_num <- paste0(rep(1:12, length(target_name)), " wk ahead ",
#                 rep(target_name, each = 12))
#
#US_base_df <- expand.grid(location =  c("US"),
#                          quantile = quantiles,
#                          scenario_id = scen_id,
#                          model_projection_date = mproj_date,
#                          target = target_num) %>%
#  mutate(target_end_date = as.Date(model_projection_date) +
#           (as.numeric(gsub("[^[:digit:]]", "", target)) * 7) - 1,
#         type = ifelse(is.na(quantile), "point", "quantile"),
#         scenario_name = case_when(
#           scenario_id == "A-2022-01-09" ~ "optSev_highIE",
#           scenario_id == "B-2022-01-09" ~ "optSev_lowIE",
#           scenario_id == "C-2022-01-09" ~ "pessSev_highIE",
#           scenario_id == "D-2022-01-09" ~ "pessSev_lowIE"),
#         value = case_when(
#           grepl(" wk .+ inc case", target) & quantile == 0.5 ~ 5000000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum case", target) & quantile == 0.5 ~ 65000000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc death", target) & quantile == 0.5 ~ 12000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum death", target) & quantile == 0.5 ~ 850000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc hosp", target) & quantile == 0.5 ~ 150000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum hosp", target) & quantile == 0.5 ~ 1 * (as.numeric(gsub("[^[:digit:]]", "", target)))),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 1 & as.numeric(gsub("[^[:digit:]]", "", target)) < 7, value / 2, value),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 7, value * 0.1, value),
#         value = ifelse(scenario_id == "B-2022-01-09", value * 1.05, value),
#         value = ifelse(scenario_id == "C-2022-01-09", value * 1.1, value),
#         value = ifelse(scenario_id == "D-2022-01-09", value * 1.15, value),
#         target_name = gsub("\\d+ wk ahead ", "", target))
#
#lst_df <- split(US_base_df, list(US_base_df$location, US_base_df$scenario_id,
#                                 US_base_df$target_name))
#
#US_base_df <- lapply(lst_df, function(x) {
#  for (i in 2:12) {
#    if (grepl("cum ", unique(x$target_name))) {
#      x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"] <-
#        x[which(x$quantile == 0.5 & grepl(paste0("^", i - 1, " wk"),x$target)), "value"] +
#        (x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"]/10)
#    }
#    x
#  }
#  x
#}) %>% bind_rows()
#
#lst_df <- split(US_base_df, list(US_base_df$scenario_id, US_base_df$location,
#                                 US_base_df$target))
#US_base_df <- lapply(lst_df, function(x) {
#  df <- mutate(x, value = ifelse(type == "point",
#                                 x[which(quantile == 0.5), "value"], value))
#  sort_quantile <- sort(na.omit(unique(df$quantile)))
#  for (i in 1:(length(sort_quantile))) {
#    if (is.na(df[which(df$quantile == sort_quantile[i]), "value"])) {
#      if (sort_quantile[1] < 0.5) {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- df[which(df$quantile == 0.5), "value"]
#      } else {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- sort_quantile[i] * df[which(df$quantile == 0.5), "value"] + df[which(df$quantile == 0.5), "value"]
#      }
#    }
#    df
#  }
#  df
#}) %>% bind_rows() %>%
#  select(-target_name)
#
#rm(lst_df)
#
## Round 10: Alaska
#scen_id <- c("A-2021-11-09", "B-2021-11-09", "C-2021-11-09", "D-2021-11-09")
#mproj_date <- as.Date("2021-11-14")
#target_num <- paste0(rep(1:26, length(target_name)), " wk ahead ",
#                     rep(target_name, each = 26))
#
#ak_base_df <- expand.grid(location =  c("02"),
#                          quantile = quantiles,
#                          scenario_id = scen_id,
#                          model_projection_date = mproj_date,
#                          target = target_num) %>%
#  mutate(target_end_date = as.Date(model_projection_date) +
#           (as.numeric(gsub("[^[:digit:]]", "", target)) * 7) - 1,
#         type = ifelse(is.na(quantile), "point", "quantile"),
#         scenario_name = case_when(
#           scenario_id == "A-2021-11-09" ~ "optWan_highBoo",
#           scenario_id == "B-2021-11-09" ~ "optWan_lowBoo",
#           scenario_id == "C-2021-11-09" ~ "pessWan_highBoo",
#           scenario_id == "D-2021-11-09" ~ "pessWan_lowBoo"),
#         value = case_when(
#           grepl(" wk .+ inc case", target) & quantile == 0.5 ~ 2700 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum case", target) & quantile == 0.5 ~ 141000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc death", target) & quantile == 0.5 ~ 30 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum death", target) & quantile == 0.5 ~ 855 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc hosp", target) & quantile == 0.5 ~ 100 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum hosp", target) & quantile == 0.5 ~ 1 * (as.numeric(gsub("[^[:digit:]]", "", target)))),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 1 & as.numeric(gsub("[^[:digit:]]", "", target)) < 7, value / 2, value),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 7, value * 0.1, value),
#         value = ifelse(scenario_id == "B-2021-11-09", value * 1.05, value),
#         value = ifelse(scenario_id == "C-2021-11-09", value * 1.1, value),
#         value = ifelse(scenario_id == "D-2021-11-09", value * 1.15, value),
#         target_name = gsub("\\d+ wk ahead ", "", target))
#
#lst_df <- split(ak_base_df, list(ak_base_df$location, ak_base_df$scenario_id,
#                                 ak_base_df$target_name))
#
#ak_base_df <- lapply(lst_df, function(x) {
#  for (i in 2:26) {
#    if (grepl("cum ", unique(x$target_name))) {
#      x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"] <-
#        x[which(x$quantile == 0.5 & grepl(paste0("^", i - 1, " wk"),x$target)), "value"] +
#        (x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"]/10)
#    }
#    x
#  }
#  x
#}) %>% bind_rows()
#
#lst_df <- split(ak_base_df, list(ak_base_df$scenario_id, ak_base_df$location,
#                                 ak_base_df$target))
#ak_base_df <- lapply(lst_df, function(x) {
#  df <- mutate(x, value = ifelse(type == "point",
#                                 x[which(quantile == 0.5), "value"], value))
#  sort_quantile <- sort(na.omit(unique(df$quantile)))
#  for (i in 1:(length(sort_quantile))) {
#    if (is.na(df[which(df$quantile == sort_quantile[i]), "value"])) {
#      if (sort_quantile[1] < 0.5) {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- df[which(df$quantile == 0.5), "value"]
#      } else {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- sort_quantile[i] * df[which(df$quantile == 0.5), "value"] + df[which(df$quantile == 0.5), "value"]
#      }
#    }
#    df
#  }
#  df
#}) %>% bind_rows() %>%
#  select(-target_name)
#
#rm(lst_df)
#
## Round 8: Alabama
#scen_id <- c("A-2021-08-17", "B-2021-08-17", "C-2021-08-17", "D-2021-08-17")
#mproj_date <- as.Date("2021-08-15")
#target_num <- paste0(rep(1:27, length(target_name)), " wk ahead ",
#                     rep(target_name, each = 27))
#quantiles <- c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45,
#               0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.975,
#               0.99, NA)
#
#al_base_df <- expand.grid(location =  c("01"),
#                          quantile = quantiles,
#                          scenario_id = scen_id,
#                          model_projection_date = mproj_date,
#                          target = target_num) %>%
#  mutate(target_end_date = as.Date(model_projection_date) +
#           (as.numeric(gsub("[^[:digit:]]", "", target)) * 7) - 1,
#         type = ifelse(is.na(quantile), "point", "quantile"),
#         scenario_name = case_when(
#           scenario_id == "A-2021-08-17" ~ "noWan",
#           scenario_id == "B-2021-08-17" ~ "highPro_fastWan",
#           scenario_id == "C-2021-08-17" ~ "lowPro_slowWan",
#           scenario_id == "D-2021-08-17" ~ "lowPro_fastWan"),
#         value = case_when(
#           grepl(" wk .+ inc case", target) & quantile == 0.5 ~ 900000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum case", target) & quantile == 0.5 ~ 37000000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc death", target) & quantile == 0.5 ~ 4600 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum death", target) & quantile == 0.5 ~ 621000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ inc hosp", target) & quantile == 0.5 ~ 80000 * (as.numeric(gsub("[^[:digit:]]", "", target))),
#           grepl(" wk .+ cum hosp", target) & quantile == 0.5 ~ 1 * (as.numeric(gsub("[^[:digit:]]", "", target)))),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 1 & as.numeric(gsub("[^[:digit:]]", "", target)) < 7, value / 2, value),
#         value = ifelse(as.numeric(gsub("[^[:digit:]]", "", target)) > 7, value * 0.1, value),
#         value = ifelse(scenario_id == "B-2021-08-17", value * 1.05, value),
#         value = ifelse(scenario_id == "C-2021-08-17", value * 1.1, value),
#         value = ifelse(scenario_id == "D-2021-08-17", value * 1.15, value),
#         target_name = gsub("\\d+ wk ahead ", "", target))
#
#lst_df <- split(al_base_df, list(al_base_df$location, al_base_df$scenario_id,
#                                 al_base_df$target_name))
#
#al_base_df <- lapply(lst_df, function(x) {
#  for (i in 2:27) {
#    if (grepl("cum ", unique(x$target_name))) {
#      x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"] <-
#        x[which(x$quantile == 0.5 & grepl(paste0("^", i - 1, " wk"),x$target)), "value"] +
#        (x[which(x$quantile == 0.5 & grepl(paste0("^", i, " wk"),x$target)), "value"]/10)
#    }
#    x
#  }
#  x
#}) %>% bind_rows()
#
#lst_df <- split(al_base_df, list(al_base_df$scenario_id, al_base_df$location,
#                                 al_base_df$target))
#al_base_df <- lapply(lst_df, function(x) {
#  df <- mutate(x, value = ifelse(type == "point",
#                                 x[which(quantile == 0.5), "value"], value))
#  sort_quantile <- sort(na.omit(unique(df$quantile)))
#  for (i in 1:(length(sort_quantile))) {
#    if (is.na(df[which(df$quantile == sort_quantile[i]), "value"])) {
#      if (sort_quantile[1] < 0.5) {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- df[which(df$quantile == 0.5), "value"]
#      } else {
#        df[which(df$quantile == sort_quantile[i]), "value"] <- sort_quantile[i] * df[which(df$quantile == 0.5), "value"] + df[which(df$quantile == 0.5), "value"]
#      }
#    }
#    df
#  }
#  df
#}) %>% bind_rows() %>%
#  select(-target_name)
#
#rm(lst_df)
#
## Link all data frame together
#base_df <- rbind(US_base_df, ak_base_df, al_base_df)
#
#
#
## Write df:
#write.csv(base_df %>% filter(location == "US"), "tests/testthat/training_data/2022-01-09_no_error.csv", row.names = FALSE)
## col test
#write.csv(base_df %>% filter(location == "US", is.na(quantile)) %>% rename(fips = location), "tests/testthat/training_data/2022-01-09_colname.csv", row.names = FALSE)
#arrow::write_parquet(base_df %>% filter(location == "US") %>% mutate(row = seq(nrow(.))), "tests/testthat/training_data/2022-01-09_addcol.pqt")
## scenario
#write.csv(base_df %>% filter(location == "US") %>% mutate(scenario_id = gsub("A-2022", "A-2020", scenario_id)), "tests/testthat/training_data/2022-01-09_badidscen.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(scenario_name = gsub("optSev_highIE", "optSevhighIE", scenario_name)), "tests/testthat/training_data/2022-01-09_badnamescen.csv", row.names = FALSE)
## model projection date
#write.csv(base_df %>% filter(location == "US") %>% mutate(model_projection_date = rep(c("2022-01-09", "2022-01-11"), each = nrow(.)/2)), "tests/testthat/training_data/2022-01-09_multimpd.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(model_projection_date = as.Date("2922-01-09")), "tests/testthat/training_data/2022-01-09_mpd_error.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(model_projection_date = "01/09/2022"), "tests/testthat/training_data/2022-01-09_mpd_format.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(model_projection_date = as.Date("2922-01-09")), "tests/testthat/training_data/2922-01-09_mpdfile_error.csv", row.names = FALSE)
## quantiles
#write.csv(base_df %>% filter(location == "US") %>% mutate(quantile = gsub(0.01, 0.02, quantile)), "tests/testthat/training_data/2022-01-09_badquant.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% filter(!grepl("0\\.5$", quantile)), "tests/testthat/training_data/2022-01-09_missquant.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% filter(!grepl("^0$|^1$", quantile)), "tests/testthat/training_data/2022-01-09_missoptquant.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & quantile == 1 & location == "US" & target == "1 wk ahead inc death", 0, value)), "tests/testthat/training_data/2022-01-09_valquant.csv", row.names = FALSE)
## value
#write.csv(base_df %>% filter(location == "US") %>% mutate(type = ifelse(is.na(quantile) & location == "US", "pnt", type)), "tests/testthat/training_data/2022-01-09_badpointtype.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(quantile = ifelse(grepl("point", type) & location == "US", 0.5, quantile)), "tests/testthat/training_data/2022-01-09_badpointquant.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & quantile == 0 & location == "US" & target == "1 wk ahead inc death", -10, value)), "tests/testthat/training_data/2022-01-09_negvalue.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% rbind(filter(., scenario_id == "A-2022-01-09" & location == "US" & target == "1 wk ahead inc death")), "tests/testthat/training_data/2022-01-09_doublepoint.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% rbind(filter(., scenario_id == "A-2022-01-09" & location == "US" & target == "1 wk ahead inc death", quantile == 0)), "tests/testthat/training_data/2022-01-09_doublequantzero.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & quantile == 1 & location == "US" & target == "1 wk ahead inc death", 1e9, value)), "tests/testthat/training_data/2022-01-09_highvalue.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & location == "US" & grepl("inc death", target), 1, value)), "tests/testthat/training_data/2022-01-09_uniquevalue.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & location == "US" & target == "1 wk ahead cum case", 10, value)), "tests/testthat/training_data/2022-01-09_lowcumcase.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% mutate(value = ifelse(scenario_id == "A-2022-01-09" & location == "US" & target == "1 wk ahead cum death", 1, value)), "tests/testthat/training_data/2022-01-09_lowcumdeath.csv", row.names = FALSE)
## target
#write.csv(base_df %>% filter(location == "US") %>% mutate(target = gsub("1 wk ahead inc case", "1 wk ahead inccase", target)), "tests/testthat/training_data/2022-01-09_badnametarget.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>% filter(!grepl("12 wk", target)), "tests/testthat/training_data/2022-01-09_misswk.csv", row.names = FALSE)
#gz_df <- base_df %>% filter(location == "01")
#gz1 <- gzfile("tests/testthat/training_data/2021-08-15_morewk.gz", "w")
#write.csv(gz_df, gz1, row.names = FALSE)
#close(gz1)
#write.csv(base_df %>% filter(location == "US") %>% mutate(target_end_date = as.Date(gsub("2022-01-15", "2022-01-09", target_end_date))), "tests/testthat/training_data/2022-01-09_badstartdate.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "US") %>%
#            mutate(
#              scenario_id = case_when(
#                scenario_id == "A-2022-01-09" ~ "A-2022-02-25",
#                scenario_id == "B-2022-01-09" ~ "B-2022-02-25",
#                scenario_id == "C-2022-01-09" ~ "C-2022-02-25",
#                scenario_id == "D-2022-01-09" ~ "D-2022-02-25"),
#              scenario_name = case_when(
#                scenario_name == "optSev_highIE" ~ "optWan_noVar",
#                scenario_name == "optSev_lowIE" ~ "optWan_Var",
#                scenario_name == "pessSev_highIE" ~ "pessWan_noVar",
#                scenario_name == "pessSev_lowIE" ~ "pessWan_Var"),
#              model_projection_date = "2022-03-13",
#            target_end_date = as.Date(target_end_date) + lubridate::period(9, "week")),
#          "tests/testthat/training_data/2022-03-13_round13_missingweek.csv", row.names = FALSE)
## location
#write.csv(base_df %>% filter(location == "US") %>% mutate(location = gsub("US", "0202", location)), "tests/testthat/training_data/2022-01-09_badlocation.csv", row.names = FALSE)
#write.csv(base_df %>% filter(location == "02") %>% mutate(location = gsub("02", "2", location)), "tests/testthat/training_data/2021-11-14_no0location.csv", row.names = FALSE)
#zip("tests/testthat/training_data/2021-11-14_no0location", "tests/testthat/training_data/2021-11-14_no0location.csv")
#file.remove("tests/testthat/training_data/2021-11-14_no0location.csv")
#
## fail to ID round information
#write.csv(base_df %>% filter(location == "US", is.na(quantile), target_end_date == "2022-01-15", scenario_id == "A-2022-01-09") %>% mutate(scenario_id = "noID"), "tests/testthat/training_data/2022-01-15_noround.csv", row.names = FALSE)
#
## Truth data
#lst_gs <- pull_gs_data()
#saveRDS(lst_gs, "tests/testthat/training_data/2022-01-09_lst_gs.rds")
#
