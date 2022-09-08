#' Runs Validation Checks on the `age_group` column
#'
#' Validate Scenario Modeling Hub submissions: test if the `age_group` column
#' contains the expected  value.
#'
#'@param df data frame to test
#'@param js_def list containing round definitions: number and names of columns,
#' target names, ...
#'
#'@details  This function contains 5 tests:
#'\itemize{
#'  \item{Age group: }{If the submission contains projection by age group,
#'  the `age_group` column contains the age group written `<AGEMIN>-<AGEMAX>`.}
#'  \item{Age group value: }{If the submission contains projection by
#'  age group, the `age_group` column contains the age group values as
#'  specify in the associated SMH GitHub Repository.}
#'  \item{Age group: }{If the submission contains projection by age group,
#'  `<AGEMIN>` cannot be equal or greater than` <AGEMAX>`.}
#'  \item{Age group overall: }{If the submission contains projection by
#'  age group, the `age_group` column should either contain one age group
#'  `0-130` OR multiple age groups adding to `0-130` + the overall value for
#'  the age group `0-130`.}
#'   \item{Age group target: }{If the submission contains projection by
#'  age group and if one or multiple targets required specific `age_group`
#'  value(s), no additional value(s) is provided in the submission file.}
#' }
#' Function called in the `validate_submission()` function.
#'
#'@importFrom stats na.omit
#'@importFrom dplyr distinct select mutate %>%
#'@importFrom tidyr separate
#'@importFrom purrr discard map keep
#'@export
test_agegroup <- function(df, js_def) {

  # - age group written `<AGEMIN>-<AGEMAX>`
  if (length(unique(grep("\\d{1,2}-\\d{1,3}", df$age_group, value = TRUE,
                         invert = TRUE))) > 0) {
    age_writ <- paste0(
      "\U000274c Error 801: The `age_group` column should contain value ",
      "written: 'AGEMIN-AGEMAX', one or more 'age_group' value(s) is ",
      "not corresponding in the submission file, please verify: '",
      paste(unique(grep("\\d{1,2}-\\d{1,3}", df$age_group, value = TRUE,
                  invert = TRUE)), collapse = "', '"), "'.")
  } else {
    age_writ <- NA
  }

  # - correspond to the age group values as specify in the associated SMH
  # GitHub Repository
  age <- dplyr::distinct(dplyr::select(df, age_group))

  if (all(is.na(age_writ))) {
    age <- tidyr::separate(age, age_group, c("min", "max"), sep = "-",
                           remove = FALSE)

    if (isFALSE(all(age$min %in% js_def$ages$age_min))) {
      age_min <- paste0(
        "\U000274c Error 802: The `age_group` column contains unexpected ",
        "value: '", paste(age$min[!(age$min %in% js_def$ages$age_min)],
                          collapse = "', '"),  "'. 'AGEMIN' can only be: ",
        paste(js_def$ages$age_min, collapse = ', '), ".")
    } else {
      age_min <- NA
    }

    if (isFALSE(all(age$max %in% js_def$ages$age_max))) {
      age_max <- paste0(
        "\U000274c Error 802: The `age_group` column contains unexpected ",
        "value: '", paste(age$max[!(age$max %in% js_def$ages$age_max)],
                          collapse = "', '"), "'. 'AGEMAX' can only be: ",
        paste(js_def$ages$age_max, collapse = ', '), ".")
    } else {
      age_max <- NA
    }

    # -`<AGEMIN>` cannot be equal or greater than` <AGEMAX>`
    age_test <- dplyr::mutate(
      age, min = as.numeric(min), max = as.numeric(max)) %>%
      dplyr::mutate(diff = max - min) %>%
      dplyr::filter(diff <= 0)
    if (dim(age_test)[1] > 0) {
      age_val <- paste0(
        "\U000274c Error 803: AGEMIN cannot be equal or greater than AGEMAX",
        ". Please verify the age_group: ", paste(unique(age_test$age_group),
                                                 collapse = ", "), ".")
    } else {
      age_val <- NA
    }
  } else {
    age_min <- NA
    age_max <- NA
    age_val <- NA
  }


  # -  either contain one age group  `0-130` OR multiple age groups adding to
  # `0-130` + the overall value for the age group `0-130`
  sel_group <- grep(
    "value|target_end_date|type|model_projection_date|scenario_name|age_group",
    js_def$column_names, invert = TRUE, value = TRUE)
  lst_df <-  split(df, as.list(df[,sel_group]), drop = TRUE)
  lst_df <- purrr::discard(lst_df, function(x) dim(x)[[1]] < 1)
  overall_test <- lapply(lst_df, function(x) {
    group <- paste(names(unique(x[, sel_group])), ":", unique(x[, sel_group]),
                   collapse = ", ")
    if (isFALSE(any(x$age_group %in% "0-130"))) {
      overall_test <- paste0(
        "\U000274c Error 804: All group should have an overall age group ",
        "value, please verify: ", group)
    } else {
      overall_test <- NA
    }
    add_age <- grep("0-130", x$age_group, value = TRUE, invert = TRUE)
    if (length(add_age) > 0 & all(grepl("\\d{1,2}-\\d{1,3}", add_age))) {
      if (any(!(seq(0, 130) %in% sort(unique(unlist(
        lapply(strsplit(add_age, "-"), function(x) seq(x[[1]], x[[2]])))))))) {
        sub_group <- paste0(
          "\U0001f7e1 Warning 805: All age group are not adding to 0-130",
          ", please verify: ", group)
      } else {
        sub_group <- NA
      }
    } else {
      sub_group <- NA
    }
    all_test <- unique(c(sub_group, overall_test))
    return(all_test)
  })
  if (length(na.omit(unlist(overall_test))) > 100) {
    overall_test <- unique(na.omit(unlist(overall_test))) %>%
      gsub(" target : \\d{1,2} wk ahead ", " target: ", .)  %>%
      gsub(" quantile : (\\d?(\\.\\d+)?|point)(,)?", "", .) %>%
      gsub(",$", "", .) %>% unique()
  }

  #- targets with specific age_group does not contains additional age_group
  age_target <- names(purrr::discard(purrr::map(
    c(js_def$targets$required, js_def$targets$optional), "agegroup"),
    function(x) any(x == "all")))
  targ_age_test <- lapply(age_target, function(x) {
    test <- unique(na.omit(unlist(c(js_def$targets$required,
                                    js_def$targets$optional)[[x]]$agegroup)))
    df_test <- dplyr::filter(df, !grepl(paste(test, collapse = "|"), age_group),
                             grepl(x, target))
    if (dim(df_test)[1] > 0) {
      age_test <- paste0(
        "\U0001f7e1 Warning 806: The target ", x, " should only contain the ",
        "agegroup(s): ", paste(test, collapse = ", "),
        ", the data frame contains other age_group (",
        paste(unique(df_test$age_group), collapse = ", "), "), please verify.")
    } else {
     age_test <- NA
    }
    return(age_test)
  })

  test_age <- unique(na.omit(c(age_writ, age_min, age_max, age_val,
                               unlist(overall_test), unlist(targ_age_test))))
  if (length(test_age) == 0)
    test_age <- "No errors or warnings found on Age_group"

  return(test_age)
}
