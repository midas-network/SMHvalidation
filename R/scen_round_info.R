################################################################################
#' Extract README path information by round
#'
#' Extracts the raw Github path for each README by round from the GitHub
#' repository of the COVID19 Scenario Modeling Hub
#' \link{https://github.com/midas-network/covid19-scenario-modeling-hub}.
#' The function returns a tibble with 3 columns: "url" contains the link of the
#' README and the columns "round" and "round_number" containing the associated
#' round number.
#'
#' @param repo_path character vector, relative path of the COVID-19 scenario
#' model hub (i.e "midas-network/covid19-scenario-modeling-hub/")
#'
#' @importFrom gh gh
#' @importFrom dplyr tibble filter mutate %>%
#' @importFrom purrr map
#'
#' @examples
#' scenario_path_round("midas-network/covid19-scenario-modeling-hub/")
#'
#' @noRd
scenario_path_round <- function(repo_path) {
  # Extract path of each round README
  tree <- gh::gh(paste0("GET /repos/", repo_path, "git/trees/master?recursive=1"))
  tree_readme <- tibble(files =  unlist(purrr::map(tree$tree, "path"))) %>%
    filter(grepl("README", files), grepl(".md$", files), grepl("round", files),
           !grepl("1_and_2", files)) %>%
    mutate(url = paste0("https://raw.githubusercontent.com/", repo_path,
                        "master/", files),
           round = tolower(gsub("previous-rounds/README_|\\.md", "", files,
                                ignore.case = TRUE)))
  # Manually add information for the first 3 rounds
  first_readme <- data.frame(
    files = NA,
    url = c(
      "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/6dc683b9710c3a7eee6fa40d2986d0d79c0c918f/README.md",
      "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/a88188d964543ae06885d499eecaba8e34ee68f9/README.md",
      "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/52263c2086d8ef2d59c92b0a72d0d5c521290917/README.md"
    ),
    round = c("round1", "round2", "round3")
  )
  # Add last round and binds all the information together
  readme <- bind_rows(tree_readme, first_readme) %>%
    mutate(round_number = as.numeric(gsub("[^[:digit:]]", "", round))) %>%
    add_row(round_number = max(.$round_number) + 1,
            round = paste0("round", max(.$round_number) + 1),
            url = paste0("https://raw.githubusercontent.com/", repo_path,
                         "master/", "README.md")) %>%
    select(-files)
  return(readme)
}


################################################################################
#' Create summary table with round, scenario and date information
#'
#' From a path linking to the Scenario Modeling Hub, reads all the scenario's
#' README and extracts scenario name, scenario id and 1 weak ahead projection
#' date information.
#'
#' @param repo_path character vector, relative path of the COVID-19 scenario
#' model hub (i.e "midas-network/covid19-scenario-modeling-hub/")
#'
#' @importFrom utils read.delim
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows %>% add_row
#' @importFrom purrr map
#'
#' @examples
#' scen_round_info()
#'
#'@export
scen_round_info <- function(
  repo_path = "midas-network/covid19-scenario-modeling-hub/") {


  df <- scenario_path_round(repo_path)

  scen_round_info <- lapply(seq_len(dim(df)[1]), function(x) {
    # Read README
    scenario <- read.delim(df[x, "url", TRUE], sep = "\n",
                           header = FALSE)
    # extract start date of the projection and calculate the first week ahead
    # date value
    start_date <- scenario[which(grepl("Start date", scenario[,1])),] %>%
      stringr::str_extract("[:alpha:]+ \\d+, \\d\\d\\d\\d") %>%
      as.Date("%B %d, %Y") + 6
    # extract scenario id and name depending on the format of the
    # README (change after round 4)
    if (df[x, "round_number", TRUE] %in% c(1, 2, 3, 4)) {
      scenario_id <- scenario[which(grepl("Scenario id", scenario[,1])),] %>%
        stringr::str_extract(".-\\d{4}-\\d{2}-\\d{2}")
      scenario_name <- scenario[which(grepl("Scenario name",
                                            scenario[,1])),] %>%
        stringr::str_extract("\\`.+\\`")
    } else {
      table_info <- scenario[grep("\\|", scenario[,1]), 1] %>% strsplit("\\|")
      scenario_id <- table_info %>%
        unlist() %>% .[grep(".-\\d{4}-\\d{2}-\\d{2}", .)] %>% trimws()
      scenario_name <- purrr::map(table_info, table_info[grep(
        "scenario name", table_info, ignore.case = TRUE)] %>% unlist() %>%
          grep("scenario name", ., ignore.case = TRUE)) %>%
        grep("[[:alpha:]]", ., value = TRUE) %>%
        grep("scenario name|NULL", ., value = TRUE, invert = TRUE,
             ignore.case = TRUE) %>%
        trimws()
    }
    # regroup all the information in a table
    df <- data.frame(
      first_week_ahead = start_date,
      scenario_id = scenario_id,
      scenario_name = scenario_name,
      round = df[x, "round", TRUE]
    )
  }) %>% bind_rows() %>%
    mutate(scenario_name = gsub("\\`", "", scenario_name))

  return(scen_round_info)

}
