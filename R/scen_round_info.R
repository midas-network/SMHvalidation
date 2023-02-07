################################################################################
#' Extract README path information by round [DEPRECATED]
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
#' @param branch character, name of the branch to extract information from. By
#' default "master"
#' @param add_readme data frame of 4 columns: files (NA),
#'   url ("https""//LINK/README") and round ("round1") to manually add README
#'   path information in the output. By default, NULL (does not add anything)
#'
#' @importFrom gh gh
#' @importFrom dplyr tibble filter mutate %>%
#' @importFrom purrr map
#'
#' @examples
#' scenario_path_round("midas-network/covid19-scenario-modeling-hub/")
#'
#' @noRd
scenario_path_round <- function(repo_path, branch = "master",
                                add_readme = NULL) {
  # Extract path of each round README
  tree <- gh::gh(paste0("GET /repos/", repo_path, "git/trees/", branch,
                        "?recursive=1"))
  tree_readme <- tibble(files =  unlist(purrr::map(tree$tree, "path"))) %>%
    filter(grepl("README", files), grepl(".md$", files), grepl("round", files),
           !grepl("1_and_2", files), !grepl("resources", files)) %>%
    mutate(url = paste0("https://raw.githubusercontent.com/", repo_path,
                        "master/", files),
           round = tolower(gsub("previous-rounds/README_|\\.md", "", files,
                                ignore.case = TRUE)))

  # Add last round and binds all the information together
  readme <- bind_rows(tree_readme, add_readme)

  if (dim(readme)[1] == 0) {
    readme <- data.frame(
      url = paste0("https://raw.githubusercontent.com/", repo_path, branch,
                   "/", "README.md"),
      round = "round1",
      round_number = 1
    )
  } else {
    readme <- mutate(
      readme,
      round_number = as.numeric(gsub("[^[:digit:]]", "", round))) %>%
      add_row(round_number = max(.$round_number) + 1,
              round = paste0("round", max(.$round_number) + 1),
              url = paste0("https://raw.githubusercontent.com/", repo_path,
                           "master/", "README.md"))   %>%
      select(-files)
  }
  return(readme)
}


################################################################################
#' Create summary table with round, scenario and date information [DEPRECATED]
#'
#' From a path linking to the Scenario Modeling Hub, reads all the scenario's
#' README and extracts scenario name, scenario id and 1 weak ahead projection
#' date information.
#'
#' @param repo_path character vector, relative path of the COVID-19 scenario
#' model hub (i.e "midas-network/covid19-scenario-modeling-hub/")
#' @param branch character, name of the branch to extract information from. By
#' default "master"
#' @param add_readme data frame of 4 columns: files (NA),
#'  url ("https""//LINK/README") and round ("round1") to manually add README
#'  path information in the output. By default, NULL (does not add anything)
#'
#' @importFrom utils read.delim
#' @importFrom stringr str_extract
#' @importFrom dplyr bind_rows %>% add_row
#' @importFrom purrr map
#'
#' @details For the four first rounds of COVID-19, the SMH README are slightly
#' different and required the use of the "add_readme" parameters or it will
#' returns an error. Please see the examples
#'
#' Function deprecated in Feb, 2023.
#'
#' @examples
#'
#' \dontrun{
#' # For COVID-19
#' repo_path <- "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/"
#' scen_round_info(add_readme =  data.frame(
#'   files = NA,
#'   url = c(
#'     paste0(repo_path, "6dc683b9710c3a7eee6fa40d2986d0d79c0c918f/README.md"),
#'     paste0(repo_path, "a88188d964543ae06885d499eecaba8e34ee68f9/README.md"),
#'     paste0(repo_path, "52263c2086d8ef2d59c92b0a72d0d5c521290917/README.md")
#'   ),
#'   round = c("round1", "round2", "round3")
#' )
#')
#'
#'# For FLU
#'scen_round_info(repo_path = "midas-network/flu-scenario-modeling-hub/",
#'                branch = "main")
#' }
#'
#'@export
scen_round_info <- function(
  repo_path = "midas-network/covid19-scenario-modeling-hub/", branch = "master",
  add_readme = NULL) {

  df <- scenario_path_round(repo_path, branch = branch,
                            add_readme = add_readme)

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
    # README (change after round 4 for COVID SMH repo)
    if (!is.null(add_readme) & (df[x, "round_number", TRUE] %in% c(1, 2, 3, 4))) {
        scenario_id <- scenario[which(grepl("Scenario id", scenario[,1])),] %>%
          stringr::str_extract(".-\\d{4}-\\d{2}-\\d{2}")
        scenario_name <- scenario[which(grepl("Scenario name",
                                              scenario[,1])),] %>%
          stringr::str_extract("\\`.+\\`")
      }  else {
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
