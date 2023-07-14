#' Format Gold Standard Data for Plotting
#'
#' @param lst_gs list of dataframe
#' @param projection_date date
#'
#' @export
#'
#' @importFrom dplyr bind_rows mutate select
#' @importFrom tidyr separate
format_gs_data <- function(lst_gs, projection_date) {

  # Ground Truth Data
  gs_data <- lapply(1:length(lst_gs), function(x) lst_gs[[x]] %>%
                      dplyr::mutate(outcome = names(lst_gs)[[x]])) %>%
    dplyr::bind_rows()

  gs_data <- gs_data %>%
    dplyr::mutate(outcome = tolower(outcome),
                  outcome = gsub("_num", "", outcome),
                  outcome = gsub("incidence", "inc", outcome),
                  outcome = gsub("cumulative", "cum", outcome),
                  outcome = gsub("hospitalization", "hospitalization_inc",
                                 outcome)) %>%
    dplyr::select(date = time_value, location = fips, value, outcome, value) %>%
    tidyr::separate(outcome, into = c("outcome", "incid_cum"), sep = "_") %>%
    dplyr::mutate(outcome = dplyr::recode(outcome, "confirmed" = "case",
                                          "deaths" = "death",
                                          "hospitalization" = "hosp")) %>%
    dplyr::mutate(pre_gs_end = date < projection_date)

  return(gs_data)
}

#' Print outlier tables for validation report
#'
#' @param data data frame
#' @param tab_title character vector, title
#' @param metric character vector, metric
#' @param thresholds numeric vector, thresholds
#' @param colors vector, colors
#'
#' @export
#'
#' @importFrom dplyr select filter mutate across
#' @importFrom tidyr pivot_wider
#' @importFrom scales comma percent
#' @importFrom tibble tibble
#' @importFrom gridExtra ttheme_default tableGrob
#' @importFrom gtable gtable_add_grob gtable_add_rows
#' @importFrom grid gpar rectGrob
#' @importFrom tidyselect all_of
print_table <- function(
    data = proj_plot_data_calib_cum,
    tab_title =
      "Top Outliers: Percent difference from ground truth, Cumulative",
    metric = "prctdiff_gt", #"median",
    thresholds = c(-Inf, -.20, -.06, 0),
    colors = c("red", "orange", "yellow", "orange")) {

  data <- data %>% filter(!is.na(scenario_id))
  # Format the table
  tab_data <- data %>%
    dplyr::select(scenario_id, state, outcome, `ground truth`,
                  value = tidyselect::all_of(metric)) %>%
    tidyr::pivot_wider(names_from = scenario_id, values_from = value)

  # Cells to highlight
  NAs <- which(is.na(tab_data), arr.ind = TRUE)
  if (nrow(NAs) > 0) {
    NAs <- NAs[which(NAs[[2]] %in% which(names(tab_data) %in%
                                           unique(data$scenario_id))),]
  }

  if (nrow(NAs) > 0 || is.null(nrow(NAs))) {
    NAs <- tibble(row = NAs[["row"]],
                  col = NAs[["col"]])
    NAs[[1]] <- NAs[[1]] + 1
  }

  thresh_cols <- lapply(1:length(thresholds),  function(x) {
    tmp <- which(tab_data >= thresholds[x] &
                   tab_data < c(thresholds, Inf)[x + 1] & is.na(tab_data),
                 arr.ind = TRUE)
    tmp <- tibble::as_tibble(tmp)

    if (nrow(NAs) > 0 & !is.null(nrow(NAs))) {
      tmp <- tmp[which(tmp$col %in%  which(names(tab_data) %in%
                                             unique(data$scenario_id))), ]
      tmp$row <- tmp$row + 1
    } else {
      tmp <- NA
    }
    tmp
  })

  # Format
  tab_data <- tab_data %>%
    dplyr::mutate(`ground truth` = scales::comma(`ground truth`, accuracy = 1))

  if (grepl("prct|percent", metric)) {
    tab_data <- tab_data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(unique(data$scenario_id)), ~ scales::percent(
          .x, accuracy=1)))
  } else if (grepl("ratio", metric)) {
    tab_data <- tab_data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(unique(data$scenario_id)), ~ round(.x, 2)))
  } else {
    tab_data <- tab_data %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(
        unique(data$scenario_is)), ~ scales::comma(.x, accuracy = 1)))
  }

  if (nrow(tab_data) == 0) {
    tab_data <- tibble::tibble(
      value = paste0("               There are no projections in this category",
                     " for this submission. Well done!               "))
  }

  # Build the table
  tt <- gridExtra::ttheme_default(base_size = 10)
  g <- gridExtra::tableGrob(tab_data, rows = NULL, theme = tt)
  g <- gtable::gtable_add_grob(
    g, grobs = grid::rectGrob(
      gp = grid::gpar(fill = NA, lwd = 2, fontsize = 7)), t = 2, b = nrow(g),
    l = 1, r = ncol(g))
  g <- gtable::gtable_add_grob(
    g, grobs = grid::rectGrob(
      gp = grid::gpar(fill = NA, lwd = 2, fontsize = 7)), t = 1, l = 1,
    r = ncol(g))

  find_cell <- function(table, row, col, name="core-fg") {
    l <- table$layout
    which(l$t == row & l$l == col & l$name == name)
  }

  # Highlight based on thresholds
  for (i in 1:length(thresh_cols)) {

    if (nrow(thresh_cols[[i]]) == 0 || is.na(thresh_cols[[i]])) next

    for (x in 1:nrow(thresh_cols[[i]])) {
      ind_ <- find_cell(g, as.integer(thresh_cols[[i]][x,1]),
                        as.integer(thresh_cols[[i]][x,2]), "core-bg")
      g$grobs[ind_][[1]][["gp"]] <- grid::gpar(fill = colors[i],
                                               col = "lightgrey")
    }
  }

  # Grey out NAs
  if (nrow(NAs) > 0) {
    for (x in seq_along(nrow(NAs))) {
      print(x)
      ind_ <- find_cell(g, as.integer(NAs[x,1]), as.integer(NAs[x,2]),
                        "core-fg")
      g$grobs[ind_][[1]][["gp"]] <- grid::gpar(col = "lightgrey")
    }
  }

  # Add Title
  title <- grid::textGrob(tab_title, gp = grid::gpar(fontsize = 14))
  padding <- grid::unit(1,"line")
  g <- gtable::gtable_add_rows(g, heights = grid::grobHeight(title) + padding,
                               pos = 0)
  g <- gtable::gtable_add_grob(g, list(title), t = 1, l = 1, r = ncol(g))

  g
}

#' Plot Projections
#'
#' @param data dataframe
#' @param st vector
#' @param projection_date a data vector
#' @param legend_rows numeric, 1 by default
#' @param y_sqrt boolean, FALSE by default
#'
#' @export
#'
#' @importFrom ggplot2 scale_y_sqrt scale_y_continuous aes ggplot geom_ribbon
#' @importFrom ggplot2 geom_vline geom_point scale_x_date scale_color_viridis_d
#' @importFrom ggplot2 scale_fill_viridis_d theme_bw theme guides guide_legend
#' @importFrom ggplot2 coord_cartesian element_text facet_wrap xlab geom_line
#' @importFrom lubridate as_date
#' @importFrom dplyr filter
#' @importFrom glue glue
plot_projections <- function(data, st, projection_date, legend_rows = 1,
                             y_sqrt = FALSE) {

  if (y_sqrt) {
    scale_y_funct <- ggplot2::scale_y_sqrt
  } else {
    scale_y_funct <- ggplot2::scale_y_continuous
  }

  projection_date <- lubridate::as_date(projection_date)


  plot <- data %>%
    dplyr::filter(scenario_id != "ground truth") %>%
    ggplot2::ggplot(ggplot2::aes(x = date)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high,
                                      fill = scenario_id), alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = median, color = scenario_id),
                       size = 1.5, linetype = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = projection_date),
                        color = "grey", linetype = 2, size = 1.5)

  if (any(grepl("point", colnames(data)))) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(y = point, color = scenario_id),
                         linetype = 2)
  }

  if (any(grepl("value_gt", colnames(data)))) {
    plot <- plot +
      ggplot2::geom_point(data = data %>% dplyr::filter(pre_gs_end == TRUE),
                          ggplot2::aes(y = value_gt), color = "black") +
      ggplot2::geom_point(data = data %>% dplyr::filter(pre_gs_end == FALSE),
                          ggplot2::aes(y = value_gt), color = "red")
  }

  plot <- plot +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    ggplot2::scale_color_viridis_d("Scenario") +
    ggplot2::scale_fill_viridis_d("Scenario") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top", legend.text = ggplot2::element_text(
      size = 8), axis.text.x = ggplot2::element_text(
        size = 8, angle = 45, hjust = 1),
      axis.text.y = ggplot2::element_text(size = 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
    ggplot2::xlab(NULL) +
    ggplot2::coord_cartesian(
      xlim = c(projection_date - 7 * 3, lubridate::as_date(max(data$date)))) +
    ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
    scale_y_funct(glue::glue(
      "Weekly {ifelse(data$incid_cum=='inc', 'Incident', 'Cumulative')} ",
      "Outcomes, {st}"))

}

#' Generate PDF of state plots
#'
#' @param proj_data dataframe
#' @param gs_data list of dataframe, containing observed data
#' @param team_model_name character vector, name of the team and models,
#'   following Scenario Modeling Hub standard
#' @param projection_date date vector
#' @param save_path character vector, path to the saving folder for the PDF
#'    output
#' @param plot_quantiles numeric vector, quantiles to use for plotting (should
#'    correspond to the quantiles from the `proj_data` parameter)
#' @param y_sqrt boolean, FALSE by default
#'
#' @export
#'
#' @importFrom dplyr filter mutate select rename arrange bind_rows left_join
#' @importFrom dplyr full_join desc slice_head pull
#' @importFrom tibble as_tibble
#' @importFrom tidyr separate pivot_wider
#' @importFrom readr read_csv
#' @importFrom lubridate as_date
#' @importFrom ggpubr get_legend
#' @importFrom grDevices pdf dev.off
#' @importFrom gridExtra grid.arrange
#' @importFrom grid unit gpar textGrob
#' @importFrom cowplot ggdraw draw_label plot_grid
#' @importFrom glue glue
#' @importFrom ggplot2 theme margin
#'
make_state_plot_pdf <- function(proj_data, gs_data, team_model_name,
                                projection_date, save_path,
                                plot_quantiles = c(0.025, 0.975),
                                y_sqrt = FALSE) {

  # Projections - Clean up and merge
  proj_data <- proj_data %>%
    dplyr::mutate(location = stringr::str_pad(location, 2, "left", "0")) %>%
    dplyr::filter(output_type_id %in% c(plot_quantiles[1], 0.5,
                                  plot_quantiles[2])) %>%
    tidyr::separate(target, into = c("incid_cum", "outcome"), sep = " ") %>%
    dplyr::rename(date = target_end_date, quantile = output_type_id) %>%
    tibble::as_tibble()


  if (!is.null(gs_data)) {
    proj_data <- proj_data %>%
      dplyr::bind_rows(gs_data %>% dplyr::rename(value_gt = value) %>%
                         dplyr::mutate(date = lubridate::as_date(date),
                                       scenario_id = "ground truth") %>%
                         dplyr::filter(date <= max(proj_data$date,
                                                   na.rm = TRUE))) %>%
      dplyr::select(scenario_id, location, incid_cum, outcome, date, quantile,
                    value, value_gt, pre_gs_end)
  } else {
    proj_data <- proj_data %>%
      dplyr::select(scenario_id, location, incid_cum, outcome,  date, quantile,
                    value)
  }

  proj_data <- proj_data  %>%
    dplyr::mutate(state = number2abbr[as.character(location)]) %>%
    dplyr::arrange(scenario_id, state, incid_cum, outcome, date)

  proj_plot_data <- proj_data %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value)

  if ("NA" %in% colnames(proj_plot_data))
    proj_plot_data <- dplyr::select(proj_plot_data, -`NA`)


  # Rename the quantiles
  colnames(proj_plot_data) <- gsub(plot_quantiles[1], "low",
                                   colnames(proj_plot_data))
  colnames(proj_plot_data) <- gsub(0.5, "median", colnames(proj_plot_data))
  colnames(proj_plot_data) <- gsub(plot_quantiles[2], "high",
                                   colnames(proj_plot_data))

  # Add calibration stats
  if (!is.null(gs_data)) {
      proj_plot_data_calib <- proj_plot_data %>%
        dplyr::filter(scenario_id != "ground truth" & date == (
          lubridate::as_date(projection_date) + 6)) %>%
        dplyr::select(-value_gt) %>%
        dplyr::full_join(proj_plot_data %>%
                           dplyr::filter(scenario_id == "ground truth",
                                         !is.na(value_gt), pre_gs_end) %>%
                           dplyr::filter(date == max(date)) %>%
                           dplyr::select(location, state, incid_cum, outcome,
                                         value_gt)) %>%
        dplyr::mutate(diff_gt = round(median - value_gt),
                      prctdiff_gt = round(diff_gt / value_gt,2),
                      ratio_gt = round(median / value_gt,4),
                      logratio_gt = log(ratio_gt))


      # Max differences

      # Incident
      proj_plot_data_calib_inc <- proj_plot_data_calib %>%
        dplyr::filter(incid_cum == "inc") %>%
        dplyr::filter(!((value_gt <= 20 & median <= 20))) %>%
        #arrange(desc(abs(prctdiff_gt))) %>%
        dplyr::arrange(desc(abs(logratio_gt))) %>%
        dplyr::slice_head(n = 30) %>%
        dplyr::mutate(outcome = paste0('incid ', .$outcome),
                      median = round(median)) %>%
        dplyr::select(scenario_id, state, outcome, `ground truth` = value_gt,
                      median, prctdiff_gt, ratio_gt, logratio_gt)

      # cumulative - over estimated cum
      proj_plot_data_calib_cum_pos <- proj_plot_data_calib %>%
        dplyr::filter(prctdiff_gt >= 0) %>%
        dplyr::filter(incid_cum == "cum") %>%
        dplyr::filter(!((value_gt <= 20 & median <= 20))) %>%
        #arrange(desc(prctdiff_gt)) %>%
        dplyr::arrange(desc(abs(logratio_gt))) %>%
        dplyr::slice_head(n = 20) %>%
        dplyr::mutate(outcome = paste0('incid ', .$outcome),
                      median = round(median)) %>%
        dplyr::select(scenario_id, state, outcome, `ground truth` = value_gt,
                      median, prctdiff_gt, ratio_gt, logratio_gt)

      # cumulative - underestimated cum
      proj_plot_data_calib_cum_neg <- proj_plot_data_calib %>%
        dplyr::filter(prctdiff_gt < 0) %>%
        dplyr::filter(incid_cum == "cum") %>%
        #arrange(desc(abs(prctdiff_gt))) %>%
        dplyr::arrange(desc(abs(logratio_gt))) %>%
        dplyr::slice_head(n = 40) %>%
        dplyr::mutate(outcome = paste0('incid ', .$outcome),
                      median = round(median)) %>%
        dplyr::select(scenario_id, state, outcome, `ground truth` = value_gt,
                      median, prctdiff_gt, ratio_gt, logratio_gt)

      # TABLES

      tab_inc_num <- print_table(
        data = proj_plot_data_calib_inc,
        tab_title = "Incident Outliers: [Wk1 Projected] minus [Wk0 Reported]",
        metric = "median", thresholds = NA, colors = NA)
      tab_cum_num_pos <- print_table(
        data = proj_plot_data_calib_cum_pos,
        tab_title =
          "Cumulative Overestimates: [Wk1 Projected] minus [Wk0 Reported]",
        metric = "median", thresholds = NA, colors = NA)
      tab_cum_num_neg <- print_table(
        data = proj_plot_data_calib_cum_neg,
        tab_title =
          "Cumulative Underestimates: [Wk1 Projected] minus [Wk0 Reported]",
        metric = "median", thresholds = NA, colors = NA)

      tab_inc_ratios <- print_table(
        data = proj_plot_data_calib_inc,
        tab_title = "Incident Outliers: [Wk1 Projected] / [Wk0 Reported]",
        metric = "ratio_gt", thresholds = c(0, .25, .5, 0.75, 1.333, 2, 4),
        colors = c("red", "orange", "yellow", NA, "yellow", "orange", "red"))
      tab_cum_ratios_pos <- print_table(
        data = proj_plot_data_calib_cum_pos,
        tab_title =
          "Cumulative Overestimates: [Wk1 Projected] / [Wk0 Reported]",
        metric = "ratio_gt", thresholds = c(1, 1.1, 1.25),
        colors = c("yellow", "orange", "red"))
      tab_cum_ratios_neg <- print_table(
        data = proj_plot_data_calib_cum_neg,
        tab_title =
          "Cumulative Underestimates: [Wk1 Projected] / [Wk0 Reported]",
        metric = "ratio_gt", thresholds = c(0, .8, 0.91),
        colors = c("red", "orange", "yellow"))
  }

  # Create PDF

  states_ <- unique(proj_plot_data %>% dplyr::filter(
    scenario_id != "ground truth") %>% dplyr::pull(state))
  if ("US" %in% states_) {
    states_ <- c("US", states_[states_ != "US"])
  }

  # INCIDENT
  p_inc <- proj_plot_data %>%
    dplyr::filter(state == states_[1]) %>%
    dplyr::filter(incid_cum == "inc") %>%
    plot_projections(states_[1], projection_date, legend_rows = 1,
                     y_sqrt = y_sqrt)
  p_legend <- ggpubr::get_legend(p_inc)


  pdf(save_path, width = 8.5, height = 11)

  # Tables
  if (!is.null(gs_data)) {
    gridExtra::grid.arrange(
      grid::textGrob(paste0("MODEL PROJECTIONS:\n", team_model_name, "  --  ",
                            projection_date),
                     gp = grid::gpar(fontsize = 14, fontface = "bold")),
      tab_inc_ratios,
      tab_cum_ratios_pos,
      tab_cum_ratios_neg,
      padding = unit(1, "line"),
      nrow = 4,
      heights = c(0.5,3,2,2))

    gridExtra::grid.arrange(
      tab_inc_num,
      tab_cum_num_pos,
      tab_cum_num_neg,
      padding = unit(1, "line"),
      nrow = 3,
      heights = c(3,2,2))
  }


  # Curves

  for (st in states_) {
    # INCIDENT
    p_inc <- proj_plot_data %>%
      dplyr::filter(state == st) %>%
      dplyr::filter(incid_cum == "inc") %>%
      plot_projections(st, projection_date, legend_rows = 1, y_sqrt = y_sqrt)

    # CUMULATIVE
    p_cum <- proj_plot_data %>%
      dplyr::filter(state == st) %>%
      dplyr::filter(incid_cum == "cum") %>%
      plot_projections(st, projection_date, legend_rows = 1, y_sqrt = y_sqrt)

    title <- cowplot::ggdraw() +
      cowplot::draw_label(glue::glue(
        "{st} -- {projection_date} - {team_model_name}"), fontface = 'bold',
        x = 0.5, hjust = .5) +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

    # Plot it all together
    plot(cowplot::plot_grid(
      title,
      p_legend,
      cowplot::plot_grid(
        p_inc + ggplot2::theme(legend.position = "none"),
        p_cum + ggplot2::theme(legend.position = "none"),
        nrow = 1),
      ncol = 1, rel_heights = c(.05,.1, 1)))
  }
  dev.off()
}





#' Generate Validation Plots
#'
#' @param path_proj dataframe, format as the Scenario Modeling Hub standard
#' @param lst_gs, list of dataframe, observed data (advice to use the output
#'    of the function `pull_gs_data()`). If NULL, no comparison to observed
#'    data will be done.
#' @param save_path character vector, path to the saving folder for the PDF
#'    output
#' @param y_sqrt boolean, by default FALSE
#' @param plot_quantiles numeric vector, quantiles to use for plotting (should
#'    correspond to the quantiles from the `proj_data` parameter)
#'
#' @export
#'
#' @importFrom stringr str_split
#' @importFrom lubridate as_date
#' @importFrom dplyr mutate_if mutate
generate_validation_plots <- function(path_proj, lst_gs,
                                      save_path=dirname(path_proj),
                                      y_sqrt = FALSE,
                                      plot_quantiles = c(0.025, 0.975)) {

  # SETUP
  file_ <- basename(path_proj)
  info_ <- stringr::str_split(file_, pattern = "_")[[1]]
  print(info_)
  projection_date <- lubridate::as_date(stringr::str_extract(
    file_, "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}"))
  team_model_name <- gsub(
    "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}(_|-)|(.csv|.zip|.gz|.pq)",
    "", file_)
  save_path <- file.path(save_path, paste0(projection_date, "_",
                                           team_model_name, "_plots.pdf"))

  # Ground Truth Data
  if (is.null(lst_gs)) {
    #lst_gs <- pull_gs_data()
    gs_data <- NULL
  } else {
    gs_data <- suppressWarnings(format_gs_data(lst_gs, projection_date))
  }

  # Projections
  proj_data <- suppressMessages(read_files(path_proj)) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate(
      target_end_date = lubridate::as_date(origin_date) - 1 + (horizon * 7),
      origin_date = lubridate::as_date(origin_date)) %>%
    dplyr::filter(grepl(
      "inc case|inc death|inc hosp|cum case|cum death|cum hosp", target) &
      grepl("quantile", output_type))

  if (any("age_group" %in% colnames(proj_data)))
    proj_data <- dplyr::filter(proj_data, grepl("0-130", age_group))

  #remove artifact column
  if ("X" %in% colnames(proj_data)) {
    proj_data <- proj_data %>% dplyr::select(-X)
  }

  # Create PDF of State Plots
  make_state_plot_pdf(proj_data = proj_data, gs_data = gs_data,
                      team_model_name = team_model_name,
                      projection_date = projection_date,
                      save_path = save_path, plot_quantiles = plot_quantiles,
                      y_sqrt = y_sqrt)

}

