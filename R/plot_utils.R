#' Format tables
#'
#' Internal function to format outlier tables for validation report
#'
#' @param tab_data data frame
#' @param metric character vector, metric
#'
#' @importFrom dplyr mutate across
#' @importFrom scales comma percent
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' @noRd
format_tables <- function(tab_data, metric, sel_group) {
  tab_data <-
    dplyr::mutate(tab_data,
                  `ground truth` = scales::comma(.data[["ground truth"]],
                                                 accuracy = 1))

  if (grepl("prct|percent", metric)) {
    tab_data <- tab_data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(sel_group),
                                  ~ scales::percent(.x, accuracy = 1)))
  } else if (grepl("ratio", metric)) {
    tab_data <- tab_data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(sel_group),
                                  ~ round(.x, 2)))
  } else {
    tab_data <- tab_data |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(sel_group),
                                  ~ scales::comma(.x, accuracy = 1)))
  }

  if (nrow(tab_data) == 0) {
    tab_data <-
      tibble::tibble(value = paste0("There are no projections in this category",
                                    " for this submission. Well done!"))
  }
  return(tab_data)
}


#' Format - Highlight NA cells
#'
#' Internal function to format cells containing NA value
#'
#' @param tab_data data frame
#'
#' @importFrom tibble tibble
#' @noRd
na_cells <- function(tab_data, sel_group) {
  # Cells to highlight
  nas <- which(is.na(tab_data), arr.ind = TRUE)
  if (nrow(nas) > 0) {
    nas <- nas[which(nas[[2]] %in% which(names(tab_data) %in%
                                           sel_group)), ]
  }
  if (nrow(nas) > 0 || is.null(nrow(nas))) {
    nas <- tibble::tibble(row = nas[["row"]], col = nas[["col"]])
    nas[[1]] <- nas[[1]] + 1

  }
  return(nas)
}

#' Print outlier tables for validation report
#'
#' @param data data frame
#' @param tab_title character vector, title
#' @param metric character vector, metric
#' @param thresholds numeric vector, thresholds
#' @param colors vector, colors
#'
#' @noRd
#'
print_table <- function(data, tab_title, metric = "prctdiff_gt", #"median",
                        thresholds = c(-Inf, -.20, -.06, 0),
                        colors = c("red", "orange", "yellow", "orange")) {

  data <-  dplyr::filter(data, !is.na(.data[["scenario_id"]]))
  sel_group <- unique(data$scenario_id)
  # Format the table
  tab_data <- data |>
    dplyr::select(tidyr::all_of(c("scenario_id", "state", "outcome",
                                  "ground truth")),
                  value = tidyr::all_of(metric)) |>
    tidyr::pivot_wider(names_from = .data[["scenario_id"]],
                       alues_from = .data[["value"]])

  # Cells to highlight
  nas <- na_cells(tab_data, sel_group)

  thresh_cols <- lapply(seq_along(thresholds),  function(x) {
    tmp <- which(tab_data >= thresholds[x] &
                   tab_data < c(thresholds, Inf)[x + 1] & is.na(tab_data),
                 arr.ind = TRUE)
    if (nrow(nas) > 0 & !is.null(nrow(nas))) {
      tmp <- tmp[which(tmp$col %in%  which(names(tab_data) %in% sel_group)), ]
      tmp$row <- tmp$row + 1
    } else {
      tmp <- NA
    }
    tmp
  })

  # Format
  tab_data <- format_tables(tab_data, metric, sel_group)

  # Build the table
  tt <- gridExtra::ttheme_default(base_size = 10)
  g <- gridExtra::tableGrob(tab_data, rows = NULL, theme = tt)
  grid_grob <- grid::rectGrob(gp = grid::gpar(fill = NA, lwd = 2, fontsize = 7))
  g <- gtable::gtable_add_grob(g, grobs = grid_grob, t = 2, b = nrow(g), l = 1,
                               r = ncol(g))
  g <- gtable::gtable_add_grob(g,  grid_grob, t = 1, l = 1, r = ncol(g))

  find_cell <- function(table, row, col, name = "core-fg") {
    l <- table$layout
    which(l$t == row & l$l == col & l$name == name)
  }

  # Highlight based on thresholds
  for (i in seq_along(thresh_cols)) {

    if (nrow(thresh_cols[[i]]) == 0 || is.na(thresh_cols[[i]])) next

    for (x in seq_len(nrow(thresh_cols[[i]]))) {
      ind_ <- find_cell(g, as.integer(thresh_cols[[i]][x, 1]),
                        as.integer(thresh_cols[[i]][x, 2]), "core-bg")
      g$grobs[ind_][[1]][["gp"]] <- grid::gpar(fill = colors[i],
                                               col = "lightgrey")
    }
  }

  # Grey out NAs
  if (nrow(nas) > 0) {
    for (x in seq_along(nrow(nas))) {
      ind_ <- find_cell(g, as.integer(nas[x, 1]), as.integer(nas[x, 2]),
                        "core-fg")
      g$grobs[ind_][[1]][["gp"]] <- grid::gpar(col = "lightgrey")
    }
  }

  # Add Title
  title <- grid::textGrob(tab_title, gp = grid::gpar(fontsize = 14))
  padding <- grid::unit(1, "line")
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
#' @noRd
#'
plot_projections <- function(data, st, projection_date, legend_rows = 1,
                             y_sqrt = FALSE) {

  if (y_sqrt) {
    scale_y_funct <- ggplot2::scale_y_sqrt
  } else {
    scale_y_funct <- ggplot2::scale_y_continuous
  }

  projection_date <- as.Date(projection_date)


  plot <- dplyr::filter(data, .data[["scenario_id"]] != "ground truth") |>
    ggplot2::ggplot(ggplot2::aes(x = .data[["date"]])) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["low"]],
                                      ymax = .data[["high"]],
                                      fill = .data[["scenario_id"]]),
                         alpha = 0.20) +
    ggplot2::geom_line(ggplot2::aes(y = .data[["median"]],
                                    color = .data[["scenario_id"]]),
                       linewidth = 1.5, linetype = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = projection_date),
                        color = "grey", linetype = 2, linewidth = 1.5)

  if (any(grepl("point", colnames(data)))) {
    plot <- plot +
      ggplot2::geom_line(ggplot2::aes(y = .data[["point"]],
                                      color = .data[["scenario_id"]]),
                         linetype = 2)
  }

  if (any(grepl("value_gt", colnames(data)))) {
    plot <- plot +
      ggplot2::geom_point(data =
                            dplyr::filter(data, .data[["pre_gs_end"]] == TRUE),
                          ggplot2::aes(y = .data[["observation"]]),
                          color = "black") +
      ggplot2::geom_point(data =
                            dplyr::filter(data, .data[["pre_gs_end"]] == FALSE),
                          ggplot2::aes(y = .data[["observation"]]),
                          color = "red")
  }

  plot <- plot +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    ggplot2::scale_color_viridis_d("Scenario") +
    ggplot2::scale_fill_viridis_d("Scenario") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top",
                   legend.text = ggplot2::element_text(size = 8),
                   axis.text.x = ggplot2::element_text(size = 8, angle = 45,
                                                       hjust = 1),
                   axis.text.y = ggplot2::element_text(size = 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE)) +
    ggplot2::xlab(NULL) +
    ggplot2::coord_cartesian(xlim = c(projection_date - 7 * 3,
                                      lubridate::as_date(max(data$date)))) +
    ggplot2::facet_wrap(~outcome, ncol = 1, scales = "free_y") +
    scale_y_funct(glue::glue("Weekly {ifelse(data$incid_cum=='inc', ",
                             "'Incident', 'Cumulative')} Outcomes, {st}"))

}

#' Generate PDF of state plots
#'
#' @param proj_data dataframe
#' @param target_data dataframe, containing observed data
#' @param team_model_name character vector, name of the team and models,
#'   following Scenario Modeling Hub standard
#' @param projection_date date vector
#' @param save_path character vector, path to the saving folder for the PDF
#'    output
#' @param plot_quantiles numeric vector, quantiles to use for plotting (should
#'    correspond to the quantiles from the `proj_data` parameter)
#' @param y_sqrt boolean, FALSE by default
#'
#' @noRd
#'
#'
make_state_plot_pdf <- function(proj_data, target_data, team_model_name,
                                projection_date, save_path,
                                plot_quantiles = c(0.025, 0.975),
                                y_sqrt = FALSE) {
  # Projections - Clean up and merge
  proj_data <- loc_zero(proj_data) |>
    dplyr::filter(.data[["output_type_id"]] %in%
                    c(plot_quantiles[1], 0.5, plot_quantiles[2]),
                  !is.na(.data[["output_type_id"]])) |>
    tidyr::separate(tidyr::matches("^target$"),
                    into = c("type", "outcome"), sep = " ") |>
    dplyr::rename(tidyr::all_of(c(quantile = "output_type_id")))

  if (!is.null(target_data)) {
    target_data$date <- as.Date(target_data$date)
    target_data$scenario_id <- "ground truth"
    target_data <-
      dplyr::filter(target_data,
                    .data[["date"]] <= max(proj_data$date, na.rm = TRUE),
                    .data[["type"]] %in% c("inc", "cum"),
                    .data[["outcome"]] %in% c("hosp", "case", "death"))
    proj_data <- proj_data |>
      dplyr::bind_rows(target_data)
    col_sel <- c("scenario_id", "location", "type", "outcome", "date",
                 "quantile", "value", "observation", "pre_gs_end")
  } else {
    col_sel <- c("scenario_id", "location", "type", "outcome", "date",
                 "quantile", "value")
  }
  if (any("age_group" %in% colnames(proj_data)))
    proj_data <- dplyr::filter(proj_data, grepl("0-130", .data[["age_group"]]))
  if (any("race_ethnicity" %in% colnames(proj_data)))
    proj_data <- dplyr::filter(proj_data, grepl("overall",
                                                .data[["race_ethnicity"]]))

  proj_data <- dplyr::select(proj_data, tidyr::all_of(col_sel)) |>
    dplyr::mutate(state =  as.character(.data[["location"]])) |>
    dplyr::arrange(dplyr::across(tidyr::all_of(c("scenario_id", "state", "type",
                                                 "outcome", "date"))))

  proj_plot_data <-
    tidyr::pivot_wider(proj_data, names_from = .data[["quantile"]],
                       values_from = .data[["value"]])

  if ("NA" %in% colnames(proj_plot_data))
    proj_plot_data <- dplyr::select(proj_plot_data, -tidyr::matches("^NA$"))

  # Rename the quantiles
  colnames(proj_plot_data) <- gsub(plot_quantiles[1], "low",
                                   colnames(proj_plot_data))
  colnames(proj_plot_data) <- gsub(0.5, "median", colnames(proj_plot_data))
  colnames(proj_plot_data) <- gsub(plot_quantiles[2], "high",
                                   colnames(proj_plot_data))

  # Add calibration stats
  if (!is.null(target_data)) {
    proj_plot_data_calib <- proj_plot_data |>
      dplyr::filter(.data[["scenario_id"]] != "ground truth" &
                      .data[["date"]] == (as.Date(projection_date) + 6)) |>
      dplyr::select(-tidyr::any_of(c("observation", "population"))) |>
      dplyr::full_join(dplyr::filter(proj_plot_data,
                                     .data[["scenario_id"]] == "ground truth",
                                     !is.na(.data[["observation"]]),
                                     .data[["pre_gs_end"]]) |>
                         dplyr::filter(.data[["date"]] ==
                                         max(.data[["date"]])) |>
                         dplyr::select(dplyr::all_of(c("location", "state",
                                                       "type", "outcome",
                                                       "observation")))) |>
      dplyr::mutate(diff_gt = round(.data[["median"]] - .data[["observation"]]),
                    prctdiff_gt = round(.data[["diff_gt"]] /
                                          .data[["observation"]], 2),
                    ratio_gt = round(.data[["median"]] /
                                       .data[["observation"]], 4),
                    logratio_gt = log(.data[["ratio_gt"]]))

    # Max differences
    # Incident
    proj_plot_data_calib_inc <- proj_plot_data_calib |>
      dplyr::filter(.data[["type"]] == "inc",
                    !(.data[["observation"]] <= 20 &
                        .data[["median"]] <= 20)) |>
      dplyr::arrange(dplyr::desc(abs(.data[["logratio_gt"]]))) |>
      dplyr::slice_head(n = 30) |>
      dplyr::mutate(outcome = paste0("incid ", .data[["outcome"]]),
                    median = round(.data[["median"]])) |>
      dplyr::select(tidyr::all_of(c("scenario_id", "state", "outcome",
                                    "ground truth" = "observation", "median",
                                    "prctdiff_gt", "ratio_gt", "logratio_gt")))

    # cumulative - over estimated cum
    proj_plot_data_calib_cum_pos <- proj_plot_data_calib |>
      dplyr::filter(.data[["type"]] == "cum", .data[["prctdiff_gt"]] >= 0,
                    !(.data[["observation"]] <= 20 &
                        .data[["median"]] <= 20)) |>
      dplyr::arrange(dplyr::desc(abs(.data[["logratio_gt"]]))) |>
      dplyr::slice_head(n = 20) |>
      dplyr::mutate(outcome = paste0("incid ", .data[["outcome"]]),
                    median = round(.data[["median"]])) |>
      dplyr::select(tidyr::all_of(c("scenario_id", "state", "outcome",
                                    "ground truth" = "observation", "median",
                                    "prctdiff_gt", "ratio_gt", "logratio_gt")))

    # cumulative - underestimated cum
    proj_plot_data_calib_cum_neg <- proj_plot_data_calib |>
      dplyr::filter(.data[["type"]] == "cum", .data[["prctdiff_gt"]] < 0) |>
      dplyr::arrange(dplyr::desc(abs(.data[["logratio_gt"]]))) |>
      dplyr::slice_head(n = 40) |>
      dplyr::mutate(outcome = paste0("incid ", .data[["outcome"]]),
                    median = round(.data[["median"]])) |>
      dplyr::select(tidyr::all_of(c("scenario_id", "state", "outcome",
                                    "ground truth" = "observation", "median",
                                    "prctdiff_gt", "ratio_gt", "logratio_gt")))

    # TABLES
    ttl_w <- ": [Wk1 Projected] minus [Wk0 Reported]"
    tab_inc_num <- print_table(data = proj_plot_data_calib_inc,
                               tab_title = paste0("Incident Outliers", ttl_w),
                               metric = "median", thresholds = NA, colors = NA)
    tab_cum_num_pos <-
      print_table(data = proj_plot_data_calib_cum_pos,
                  tab_title = paste0("Cumulative Overestimates", ttl_w),
                  metric = "median", thresholds = NA, colors = NA)
    tab_cum_num_neg <-
      print_table(data = proj_plot_data_calib_cum_neg,
                  tab_title = paste0("Cumulative Underestimates", ttl_w),
                  metric = "median", thresholds = NA, colors = NA)

    ttl_w <- ": [Wk1 Projected] / [Wk0 Reported]"
    tab_inc_ratios <-
      print_table(data = proj_plot_data_calib_inc,
                  tab_title = paste0("Incident Outliers", ttl_w),
                  metric = "ratio_gt",
                  thresholds = c(0, .25, .5, 0.75, 1.333, 2, 4),
                  colors = c("red", "orange", "yellow", NA, "yellow", "orange",
                             "red"))
    tab_cum_ratios_pos <-
      print_table(data = proj_plot_data_calib_cum_pos,
                  tab_title = paste0("Cumulative Overestimates", ttl_w),
                  metric = "ratio_gt", thresholds = c(1, 1.1, 1.25),
                  colors = c("yellow", "orange", "red"))
    tab_cum_ratios_neg <-
      print_table(data = proj_plot_data_calib_cum_neg,
                  tab_title = paste0("Cumulative Underestimates", ttl_w),
                  metric = "ratio_gt", thresholds = c(0, .8, 0.91),
                  colors = c("red", "orange", "yellow"))
  }

  # Create PDF

  states_ <- unique(dplyr::filter(proj_plot_data,
                                  .data[["scenario_id"]] != "ground truth") |>
                      dplyr::pull("state"))
  if ("US" %in% states_) {
    states_ <- c("US", states_[states_ != "US"])
  }

  # INCIDENT
  p_inc <- proj_plot_data |>
    dplyr::filter(.data[["state"]] == states_[1], .data[["type"]] == "inc") |>
    plot_projections(states_[1], projection_date, legend_rows = 1,
                     y_sqrt = y_sqrt)
  p_legend <- ggpubr::get_legend(p_inc)

  pdf(save_path, width = 8.5, height = 11)

  # Tables
  if (!is.null(gs_data)) {
    gridExtra::grid.arrange(grid::textGrob(paste0("MODEL PROJECTIONS:\n",
                                                  team_model_name, "  --  ",
                                                  projection_date),
                                           gp = grid::gpar(fontsize = 14,
                                                           fontface = "bold")),
                            tab_inc_ratios, tab_cum_ratios_pos,
                            tab_cum_ratios_neg, padding = grid::unit(1, "line"),
                            nrow = 4, heights = c(0.5, 3, 2, 2))
    gridExtra::grid.arrange(tab_inc_num, tab_cum_num_pos, tab_cum_num_neg,
                            padding = grid::unit(1, "line"), nrow = 3,
                            heights = c(3, 2, 2))
  }

  # Curves
  for (st in states_) {
    p_state <- dplyr::filter(proj_plot_data, .data[["state"]] == st)
    # INCIDENT
    if (any("inc" %in% p_state$type)) {
      p_inc <- dplyr::filter(p_state, .data[["type"]] == "inc") |>
        plot_projections(st, projection_date, legend_rows = 1, y_sqrt = y_sqrt)
    } else {
      p_inc <- NULL
    }

    # CUMULATIVE
    if (any("cum" %in% p_state$type)) {
      p_cum <- dplyr::filter(p_state, .data[["type"]] == "cum") |>
        plot_projections(st, projection_date, legend_rows = 1, y_sqrt = y_sqrt)
    } else {
      p_cum <- NULL
    }

    title <- cowplot::ggdraw() +
      cowplot::draw_label(glue::glue("{st} -- {projection_date} - ",
                                     "{team_model_name}"), fontface = "bold",
                          x = 0.5, hjust = .5) +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))

    # Plot it all together
    plot_grid <- cowplot::plot_grid(p_inc +
                                      ggplot2::theme(legend.position = "none"),
                                    p_cum +
                                      ggplot2::theme(legend.position = "none"),
                                    nrow = 1)
    plot(cowplot::plot_grid(title, p_legend, plot_grid, ncol = 1,
                            rel_heights = c(.05, .1, 1)))
  }
  dev.off()
}
