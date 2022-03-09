## Plot scenarios for Checking
#gc(); rm(opt)
#
#
#
#
## OPTIONS -----------------------------------------------------------------
#
#projection_date <- "2022-01-09"
#team_model_name <- "JHU_IDD-CovidSP"
#
#y_sqrt <- FALSE
#plot_quantiles <- c(0.025, 0.975)
#
#compare_to_prev <- FALSE
#
#
#
#
## SETUP -------------------------------------------------------------------
#
#library(inference)
#library(tidyverse)
#library(cowplot)
#library(ggpubr)
#library(scales)
#library(kableExtra)
#library(gtable)
#library(gridExtra)
#library(grid)
#
## Ground truth
#pull_gt <- FALSE # pull or not
#if(pull_gt){ rm(list=ls()); pull_gt <- TRUE }
#
#
## File names
#
## projection data
#proj_data_file <- paste0("data-processed/", team_model_name, "/", projection_date, "-", team_model_name, ".csv")
## Output PDF
#plot_fname <- paste0("data-processed/", team_model_name, "/plots_", projection_date, "-", team_model_name, ".pdf")
#
### FOR PERSONAL USE ##
## projection data
#proj_data_file <- paste0("ScenarioHub/data-processed/", projection_date, "-", team_model_name, ".csv")
## Output PDF
#plot_fname <- paste0("ScenarioHub/data-processed/", "plots_", projection_date, "-", team_model_name, ".pdf")
#
#
#
## if comparing
#previous_data <- "https://raw.githubusercontent.com/midas-network/covid19-scenario-modeling-hub/master/data-processed/JHU_IDD-CovidSP/2022-01-09-JHU_IDD-CovidSP.csv"
#
#
#
## FUNCTIONS ---------------------------------------------------------------
#
#
#pull_gs_data <- function(){
#
#    # Ground Truth data
#    options(gs_targets = c(
#        "Incident Cases" = "confirmed_incidence_num",
#        "Cumulative Cases" = "confirmed_cumulative_num",
#        "Incident Hospitalizations" = "hospitalization",
#        "Cumulative Hospitalizations" = "cumulative_hospitalization",
#        "Incident Deaths" = "deaths_incidence_num",
#        "Cumulative Deaths" = "deaths_cumulative_num"
#    ))
#
#    # Get the gold standard data
#    g_targ = getOption("gs_targets")
#    #gs_path = "https://raw.githubusercontent.com/midas-network/covid19-scenario-hub_website/master/visualization/data-goldstandard/"
#    gs_path = "../covid19-scenario-hub_website/visualization/data-goldstandard/" #PATH TO THE FOLDER IN THE WEBSITE REPO
#    gs_data = lapply(g_targ[c(1,2,3,5,6)], function(x) suppressMessages(data.table::fread(paste0(gs_path,x,".csv"))))
#    # Note, gs_data will include a blank data frame for cumulative hospitalizations
#    gs_data[[names(g_targ)[4]]] = gs_data[[3]][0,]
#
#    gs_data = lapply(names(gs_data), function(x) gs_data[[x]] %>% mutate(outcome = x))
#    gs_data <- data.table::rbindlist(gs_data) %>% as_tibble()
#
#    return(gs_data)
#}
#
#
#plot_projections <- function(data, st, projection_date, legend_rows=1, y_sqrt=FALSE){
#
#    if (y_sqrt){
#        scale_y_funct <- scale_y_sqrt
#    } else {
#        scale_y_funct <- scale_y_continuous
#    }
#
#    projection_date <- lubridate::as_date(projection_date)
#
#    data %>%
#        filter(scenario_name != "ground truth") %>%
#        ggplot(aes(x = date)) +
#        geom_ribbon(aes(ymin = low, ymax = high, fill = scenario_name), alpha = 0.20) +
#        geom_line(aes(y = median, color = scenario_name), size=1.5, linetype=1) +
#        geom_line(aes(y = point, color = scenario_name), linetype=2) +
#        geom_vline(aes(xintercept=projection_date), color="grey", linetype=2, size=1.5) +
#        geom_point(data=data %>% filter(pre_gt_end==TRUE), aes(y = value_gt), color="black") +
#        geom_point(data=data %>% filter(pre_gt_end==FALSE), aes(y = value_gt), color="red") +
#        scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
#        scale_color_viridis_d("Scenario") +
#        scale_fill_viridis_d("Scenario") +
#        theme_bw() +
#        theme(legend.position = "top", legend.text = element_text(size=8),
#              axis.text.x = element_text(size=8, angle = 45, hjust=1),
#              axis.text.y = element_text(size=8)) +
#        guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
#        xlab(NULL) +
#        coord_cartesian(#ylim=c(0, max(pltdat_fc %>% pull(incD_qhigh), na.rm=TRUE)*1.1),
#            xlim=c(projection_date - 7*3, lubridate::as_date(max(data$date)))) +
#        facet_wrap(~outcome, ncol=1, scales = "free_y") +
#        scale_y_funct(glue::glue("Weekly {ifelse(data$incid_cum=='inc', 'Incident', 'Cumulative')} Outcomes, {st}"))
#
#}
#
#
#print_table<- function(data=proj_plot_data_calib_cum,
#                       tab_title = "Top Outliers: Percent difference from ground truth, Cumulative",
#                       metric = "prctdiff_gt", #"median",
#                       thresholds=c(-Inf, -.20, -.06, 0),
#                       colors=c("red", "orange", "yellow", "orange")) {
#
#
#    # Format the table
#    tab_data <- data %>%
#        select(scenario_name, state, outcome, `ground truth`, value = all_of(metric)) %>%
#        pivot_wider(names_from = scenario_name, values_from = value)
#
#    # Cells to highlight
#    NAs <- which(is.na(tab_data), arr.ind=TRUE)
#    NAs <- NAs[which(NAs[,2] %in% which(names(tab_data) %in% unique(data$scenario_name))),]
#    NAs[,1] <- NAs[,1]+1
#
#    thresh_cols <- lapply(1:length(thresholds),
#                          function(x) {
#                              tmp <- which(tab_data>=thresholds[x] &
#                                               tab_data<c(thresholds, Inf)[x+1] &
#                                               !is.na(tab_data), arr.ind=TRUE)
#                              tmp <- tmp[which(tmp[,2] %in% which(names(tab_data) %in% unique(data$scenario_name))),]
#                              tmp[,1] <- tmp[,1]+1
#                              tmp
#                          })
#
#    # Format
#
#    tab_data <- tab_data %>%
#            mutate(`ground truth` = scales::comma(`ground truth`, accuracy=1))
#
#    if (grepl("prct|percent", metric)){
#        tab_data <- tab_data %>%
#            mutate(across(all_of(unique(data$scenario_name)), ~ scales::percent(.x, accuracy=1)))
#    } else if (grepl("ratio", metric)) {
#        tab_data <- tab_data %>%
#            mutate(across(all_of(unique(data$scenario_name)), ~ round(.x,2)))
#    } else {
#        tab_data <- tab_data %>%
#            mutate(across(all_of(unique(data$scenario_name)), ~ scales::comma(.x, accuracy=1)))
#    }
#
#    # bUild the table
#
#    tt <- ttheme_default(base_size = 10)
#    g <- tableGrob(tab_data, rows = NULL, theme=tt)
#    g <- gtable_add_grob(g,
#                         grobs = rectGrob(gp = gpar(fill = NA, lwd = 2, fontsize=7)),
#                         t = 2, b = nrow(g), l = 1, r = ncol(g))
#    g <- gtable_add_grob(g,
#                         grobs = rectGrob(gp = gpar(fill = NA, lwd = 2, fontsize=7)),
#                         t = 1, l = 1, r = ncol(g))
#
#    find_cell <- function(table, row, col, name="core-fg"){
#        l <- table$layout
#        which(l$t==row & l$l==col & l$name==name)
#    }
#
#    # Highlight based on thresholds
#
#    for (i in 1:length(thresh_cols)){
#
#        if (nrow(thresh_cols[[i]])==0) next
#
#        for (x in 1:nrow(thresh_cols[[i]])){
#            ind_ <- find_cell(g, thresh_cols[[i]][x,1], thresh_cols[[i]][x,2], "core-bg")
#            g$grobs[ind_][[1]][["gp"]] <- gpar(fill=colors[i], col="lightgrey")
#        }
#        #g$grobs[ind_ft][[1]][["gp"]] <- gpar(fontsize=15, fontface="bold")
#    }
#
#    # Grey out NAs
#    for (x in 1:nrow(NAs)){
#
#        if (nrow(NAs)==0) next
#
#        ind_ <- find_cell(g, NAs[x,1], NAs[x,2], "core-fg")
#        g$grobs[ind_][[1]][["gp"]] <- gpar(col="lightgrey")
#    }
#
#
#    # Add Title
#    title <- textGrob(tab_title, gp = gpar(fontsize = 14))
#    padding <- unit(1,"line")
#    g <- gtable_add_rows(g, heights = grobHeight(title) + padding, pos = 0)
#    g <- gtable_add_grob(g, list(title), t = 1, l = 1, r = ncol(g))
#
#    g
#    #grid.draw(g)
#}
#
#
#
#
#
#
## DATA --------------------------------------------------------------------
#
## State / FIPS data
#reich_locs <- read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
#
#
## Ground Truth Data
#gt_data <- pull_gs_data() %>%
#    mutate(outcome = tolower(outcome),
#           outcome = gsub("incident", "inc", outcome),
#           outcome = gsub("cumulative", "cum", outcome)) %>%
#    select(date=time_value, location=fips, value, outcome, value) %>%
#    separate(outcome, into=c("incid_cum", "outcome"), sep=" ") %>%
#    mutate(outcome = recode(outcome, "cases"="case", "deaths"="death", "hospitalizations"="hosp")) %>%
#    mutate(pre_gt_end = date<projection_date)
#
#
#
## Projections
#
#proj_data <- read_csv(proj_data_file)
#if (compare_to_prev){
#    prev_proj_data <- read_csv(previous_data) %>%
#        mutate(scenario_name = paste0("prev_", scenario_name))
#    proj_data <- proj_data %>%
#        bind_rows(prev_proj_data)
#}
#
#
## Projections - Clean up and merge
#proj_data <- proj_data %>%
#    filter(quantile %in% c(plot_quantiles[1], 0.5, plot_quantiles[2]) | is.na(quantile)) %>%
#    mutate(quantile = ifelse(type=="point", "point", quantile)) %>%
#    separate(target, into = c("time_ahead", "time_unit", "A", "incid_cum", "outcome"), sep=" ") %>%
#    select(-A) %>%
#    rename(date = target_end_date) %>%
#    as_tibble() %>%
#    bind_rows(gt_data %>% rename(value_gt=value) %>% mutate(date = lubridate::as_date(date), scenario_name="ground truth")) %>%
#    select(scenario_id, scenario_name, location, incid_cum, outcome, date, quantile, value, value_gt, pre_gt_end) %>%
#    left_join(reich_locs %>% rename(state=abbreviation), by = "location") %>%
#    arrange(scenario_id, scenario_name, state, incid_cum, outcome, date)
#
#proj_plot_data <- proj_data %>%
#    pivot_wider(names_from = quantile, values_from = value) %>%
#    select(-`NA`)
#
## Rename the quantiles
#colnames(proj_plot_data) <- gsub(plot_quantiles[1], "low", colnames(proj_plot_data))
#colnames(proj_plot_data) <- gsub(0.5, "median", colnames(proj_plot_data))
#colnames(proj_plot_data) <- gsub(plot_quantiles[2], "high", colnames(proj_plot_data))
#
#
## Add calibration stats
#proj_plot_data_calib <- proj_plot_data %>%
#    filter(scenario_name!="ground truth" & date==(lubridate::as_date(projection_date)+6)) %>%
#    select(-value_gt) %>%
#    full_join(proj_plot_data %>%
#                  filter(scenario_name=="ground truth", !is.na(value_gt), pre_gt_end) %>%
#                  filter(date==max(date)) %>%
#                  select(location, state, incid_cum, outcome, value_gt)) %>%
#    mutate(diff_gt = round(median - value_gt),
#           prctdiff_gt = round(diff_gt / value_gt,2),
#           ratio_gt = round(median / value_gt,4),
#           logratio_gt = log(ratio_gt))
#
#
## Max differences
#
## Incident
#proj_plot_data_calib_inc <- proj_plot_data_calib %>%
#    filter(incid_cum=="inc") %>%
#    filter(!((value_gt<=20 & median<=20))) %>%
#    #arrange(desc(abs(prctdiff_gt))) %>%
#    arrange(desc(abs(logratio_gt))) %>%
#    slice_head(n=30) %>%
#    mutate(outcome = paste0('incid ', .$outcome), median = round(median)) %>%
#    select(scenario_name, state, outcome, `ground truth`=value_gt, median, prctdiff_gt, ratio_gt, logratio_gt)
#
## cumulative - over estimated cum
#proj_plot_data_calib_cum_pos <- proj_plot_data_calib %>%
#    filter(prctdiff_gt>=0) %>%
#    filter(incid_cum=="cum") %>%
#    filter(!((value_gt<=20 & median<=20))) %>%
#    #arrange(desc(prctdiff_gt)) %>%
#    arrange(desc(abs(logratio_gt))) %>%
#    slice_head(n=20)%>%
#    mutate(outcome = paste0('incid ', .$outcome), median = round(median)) %>%
#    select(scenario_name, state, outcome, `ground truth`=value_gt, median, prctdiff_gt, ratio_gt, logratio_gt)
#
## cumulative - underestimated cum
#proj_plot_data_calib_cum_neg <- proj_plot_data_calib %>%
#    filter(prctdiff_gt<0) %>%
#    filter(incid_cum=="cum") %>%
#    #arrange(desc(abs(prctdiff_gt))) %>%
#    arrange(desc(abs(logratio_gt))) %>%
#    slice_head(n=40) %>%
#    mutate(outcome = paste0('incid ', .$outcome), median = round(median)) %>%
#    select(scenario_name, state, outcome, `ground truth`=value_gt, median, prctdiff_gt, ratio_gt, logratio_gt)
#
#
#
#
## TABLES ------------------------------------------------------------------
##   - See for examples: https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
#
#
#
## tab_inc_prct <- print_table(data=proj_plot_data_calib_inc,
##                             tab_title = "Incident Outliers: Percent difference from Wk1 Projection to Wk0 Reported",
##                             metric = "prctdiff_gt",
##                             thresholds=c(-Inf, -3, -2, 1, 2, 3),
##                             colors=c("red", "orange", "yellow", "yellow", "orange", "red"))
## #summary(proj_plot_data_calib_cum_pos$prctdiff_gt)
## tab_cum_prct_pos <- print_table(data=proj_plot_data_calib_cum_pos,
##                                 tab_title = "Cumulative Overestimates: Percent difference from Wk1 Projection to Wk0 Reported",
##                                 metric = "prctdiff_gt",
##                                 thresholds=c(-Inf, -.20, -.06, 0),
##                                 colors=c("red", "orange", "yellow", "orange"))
## #summary(proj_plot_data_calib_cum$prctdiff_gt)
## tab_cum_prct_neg <- print_table(data=proj_plot_data_calib_cum_neg,
##                                 tab_title = "Cumulative Underestimates: Percent difference from Wk1 Projection to Wk0 Reported",
##                                 metric = "prctdiff_gt",
##                                 thresholds=c(-Inf, -.20, -.06, 0),
##                                 colors=c("red", "orange", "yellow", "orange"))
#
#
##summary(proj_plot_data_calib_inc$median)
#tab_inc_num <- print_table(data=proj_plot_data_calib_inc,
#                           tab_title = "Incident Outliers: [Wk1 Projected] minus [Wk0 Reported]",
#                           metric = "median",
#                           thresholds=NA,
#                           colors=NA)
##summary(proj_plot_data_calib_cum$median)
#tab_cum_num_pos <- print_table(data=proj_plot_data_calib_cum_pos,
#                               tab_title = "Cumulative Overestimates: [Wk1 Projected] minus [Wk0 Reported]",
#                               metric = "median",
#                               thresholds=NA,
#                               colors=NA)
#tab_cum_num_neg <- print_table(data=proj_plot_data_calib_cum_neg,
#                               tab_title = "Cumulative Underestimates: [Wk1 Projected] minus [Wk0 Reported]",
#                               metric = "median",
#                               thresholds=NA,
#                               colors=NA)
#
#
## summary(proj_plot_data_calib_inc$logratio_gt)
## exp(summary(proj_plot_data_calib_inc$logratio_gt))
#tab_inc_ratios <- print_table(data=proj_plot_data_calib_inc,
#                             tab_title = "Incident Outliers: [Wk1 Projected] / [Wk0 Reported]",
#                                 metric = "ratio_gt",
#                                 thresholds=c(0, .25, .5, 0.75, 1.333, 2, 4),
#                                 colors=c("red", "orange", "yellow", NA, "yellow", "orange", "red"))
#
## summary(proj_plot_data_calib_cum_pos$logratio_gt)
## exp(summary(proj_plot_data_calib_cum_pos$logratio_gt))
#tab_cum_ratios_pos <- print_table(data=proj_plot_data_calib_cum_pos,
#                                 tab_title = "Cumulative Overestimates: [Wk1 Projected] / [Wk0 Reported]",
#                                 metric = "ratio_gt",
#                                 thresholds=c(1, 1.1, 1.25),
#                                 colors=c("yellow", "orange", "red"))
## summary(proj_plot_data_calib_cum_neg$logratio_gt)
## exp(summary(proj_plot_data_calib_cum_neg$logratio_gt))
#tab_cum_ratios_neg <- print_table(data=proj_plot_data_calib_cum_neg,
#                                 tab_title = "Cumulative Underestimates: [Wk1 Projected] / [Wk0 Reported]",
#                                 metric = "ratio_gt",
#                                 thresholds=c(0, .8, 0.91),
#                                 colors=c("red", "orange", "yellow"))
#
## grid.newpage()
##
##
##
## grid.arrange(
##     textGrob(paste0("MODEL PROJECTIONS:\n", team_model_name, "  --  ", projection_date),
##              gp = gpar(fontsize = 18, fontface="bold")),
##     tab_inc_ratios,
##     tab_cum_ratios_pos,
##     tab_cum_ratios_neg,
##     padding = unit(1, "line"),
##     nrow=4,
##     heights = c(1,3,2,2))
##
## # grid.arrange(
## #     textGrob(paste0("MODEL PROJECTIONS:\n", team_model_name, "  --  ", projection_date),
## #              gp = gpar(fontsize = 18, fontface="bold")),
## #     tab_inc_prct,
## #     tab_cum_prct,
## #     padding = unit(1, "line"), heights = c(1,3,3))
##
## grid.arrange(
##     tab_inc_num,
##     tab_cum_num_pos,
##     tab_cum_num_neg,
##     padding = unit(1, "line"),
##     nrow=3,
##     heights = c(3,2,2))
#
#
#
## NATIONAL PLOT -----------------------------------------------------------
#
## st <- "US"
##
## # INCIDENT
## proj_plot_data %>%
##     filter(state==st) %>%
##     filter(incid_cum=="inc") %>%
##     plot_projections(st, projection_date, legend_rows=1, y_sqrt=FALSE)
##
## # CUMULATIVE
## proj_plot_data %>%
##     filter(state==st) %>%
##     filter(incid_cum=="cum") %>%
##     plot_projections(st, projection_date, legend_rows=1, y_sqrt=FALSE)
##
#
#
## STATE PLOTS -------------------------------------------------------------
#
#states_ <- unique(proj_plot_data %>% filter(scenario_name != "ground truth") %>% pull(state))
#states_ <- c("US", states_[states_!="US"])
#
## INCIDENT
#p_inc <- proj_plot_data %>%
#    filter(state==states_[1]) %>%
#    filter(incid_cum=="inc") %>%
#    plot_projections(st, projection_date, legend_rows=1, y_sqrt=FALSE)
#p_legend <- ggpubr::get_legend(p_inc)
#
#
#
#
#pdf(plot_fname, width=8.5, height=11)
#
## Tables
#grid.arrange(
#    textGrob(paste0("MODEL PROJECTIONS:\n", team_model_name, "  --  ", projection_date),
#             gp = gpar(fontsize = 14, fontface="bold")),
#    tab_inc_ratios,
#    tab_cum_ratios_pos,
#    tab_cum_ratios_neg,
#    padding = unit(1, "line"),
#    nrow=4,
#    heights = c(0.5,3,2,2))
#
#grid.arrange(
#    tab_inc_num,
#    tab_cum_num_pos,
#    tab_cum_num_neg,
#    padding = unit(1, "line"),
#    nrow=3,
#    heights = c(3,2,2))
#
## Curves
#
#for(st in states_){
#    # INCIDENT
#    p_inc <- proj_plot_data %>%
#        filter(state==st) %>%
#        filter(incid_cum=="inc") %>%
#        plot_projections(st, projection_date, legend_rows=1, y_sqrt=FALSE)
#
#    # CUMULATIVE
#    p_cum <- proj_plot_data %>%
#        filter(state==st) %>%
#        filter(incid_cum=="cum") %>%
#        plot_projections(st, projection_date, legend_rows=1, y_sqrt=FALSE)
#
#    title <- ggdraw() +
#        draw_label(glue::glue("{st} -- {projection_date} - {team_model_name}"), fontface = 'bold', x = 0.5, hjust = .5) +
#        theme(plot.margin = margin(0, 0, 0, 0))
#
#    # Plot it all together
#    plot(cowplot::plot_grid(
#        title,
#        p_legend,
#        cowplot::plot_grid(
#            p_inc + theme(legend.position = "none"),
#            p_cum + theme(legend.position = "none"),
#            nrow=1),
#        ncol=1, rel_heights = c(.05,.1, 1)))
#}
#dev.off()
#
#
