# Helper functions
# Author: Danni Tu
# Updated: February 10, 2021


# RMarkdown Formatting ----------------------------------------------------

# New Line
nl <- function(){
  cat("  \n")
}

# Print subheading from within R code chunk
subheading <- function(text, lvl = 2){
  cat(paste0(paste0(rep("#", times = lvl), collapse = ""), " ", text))
  nl()
}

# Round a variable to exactly n decimal places
round2 <- function(x, digits = 0) trimws(format(round(x, digits), nsmall=digits))

pt <- function(tab){
  pandoc.table(tab, split.table = Inf)
}

# Rounding and formatting tables
roundf <- function(num, digits = 2){
  format(round(x = num, digits = digits), nsmall = digits)
}

# Data Processing ---------------------------------------------------------

# For a vector x, do LOCF (last-value-carried-forward)
# imputation. If x begins with NA's, carry the first non-NA
# observation backward also
# e.g. na.locf2(c(NA, 1, 2, NA)) = c(1,1,2,2)
na.locf2 <- function(x){
  x_first = x[!is.na(x)][1]
  x_out = na.locf(x, na.rm = FALSE, fromLast = FALSE)
  x_out[is.na(x_out)] = x_first
  
  return(x_out)
}

# Wrapping around the lag function, setting 0 as the default value instead of NA
lag0 <- function(...){
  lag(..., default = 0)
}

# For a given vector, delete entries that are repeated more than nreps times in a row
# and replace these values with NA
delete_reps <- function(x, nreps = 3){
  # Get the run length encoding of x
  rle_x = rle(x)
  # Identify indices of x that have been repeated >= 3 times
  ind = rep(rle_x$lengths >= nreps,times = rle_x$lengths)
  # Replace with NA
  x[ind] <- as.numeric(NA)
  
  return(x)
}

# Smooth environmental data, replace missing values with the smoothed value
# dat0 = RST data
# var = name of environmental variable
# smooth.dir = location of the LOESS/linear interpolation objects
smooth_fun <- function(dat0, var, day_level = TRUE, smooth.dir,
                       return_loess = FALSE){
  stopifnot(var %in% c("Radiation_Dose", "Noise_dBA", "Temp", "CO2", "ppO2"))
  
  snip = ifelse(day_level, "day", "hr")
  
  if (var == "Radiation_Dose"){
    # Read in smooth object
    smooth_obj = readRDS(file.path(smooth.dir, "smooth_rad.RDS"))
    
    # Add to data
    smooth_dat <- dat0 %>%
      mutate(Radiation_Dose_S = predict(smooth_obj, dat0),
             Radiation_Dose_CS = ifelse(is.na(Radiation_Dose), Radiation_Dose_S, Radiation_Dose))
  }
  
  if (var == "Temp"){
    # Read in smooth object
    smooth_obj = list(avg = readRDS(file.path(smooth.dir, paste0("smooth_temp_avg_", snip, ".RDS"))),
                      no2 = readRDS(file.path(smooth.dir, paste0("smooth_temp_no2_", snip, ".RDS"))),
                      lab = readRDS(file.path(smooth.dir, paste0("smooth_temp_lab_", snip, ".RDS"))))
    
    # Day-level 
    if (day_level){
      # Predictions for each location
      preds <- data.frame(Avg = predict(smooth_obj$avg, dat0),
                          No2 = predict(smooth_obj$no2, dat0),
                          Lab = predict(smooth_obj$lab, dat0))
      
      smooth_dat <- dat0 %>%
        # Add columns of preds
        cbind(preds) %>%
        mutate(Temp_S = ifelse(Location == "Node 2", No2, as.numeric(NA)),
               Temp_S = ifelse(Location == "US Lab", Lab, Temp_S),
               Temp_S = ifelse(Location == "Avg", Avg, Temp_S),
               # Only interpolate if Temp is NA
               Temp_CS = ifelse(is.na(Temp), Temp_S, Temp))
    } else {
      # Hourly level
      # Smoothed temp
      tmp <- dat0 %>% select(Time_hour2) %>% distinct
      
      smooth_obj = list(avg = readRDS(file.path(smooth.path, paste0("smooth_temp_avg_hr.RDS"))),
                        no2 = readRDS(file.path(smooth.path, paste0("smooth_temp_no2_hr.RDS"))),
                        lab = readRDS(file.path(smooth.path, paste0("smooth_temp_lab_hr.RDS"))))
      
      preds <- data.frame(Avg = predict(smooth_obj$avg, tmp),
                          No2 = predict(smooth_obj$no2, tmp),
                          Lab = predict(smooth_obj$lab, tmp),
                          Time_hour2 = tmp$Time_hour2) %>%
        gather(Location, Temp_S, -Time_hour2) %>%
        mutate(Location = recode(Location, "No2" = "Node 2", "Lab" = "US Lab"))
      
      smooth_dat <- dat0 %>%
        left_join(preds, by = c("Time_hour2", "Location")) %>%
        mutate(Temp_CS = ifelse(is.na(Temp), Temp_S, Temp))
      
    }
  }
  
  if (var == "ppO2"){
    # Read in smooth object
    smooth_obj = readRDS(file.path(smooth.dir, paste0("smooth_ppo2_", snip, ".RDS")))
    
    # Add to data
    smooth_dat <- dat0 %>%
      mutate(ppO2_S = predict(smooth_obj, dat0),
             ppO2_CS = ifelse(is.na(ppO2), ppO2_S, ppO2))
  }
  
  if (var == "CO2"){
    # Read in smooth object
    smooth_obj = readRDS(file.path(smooth.dir, paste0("smooth_co2_", snip, ".RDS")))
    
    # Add to data
    smooth_dat <- dat0 %>%
      mutate(CO2_S = predict(smooth_obj, dat0),
             CO2_CS = ifelse(is.na(CO2), CO2_S, CO2))
  }
  
  if (var == "Noise_dBA"){
    # Read in smooth object
    smooth_obj = list(Day = readRDS(file.path(smooth.dir, paste0("smooth_noise_", snip, "1.RDS"))),
                      Night = readRDS(file.path(smooth.dir, paste0("smooth_noise_", snip, "2.RDS"))))
    
    # Add to data
    if (day_level){
      smooth_dat <- dat0 %>%
        mutate(Noise_dBA_S = ifelse(Noise_Day, smooth_obj$Day(Time_day), smooth_obj$Night(Time_day)),
               Noise_dBA_CS = ifelse(is.na(Noise_dBA), Noise_dBA_S, Noise_dBA))
    } else {
      smooth_dat <- dat0 %>%
        mutate(Noise_dBA_S = ifelse(Noise_Day, smooth_obj$Day(Time_hour), smooth_obj$Night(Time_hour)),
               Noise_dBA_CS = ifelse(is.na(Noise_dBA), Noise_dBA_S, Noise_dBA))
    }
  }
  
  
  if (return_loess){
    return(list(smooth_obj = smooth_obj, smooth_dat = smooth_dat))
  } else {
    return(smooth_dat)
  }
  
}

# Variable Names ----------------------------------------------------------

# Get variable label, given vector of variable names
get_label <- function(varnames){
  x = data.frame(Var = varnames) %>%
    left_join(vars_and_labels, by = "Var") %>%
    mutate_all(as.character) %>%
    mutate(Label = ifelse(is.na(Label), Var, Label))
  return(as.character(x$Label))
}

# Get lagged outcome variable, given name
get_lagged <- function(outcome){
  # Obtain name of lagged outcome variable
  outcome_lag = switch(outcome,
                       "Overall_Performance" = "Overall_Performance_L1",
                       "LRM_50" = "LRM_50_L1", 
                       "PVT_Z" = "PVT_Z_L1",
                       "LRM_Z" = "LRM_Z_L1",
                       "MeanRRT" = "MeanRRT_L1",
                       "Errors_of_omission" = "Errors_Omission_L1", 
                       "Errors_of_comission" = "Errors_Comission_L1")
  
  return(outcome_lag)
}

# Get the short version of a variable name (for filenames)
get_snippet <- function(outcome){
  outcome_snip <- switch(outcome,
         "Overall_Performance" = "o",
         "LRM_50" = "l",
         "LRM_Z" = "lz",
         "LRM_Z_R" = "lzr",
         "PVT_Z" = "pz")
  return(outcome_snip)
}


# Unlagged covariates
covar_unlag = c("Radiation_Dose_CS", "Noise_dBA_CS", "CO2_CS", "ppO2_CS", 
                "ISS_Occupants", "Temp_CS", "Male", "Age_at_Dock", "Pre_PVT", 
                "Med_SleepAid", "Med_Antihist", "Med_Pain", "Med_Decongest",
                "Test_Track_Type","Self_PC_1","Low_Workload","Poor_Sleep_Quality","Very_stressed",
                "Total_sleep_hrs","Total_sleep_missed", "Pred_Lapses_CS", "EVA_Today", 
                "EVA_Tomorrow", "Caffeine")

# Covariates we'll keep for modelling
covar_mod <-  c("Radiation_Dose_CS", "Noise_dBA_CS", "CO2_CS", "ppO2_CS", 
                "ISS_Occupants", "Temp_CS", "Male", "Age_at_Dock",
                "Pre_PVT", "Med_SleepAid", "Med_Antihist",
                "Test_Track_Type","Self_PC_1","Low_Workload","Poor_Sleep_Quality","Very_stressed",
                "Total_sleep_hrs", "Total_sleep_missed", "Pred_Lapses_CS",
                "Caffeine")

# Non-time-varying covariates
covar_nontv = c("Male", "Age_at_Dock", "Pre_PVT", "Test_Track_Type")

# Of covar_mod, get factors and numerics
covar_mod_fac = c("Male", "Med_SleepAid", "Med_Antihist", "Test_Track_Type")
covar_mod_num = setdiff(covar_mod, covar_mod_fac)

# Model Building ----------------------------------------------------------

# Build formula object for linear model
formula_linear <- function(outcome = "Overall_Performance", covariates){
  form0 = paste0(covariates, collapse = " + ")
  form = paste0(outcome, " ~ ", form0)
  return(as.formula(form))
}

# Build formulat object for functional concurrent model
# using bivariate smooth terms
formula_fcr <- function(outcome = "LRM_50",
                         covariates,
                         time_var = "inflight_time", K){
  
  # Glue together terms with time-varying effects
  cov_tv = setdiff(covariates, covar_nontv)
  form_tv0 = paste0("s(", time_var, ", ", cov_tv, ", k = ", K, ", bs = 'tp')")
  form_tv = form_tv0 %>%
    paste0(collapse = " + ")
  
  # Glue together terms with non-time-varying effects
  cov_ntv = intersect(covariates, covar_nontv)
  if (length(cov_ntv > 0)){
    form_ntv = paste0(cov_ntv, collapse = " + ")
    form0 = paste0(form_tv, " + ", form_ntv) 
  } else {
    form0 = form_tv
  }
  
  form = paste0(outcome, # Outcome
                # Random effect
                " ~ s(", 
                time_var, 
                ", k = ", K, ", bs = 're') + ",
                # Covariates
                form0) 
  return(as.formula(form))
}

# Variable Selection ------------------------------------------------------

# dat: data set
# samp_frac: the fraction used to split data into Train/Test sets
# outcome = outcome variable
# covars = vector of predictors' variable names
forest_rank_table <- function(n, dat = rst.smooth, samp_frac = 0.5,
                              outcome = "LRM_50", covars = covar_unlag){
  
  # Get varname of lagged outcome
  outcome_lag <- get_lagged(outcome)
  
  # Subsample data by fraction of samp_frac
  dat2 <- dat %>%
    sample_frac(samp_frac)
  
  # Random Forest importance table
  rf = randomForest(formula_linear(outcome = outcome,  covariates = c(covars, outcome_lag)), 
                    data = dat2, importance = TRUE)
  
  rank_table <- rf %>%
    importance() %>%
    as.data.frame() %>%
    rownames_to_column("Variable") %>%
    arrange(desc(`%IncMSE`)) %>%
    mutate(Rank = 1:n()) %>%
    select(Variable, Rank)
  
  rank_table <- rank_table %>%
    mutate(Top10 = (Rank <= 10),
           Top15 = (Rank <= 15),
           Sim = n)
  return(rank_table)
  
}

# n_sum: number of times to run forest_rank_table()
# save.dir: optional location to save the outcome of the simulations
rank_fun_sim <- function(n_sum = 100, 
                         dat = rst.smooth, 
                         samp_frac = 0.5,
                         outcome = "LRM_50", 
                         covars = covar_unlag,
                         save.dir = sel.path){
  # String associated with outcome
  outcome3 <- get_snippet(outcome)
  
  # Ranks of each variable
  ranks = parallel::mclapply(X = as.list(1:n_sum),
                             FUN = forest_rank_table, dat = dat, samp_frac = samp_frac,
                             outcome = outcome, covars = covars, mc.cores = n_cores) %>%
    Reduce(rbind,.)
  
  # Save all of the ranks
  fname = paste0("varselect_forest_", outcome3, ".rds")
  saveRDS(ranks, file.path(save.dir, fname))
  
  # Get the output table
  fname2 = paste0("varselect_forest_", outcome3, "_tab.rds")
  ranks_table <- ranks %>%
    group_by(Variable) %>%
    summarize(Top10rate = mean(Top10),
              Top15rate = mean(Top15),
              TopRank = min(Rank),
              MeanRank = mean(Rank),
              MedRank = median(Rank),
              BottomRank = max(Rank)) %>%
    arrange(desc(Top10rate))
  saveRDS(ranks_table,
          file.path(save.dir, fname2))
}


# Plot the rankings
rank_fun_plot <- function(dat_rank){
  dat_rank <- dat_rank %>%
    left_join(vars_and_labels, by = c("Variable" = "Var")) %>%
    dplyr::select(-Variable) %>%
    dplyr::select(Variable = Label, everything())
  
  var_levels = dat_rank %>% 
    arrange(Sim, Rank) %>% 
    dplyr::select(Variable) %>% 
    distinct %>% `[[`(1)
  
  rank_levels = rev(rev(unique(dat_rank$Rank)))
  
  dat_rank <- dat_rank %>%
    mutate(Variable = factor(Variable, levels = var_levels))
  p <- ggplot(dat_rank) +
      geom_line(aes(x = Sim, y = Rank, color = Variable, group = Variable)) +
      scale_y_continuous(trans = "reverse", breaks = unique(dat_rank$Rank)) +
      labs(x = "Simulation",
           y = "Rank (Higher Better)")
  
  print(p)
    # ggplotly(p)
}


# FCR contour plot --------------------------------------------------------

# Create fcr grid object
fcr_grid_obj <- function(preds = "full", covars, outcome = "LRM_50"){
  # Obtain name of lagged outcome variable
  outcome_lag = get_lagged(outcome)
  outcome_snippet = get_snippet(outcome)
  
  # Fit FCR given the covariates and outcome
  form = formula_fcr(outcome = outcome, covariates = c(covars, outcome_lag),
                     time_var = "inflight_time", K = 10)
  
  fit_fcr = do.call(fcr, list(formula = form, argvals = "inflight_time", 
                              subj = "subj", data = as.data.frame(rst.smooth),
                              argvals.new = time_grid,
                              use_bam = TRUE, nPhi = 1))
  saveRDS(fit_fcr, 
          file.path(mod.path.final, 
                    paste0("fcr_grid_", preds, "_", outcome_snippet, ".RDS")))
}

# Plot colors (code from https://github.com/tidyverse/ggplot2/issues/4088)
# modified brewer_pal()
craftbrewer_pal <- function (type = "seq", palette = 1, direction = 1) {
  pal <- scales:::pal_name(palette, type)
  force(direction)
  function(n) {
    n_max_palette <- RColorBrewer:::maxcolors[names(RColorBrewer:::maxcolors) == palette]
    
    if (n < 3) {
      pal <- suppressWarnings(RColorBrewer::brewer.pal(n, pal))
    } else if (n > n_max_palette){
      rlang::warn(paste(n, "colours used, but", palette, "has only",
                        n_max_palette, "- New palette created based on all colors of", 
                        palette))
      n_palette <- RColorBrewer::brewer.pal(n_max_palette, palette)
      colfunc <- grDevices::colorRampPalette(n_palette)
      pal <- colfunc(n)
    }
    else {
      pal <- RColorBrewer::brewer.pal(n, pal)
    }
    pal <- pal[seq_len(n)]
    if (direction == -1) {
      pal <- rev(pal)
    }
    pal
  }
}

# modified scale_fill_fermenter

scale_fill_craftfermenter <- function(..., type = "seq", palette = 1, direction = -1, na.value = "grey50", guide = "coloursteps", aesthetics = "fill") {
  type <- match.arg(type, c("seq", "div", "qual"))
  if (type == "qual") {
    warn("Using a discrete colour palette in a binned scale.\n  Consider using type = \"seq\" or type = \"div\" instead")
  }
  ggplot2:::binned_scale(aesthetics, "craftfermenter", ggplot2:::binned_pal(craftbrewer_pal(type, palette, direction)), na.value = na.value, guide = guide, ...)
}

# Given a fitted FCR model, plot the heatmaps of the predicted outcome
# by time and a selected variable ("var1")
# Inputs:
# fcr_obj = fitted FCR grid object created by fcr_grid_obj()
# dat0 = data used to fit fcr_obj
# covs = covariates used to fit fcr_obj
# var1 = name of variable to be plotted
fcr_heatmap_1var <- function(dat0 = rst.smooth,
                             fcr_obj, 
                             outcome = "LRM_50",
                             covars = covar_mod,
                             var1 = "Radiation_Dose_CS",
                             lrm_lims = NULL, leg_pos = "bottom",
                             txt_size = 10){
  
  # Ensure the outcome/covariates were in the fcr object
  the_form = as.character(fcr_obj$fit$formula)
  stopifnot(grepl(pattern = outcome, x = the_form[2]))
  stopifnot(all(unlist(covars %>% map(grepl,x = the_form[3]))))
  
  n_round = if (outcome == "LRM_50") 1 else 2
  
  # Obtain outcome label, lagged outcome name, var label
  out_name = get_label(outcome)
  outcome_lag = get_lagged(outcome)
  var_name = get_label(var1)
  
  # Grid of all time/variable combinations
  time_grid = seq(from = 0, to = 1, length.out = 50)
  var_grid = seq(from = min(dat0[,var1]),
                 to = max(dat0[,var1]),
                 length.out = 50)
  
  # Create fake data
  # All other covariates - obtain average value
  other_covars = setdiff(covars, c(var1, "Subject_ID", "subj", "Male", "Test_Track_Type"))
  
  # dat_grid <- expand.grid(subj_grid, time_grid, var_grid) %>%
  #   `colnames<-`(c("Subject_ID", "inflight_time", var1)) %>%
  #   arrange(Subject_ID, inflight_time)
  
  dat_grid <- expand.grid(time_grid, var_grid) %>%
    `colnames<-`(c("inflight_time", var1)) %>%
    mutate(Subject_ID = 1001)
  
  # Get the average covariates
  dat_fake0 <- dat0 %>%
    dplyr::select(Subject_ID, subj, Male, Test_Track_Type, 
                  all_of(c(other_covars, outcome_lag)))
  
  dat_fake <- dat_fake0 %>%
    # First average over all subjects/Male/Test Track Type
    group_by(Subject_ID, subj, Male, Test_Track_Type) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup %>%
    # Then take average over all subjects
    dplyr::select(-Subject_ID, -subj) %>%
    group_by(Male, Test_Track_Type) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    mutate(Subject_ID = 1001,
           subj = factor(Subject_ID)) %>%
    inner_join(dat_grid, by = "Subject_ID") %>% ungroup %>%
    filter(Male == TRUE,
           Test_Track_Type == "Evening")
  
  # Predict outcome on the fake data
  fcr_preds = predict(fcr_obj, newdata = dat_fake)$insample_predictions
  
  # Data for plots
  plotvar2 = sym(var1)
  dat_plot <- dat_fake %>%
    dplyr::select(all_of(var1), inflight_time) %>%
    mutate(Pred = fcr_preds) %>%
    group_by(!!plotvar2, inflight_time) %>%
    summarize(Pred = mean(Pred, na.rm = TRUE))
  
  # dat_plot
  
  # Plot
  
  # Define breaks using deciles of the predicted outcome
  if (!missing(lrm_lims)){
    stopifnot(length(lrm_lims) == 2)
    mybreaks <- c(-Inf, seq(from = lrm_lims[1], to = lrm_lims[2], length.out = 12), Inf)
  } else {
    mybreaks <- c(-Inf, quantile(dat_plot$Pred, prob = seq(0.1, 0.9, 0.1)), Inf)
  }
  names(mybreaks) <- NULL 
  
  p <- ggplot(dat_plot, aes_string(var1, "inflight_time", z = "Pred")) +
    metR::geom_contour_fill() +
    scale_fill_viridis_b(breaks = round(mybreaks,n_round),
                         guide = guide_colorsteps(
                           frame.colour = "black",
                           ticks.colour = "black",
                           barwidth=20),
                         direction = -1) +
    theme(legend.position = leg_pos,
          text = element_text(size = txt_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    labs(y = "Inflight Time",
         x = var_name,
         fill = paste0("Predicted\n", out_name))
  
  print(p)
}

# fcr_obj = fitted FCR object on the given 
fcr_heatmap_2var <- function(dat0 = rst.smooth,
                             fcr_obj, 
                             outcome = "LRM_50",
                             covars = covar_mod,
                             var1 = "Radiation_Dose_CS",
                             var2 = "Temp_CS",
                             # Time index: integer between 1 and 50
                             time_index = 1,
                             leg_pos = "bottom", txt_size = 10){
  
  n_round = if (outcome == "LRM_50") 1 else 2
  
  # Ensure the outcome/covariates were in the fcr object
  the_form = as.character(fcr_obj$fit$formula)
  stopifnot(grepl(pattern = outcome, x = the_form[2]))
  stopifnot(all(unlist(c(var1, var2, covars) %>% map(grepl,x = the_form[3]))))
  
  # Obtain name of lagged outcome variable
  outcome_lag = get_lagged(outcome)
  
  # Obtain nice name of outcome variable
  out_name = get_label(outcome)
  var_name1 = get_label(var1)
  var_name2 = get_label(var2)
  
  # Grid of all time/variable combinations
  time_grid = seq(from = 0, to = 1, length.out = 50)
  the_time = time_grid[time_index]
  var1_grid = seq(from = min(dat0[,var1]),
                  to = max(dat0[,var1]),
                  length.out = 50)
  var2_grid = seq(from = min(dat0[,var2]),
                  to = max(dat0[,var2]),
                  length.out = 50)
  
  # Create fake data
  # All other covariates - obtain average value
  other_covars = setdiff(covars, c(var1, var2, "Subject_ID", "subj", "Male", "Test_Track_Type"))
  
  # dat_grid <- expand.grid(subj_grid, time_grid, var_grid) %>%
  #   `colnames<-`(c("Subject_ID", "inflight_time", var1)) %>%
  #   arrange(Subject_ID, inflight_time)
  
  dat_grid <- expand.grid(var1_grid, var2_grid) %>%
    `colnames<-`(c(var1, var2)) %>%
    mutate(Subject_ID = 1001,
           inflight_time = the_time)
  
  # Get the average covariates
  dat_fake0 <- dat0 %>%
    dplyr::select(Subject_ID, subj, Male, Test_Track_Type, 
                  all_of(c(other_covars, outcome_lag)))
  
  dat_fake <- dat_fake0 %>%
    # First average over all subjects/Male/Test Track Type
    group_by(Subject_ID, subj, Male, Test_Track_Type) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup %>%
    # Then take average over all subjects
    dplyr::select(-Subject_ID, -subj) %>%
    group_by(Male, Test_Track_Type) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    mutate(Subject_ID = 1001,
           subj = factor(Subject_ID)) %>%
    inner_join(dat_grid, by = "Subject_ID") %>% ungroup %>%
    filter(Male == TRUE,
           Test_Track_Type == "Evening")
  
  # Predict outcome on the fake data
  fcr_preds = predict(fcr_obj, newdata = dat_fake)$insample_predictions
  
  # Data for plots
  plotvar1 = sym(var1)
  plotvar2 = sym(var2)
  
  # Get unique predicted outome for each combo of var1, var2
  dat_plot <- dat_fake %>%
    dplyr::select(all_of(c(var1, var2)), inflight_time) %>%
    mutate(Pred = fcr_preds)# %>%
  #group_by(!!plotvar1, !!plotvar2, inflight_time) %>%
  #summarize(Pred = mean(Pred, na.rm = TRUE))
  
  # Plot
  # Define breaks using deciles of the predicted outcome
  mybreaks <- c(-Inf, quantile(dat_plot$Pred, prob = seq(0.1, 0.9, 0.1)), Inf)
  names(mybreaks) <- NULL 
  
  p <- ggplot(dat_plot, aes_string(var1, var2, z = "Pred")) +
    metR::geom_contour_fill() +
    scale_fill_viridis_b(breaks = round(mybreaks,n_round),
                         guide = guide_colorsteps(
                           frame.colour = "black",
                           ticks.colour = "black",
                           barwidth=20),
                         direction = -1) +
    theme(legend.position = leg_pos,
          text = element_text(size = txt_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()) +
    labs(y = var_name2,
         x = var_name1,
         fill = paste0("Predicted\n", out_name))
  
  print(p)
}

smooth_env_plot <- function(rst.full, var = "Noise_dBA", smooth.dir = smooth.path){
  
  # For noise, plot Day/Night Separately
  if (var == "Noise_dBA"){
    cnames = c("Time_hour", "Noise_Day", var, paste0(var, "_CS"))
    var_smoothed = smooth_fun(dat0 = rst.full, var = "Noise_dBA", 
                              day_level = FALSE, smooth.dir = smooth.path,
                              return_loess = TRUE)
    
    var_smoothed_dat <- var_smoothed$smooth_dat %>%
      filter(Time_Type == "Flight") %>%
      dplyr::select(all_of(cnames)) %>%
      `colnames<-`(c("Time", "Noise_Day", "Original", "Interpolated"))
    
    # Points: the observed data 
    fig1_obs = var_smoothed_dat %>%
      filter(!is.na(Original)) %>%
      select(Time, Noise_Day, Value = Original) %>%
      mutate(Type = "Original")
    
    # Crosses: the interpolated data
    fig1_int =  var_smoothed_dat %>%
      filter(is.na(Original)) %>%
      select(Time, Noise_Day, Value = Interpolated) %>%
      mutate(Type = "Interpolated")
    
    # Line: the LOESS fit
    day_ind = which(var_smoothed_dat$Noise_Day)
    plot_dates_day = sort(c(fig1_obs$Time[day_ind], fig1_int$Time[day_ind]))
    plot_dates_night = sort(c(fig1_obs$Time[-day_ind], fig1_int$Time[-day_ind]))
    
    fig1_loess_day = data.frame(Time = plot_dates_day,
                                Noise_Day = TRUE,
                                Value = var_smoothed$smooth_obj$Day(plot_dates_day),
                                Type = "Smooth Fit")
    
    fig1_loess_night = data.frame(Time = plot_dates_night,
                                  Noise_Day = FALSE,
                                  Value = var_smoothed$smooth_obj$Night(plot_dates_night),
                                  Type = "Smooth Fit")
    
    fig1_dat <- rbind(fig1_obs, fig1_int, fig1_loess_day, fig1_loess_night)
    
    
    # Plot
    var_label = get_label(var)
    
    # Get limits
    y1 = min(fig1_dat$Value, na.rm = TRUE)
    y2 = max(fig1_dat$Value, na.rm = TRUE)
    
    # Day Plot
    p1 = ggplot(fig1_dat) +
      # # Points: the observed data 
      # geom_point(data = fig1_dat %>% filter(Type == "Original", Noise_Day),
      #            aes(x = Time, y = Value, color = Type),
      #            size = 1.5, shape = 1) +
      # Crosses: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated", Noise_Day),
                 aes(x = Time, y = Value, color = Type),
                 size = 3, shape = 18) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit", Noise_Day),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = paste0(var_label, ": Daytime"),
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 12))
    
    # Night Plot
    p2 = ggplot(fig1_dat) +
      # Points: the observed data 
      geom_point(data = fig1_dat %>% filter(Type == "Original", !Noise_Day),
                 aes(x = Time, y = Value, color = Type),
                 size = 1.5, shape = 1) +
      # Crosses: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated", !Noise_Day),
                 aes(x = Time, y = Value, color = Type),
                 size = 3, shape = 18) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit", !Noise_Day),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = paste0(var_label, ": Nighttime"),
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "bottom", text = element_text(size = 12))
    
    return(list(p1, p2))
    
  } 
  
  # For temp, plot Avg/Node 2/US Lab separately
  if (var == "Temp"){
    cnames = c("Time_hour", "Location", var, paste0(var, "_CS"))
    var_smoothed = smooth_fun(dat0 = rst.full, var = var, 
                              day_level = FALSE, smooth.dir = smooth.path,
                              return_loess = TRUE)
    
    var_smoothed_dat <- var_smoothed$smooth_dat %>%
      filter(Time_Type == "Flight") %>%
      select(all_of(cnames)) %>%
      `colnames<-`(c("Time", "Location", "Original", "Interpolated"))
    
    # Points: the observed data 
    fig1_obs = var_smoothed_dat %>%
      filter(!is.na(Original)) %>%
      select(Time, Location, Value = Original) %>%
      mutate(Type = "Original")
    
    # Crosses: the interpolated data
    fig1_int =  var_smoothed_dat %>%
      filter(is.na(Original)) %>%
      select(Time, Location, Value = Interpolated) %>%
      mutate(Type = "Interpolated")
    
    # Line: the LOESS fit
    ind_avg = which(var_smoothed_dat$Location == "Avg")
    ind_no2 = which(var_smoothed_dat$Location == "Node 2")
    ind_lab = which(var_smoothed_dat$Location == "US Lab")
    
    plot_dates_avg = sort(c(fig1_obs$Time[ind_avg], fig1_int$Time[ind_avg]))
    plot_dates_no2 = sort(c(fig1_obs$Time[ind_no2], fig1_int$Time[ind_no2]))
    plot_dates_lab = sort(c(fig1_obs$Time[ind_lab], fig1_int$Time[ind_lab]))
    
    fig1_loess_avg = data.frame(Time = plot_dates_avg,
                                Location = "Avg",
                                Value = predict(var_smoothed$smooth_obj$avg, plot_dates_avg),
                                Type = "Smooth Fit")
    
    fig1_loess_no2 = data.frame(Time = plot_dates_no2,
                                Location = "Node 2",
                                Value = predict(var_smoothed$smooth_obj$no2, plot_dates_no2),
                                Type = "Smooth Fit")
    
    fig1_loess_lab = data.frame(Time = plot_dates_lab,
                                Location = "US Lab",
                                Value = predict(var_smoothed$smooth_obj$lab, plot_dates_lab),
                                Type = "Smooth Fit")
    
    fig1_dat <- rbind(fig1_obs, fig1_int, fig1_loess_avg, fig1_loess_no2, fig1_loess_lab)
    
    
    # Plot
    var_label = get_label(var)
    
    # Get limits
    y1 = min(fig1_dat$Value, na.rm = TRUE)
    y2 = max(fig1_dat$Value, na.rm = TRUE)
    
    # Avg Plot
    p1 = ggplot(fig1_dat) +
      # Points: the observed data 
      geom_point(data = fig1_dat %>% filter(Type == "Original", Location == "Avg"),
                 aes(x = Time, y = Value, color = Type),
                 size = 1.5, shape = 1) +
      # Crosses: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated", Location == "Avg"),
                 aes(x = Time, y = Value, color = Type),
                 size =3, shape = 18) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit", Location == "Avg"),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = paste0(var_label, ": Average"),
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 12))
    
    # Node 2 Plot
    p2 = ggplot(fig1_dat) +
      # Points: the observed data 
      geom_point(data = fig1_dat %>% filter(Type == "Original", Location == "Node 2"),
                 aes(x = Time, y = Value, color = Type),
                 size = 1.5, shape = 1) +
      # Crosses: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated", Location == "Node 2"),
                 aes(x = Time, y = Value, color = Type),
                 size = 3, shape = 18) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit", Location == "Node 2"),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = paste0(var_label, ": Node 2"),
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 12))
    
    # US Lab Plot
    p3 = ggplot(fig1_dat) +
      # Points: the observed data 
      geom_point(data = fig1_dat %>% filter(Type == "Original", Location == "US Lab"),
                 aes(x = Time, y = Value, color = Type),
                 size = 1.5, shape = 1) +
      # Crosses: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated", Location == "US Lab"),
                 aes(x = Time, y = Value, color = Type),
                 size = 3, shape = 18) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit", Location == "US Lab"),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = paste0(var_label, ": US Lab"),
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "bottom", text = element_text(size = 12))
    
    return(list(p1, p2, p3))
  }
  
  # Lapses was carried forward
  if (var == "Pred_Lapses"){
    
    
    p1 = ggplot(fig1_dat) +
      # Points: the observed data 
      geom_point(data = fig1_dat %>% filter(Type == "Original"),
                 aes(x = Time, y = Value, color = Type), shape = 1,
                 size = 1.5) +
      # Diamonds: the interpolated data
      geom_point(data = fig1_dat %>% filter(Type == "Interpolated"),
                 aes(x = Time, y = Value, color = Type), shape = 18, 
                 size = 3) +
      # Line: the LOESS fit
      geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit"),
                aes(x = Time, y = Value, linetype = Type),
                size = 0.75, alpha = 0.5) +
      scale_color_manual(breaks = c("Original", "Interpolated"),
                         values = c("#A1C1F9", "#20773C")) +
      labs(title = "", y = var_label,
           x = "Time") +
      ylim(y1, y2) +
      theme_minimal() +
      theme(legend.position = "none", text = element_text(size = 12))
  }
  
  # For Radiation/CO2/O2
  cnames = c("Time_hour", var, paste0(var, "_CS"))
  var_smoothed = smooth_fun(dat0 = rst.full, var = var, 
                            day_level = FALSE, smooth.dir = smooth.path,
                            return_loess = TRUE)
  
  var_smoothed_dat <- var_smoothed$smooth_dat %>%
    filter(Time_Type == "Flight") %>%
    select(all_of(cnames)) %>%
    `colnames<-`(c("Time", "Original", "Interpolated"))
  
  # Points: the observed data 
  fig1_obs = var_smoothed_dat %>%
    filter(!is.na(Original)) %>%
    select(Time, Value = Original) %>%
    mutate(Type = "Original")
  
  # Crosses: the interpolated data
  fig1_int =  var_smoothed_dat %>%
    filter(is.na(Original)) %>%
    select(Time, Value = Interpolated) %>%
    mutate(Type = "Interpolated")
  
  # Line: the LOESS fit
  plot_dates = sort(c(fig1_obs$Time, fig1_int$Time))
  fig1_loess = data.frame(Time = plot_dates,
                          Value = predict(var_smoothed$smooth_obj,plot_dates),
                          Type = "Smooth Fit")
  
  fig1_dat <- rbind(fig1_obs, fig1_int, fig1_loess)
  
  # Plot
  var_label = get_label(var)
  
  # Get limits
  y1 = min(fig1_dat$Value, na.rm = TRUE)
  y2 = max(fig1_dat$Value, na.rm = TRUE)
  
  p1 = ggplot(fig1_dat) +
    # Points: the observed data 
    geom_point(data = fig1_dat %>% filter(Type == "Original"),
               aes(x = Time, y = Value, color = Type), shape = 1,
               size = 1.5) +
    # Diamonds: the interpolated data
    geom_point(data = fig1_dat %>% filter(Type == "Interpolated"),
               aes(x = Time, y = Value, color = Type), shape = 18, 
               size = 3) +
    # Line: the LOESS fit
    geom_line(data = fig1_dat %>% filter(Type == "Smooth Fit"),
              aes(x = Time, y = Value, linetype = Type),
              size = 0.75, alpha = 0.5) +
    scale_color_manual(breaks = c("Original", "Interpolated"),
                       values = c("#A1C1F9", "#20773C")) +
    labs(title = "", y = var_label,
         x = "Time") +
    ylim(y1, y2) +
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(size = 12))
  # For levend
  # theme(legend.position = "bottom", text = element_text(size = 12))
  
  return(p1)
  
}
