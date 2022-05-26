# Prediction of Adverse Cognitive and Behavioral Conditions in Space
# Author: Danni Tu
# server.R: define the things that are listed in ui.R

# Read in Packages --------------------------------------------------------

# Appearance and Plots 
library(shiny)
library(shinyjs)
library(pander)
library(plotly)
library(d3heatmap)
library(stargazer)
library(pls)
library(ggfortify)
library(gridExtra)
library(ggrepel)
# Modelling
library(nlme)
library(refund)
library(fda)
library(fcr)
library(randomForest)
# Data
library(zoo)
library(xts)
library(lubridate)
library(readxl)
library(purrr)
library(tidyverse)

# Directories -------------------------------------------------------------

trish_path <- "/Users/dtu/Library/CloudStorage/Box-Box/TRISH_Analysis/" # Danni Mac
dir.path <- file.path(trish_path, 'Shiny_TRISH/Data/' ) # Data Location
shiny.path <- file.path(trish_path, "Shiny_TRISH")
mod.path <- file.path(dir.path, "Model_Predictions/Hourly_Final") # Fitted Model location
smooth.path <- file.path(dir.path, "Smoothed Objects") # Smoothed environmental data objects

# Helper Functions --------------------------------------------------------

# Variable names and labels
vars_and_labels <- read.csv(file.path(dir.path, "vars_and_labels.csv"))
source(file.path("helpers.R"))

# Data --------------------------------------------------------------------

# Read in data
# Full RST data including pre- and post-flight
rst.full <- readRDS(file.path(dir.path, "rst_full.RDS"))  %>%
  mutate(Time_hour2 = as.numeric(Time_hour),
         Time_day2 = as.numeric(Time_day))
# RST + smoothed environmental data, i
rst.smooth0 <-readRDS(file.path(dir.path, "rst_smooth.rds"))
# Analytic data - only includes inflight rows
rst.smooth <- readRDS(file.path(dir.path, "rst_smooth_hourly_rand.RDS"))
# Randomized IDs
rst.rand <- readRDS(file.path(dir.path, "subj_rand.RDS"))

# SHINY SERVER ------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  # Outcome variable
  outcome_var <- reactive({
    input$outcome
  })
  
  outcome_name <- reactive({
    as.character(vars_and_labels[vars_and_labels$Var == input$outcome, "Label"])
  })
  
  # Distribution plot of outcome variable
  output$plot1 <- renderPlotly({
    p <- ggplot(rst.smooth0) +
      geom_histogram(data = rst.smooth0,
                     aes_string(x = outcome_var()),
                     bins = 30, fill = "#42656e") +
      labs(x = outcome_name(), y = "Count",
           title = paste0("Histogram of ", outcome_name()))
    
    ggplotly(p)
  })
    
  # Outcome vs. Inflight Time plot
  output$plot2 <- renderPlotly({
    rst.smooth0 <- rst.smooth0 %>%
      inner_join(rst.rand, by = "Subject_ID") 
    
    p <- ggplot(rst.smooth0) +
      geom_line(data = rst.smooth0 %>% filter(Time_Type == "Flight") %>%
                  filter((Pre_Flight > -360) | (Post_Flight < 540)),
                aes_string(x = "abs_time_months", y = outcome_var(), 
                    group = "Subject_ID2", color = "Subject_ID2")) +
      geom_line(data = rst.smooth0 %>% filter(Time_Type == "Pre") %>%
                  filter((Pre_Flight > -360) | (Post_Flight < 540)),
                aes_string(x = "abs_time_months", y = outcome_var(),
                    group = "Subject_ID2", color = "Subject_ID2"), linetype = "dashed")  +
      geom_line(data = rst.smooth0 %>% filter(Time_Type == "Post") %>%
                  filter((Pre_Flight > -360) | (Post_Flight < 540)),
                aes_string(x = "abs_time_months", y = outcome_var(),
                    group = "Subject_ID2", color = "Subject_ID2"), linetype = "dotted") +
      labs(x = "Inflight Months", y = outcome_name(),
           color = "Subject", title = paste0(outcome_name(), " vs. Inflight Months")) 
    
    ggplotly(p)
  })
  
  # Environmental Plots
  env_var <- reactive({
    input$env_var
  })
  
  env_var_smoo <- reactive({
    paste0(input$env_var, "_CS")
  })
  
  # Smoothed environmental plots
  output$plot_smoothed <- renderPlot({
    
    if (env_var() == "Noise_dBA"){
      p <- smooth_env_plot(rst.full, var = "Noise_dBA", smooth.dir = smooth.path)
      grid.arrange(p[[1]], p[[2]])
      
    } else if (env_var() == "Temp") {
      
      p <- smooth_env_plot(rst.full, var = env_var(), smooth.dir = smooth.path)
      grid.arrange(p[[1]], p[[2]], p[[3]])
    } else {
      p <- smooth_env_plot(rst.full, var = env_var(), smooth.dir = smooth.path)
      p
    }
    
   
  })
  

# Modeling and Prediction -------------------------------------------------
  
  # Read in fitted model object based on the full data
  model_obj <- reactive({
    outcome_snippet <- switch(input$mod_outcome,
                              "Overall_Performance" = "o",
                              "LRM_50" = "l",
                              "PVT_Z" = "p",
                              "LRM_Z" = "lz")
    var_snippet <- input$mod_preds
    fname <- paste0("ensemble_", outcome_snippet, "_", var_snippet, ".RDS")
    readRDS(file.path(mod.path, fname))
  })
  
  # Dynamically change the subject
  mod_subj <- reactive({
    input$mod_subj
  })
  
  # Nice version of model outcomes
  model_outcome_name <- reactive({
      get_label(input$mod_outcome)
  })
  
  # Model Details - Contour Plot/Heatmap
  # Read in fcr_grid object
  fcr_grid_obj <- reactive({
    preds_snippet = input$mod_preds
    outcome_snippet <- switch(input$mod_outcome,
                              "Overall_Performance" = "o",
                              "LRM_50" = "l",
                              "PVT_Z" = "p",
                              "LRM_Z" = "lz")
    
    fname = paste0("fcr_grid_", preds_snippet, "_", outcome_snippet, ".RDS")
    readRDS(file.path(mod.path, fname))
  })
  
  # Update the choices for fcr_contour_var1 depending on the inputs:
  # input$mod_outcome and input$mod_preds
  observe({
    x_out <- input$mod_outcome
    x_pred <- input$mod_preds
    
    # Can use character(0) to remove all choices
    if (is.null(x_out)|is.null(x_pred)){
      var_choices <- character(0)
    } else {
      # LRM-50 Outcome
      if (x_out == "LRM_50"){
        # Full set of predictors
        if (x_pred == "full"){
          var_choices = intersect(covar_mod, covar_mod_num)
        } else {
          # Top 10 predictors
          var_choices = intersect(covar_t10_l, covar_mod_num)
        }
        # var_choices = c(var_choices, "LRM_50_L1")
      }

      # LRM_Z Outcome
      if (x_out == "LRM_Z"){
        # Full set of predictors
        if (x_pred == "full"){
          var_choices = intersect(covar_mod, covar_mod_num)
        } else {
          # Top 10 predictors
          var_choices = intersect(covar_t10_lz, covar_mod_num)
        }
        # var_choices = c(var_choices, "LRM_Z_L1")
      }
    }
    
    # Get labels
    names(var_choices) <- get_label(var_choices)
    
    # Update the fcr_contour_var1 input 
    updateSelectInput(session = session, inputId = "fcr_contour_var1",
                      label = "Choose Variable:",
                      choices = var_choices)
    
    # Update the fcr_contour_var1 input 
    updateSelectInput(session = session, inputId = "fcr_contour2_var1",
                      label = "Choose x-axis Variable:",
                      choices = var_choices, selected = var_choices[1])
    
    # Update the fcr_contour_var1 input 
    updateSelectInput(session = session, inputId = "fcr_contour2_var2",
                      label = "Choose y-axis Variable:",
                      choices = var_choices, selected = var_choices[2])
  })

  output$fcr_contour <- renderPlot({
    
    # Covariate set
    the_lrm_lims = NULL
    
    # LRM-50 Outcome
    if (input$mod_outcome == "LRM_50"){
      # Full set of predictors
      if (input$mod_preds == "full"){
        covars = covar_mod
      } else {
        # Top 10 predictors
        covars = covar_t10_l
      }
      covars = c(covars, "LRM_50_L1")
      the_lrm_lims = c(-35.9, -29)
    }

    # LRM_Z Outcome
    if (input$mod_outcome == "LRM_Z"){
      # Full set of predictors
      if (input$mod_preds == "full"){
        covars = covar_mod
      } else {
        # Top 10 predictors
        covars = covar_t10_lz
      }
      covars = c(covars, "LRM_Z_L1")
      the_lrm_lims = c(-0.16, 0.39)
    }

    # Input variable
    var1 = input$fcr_contour_var1
    var1_label = get_label(var1)
    
    fcr_heatmap_1var(dat0 = rst.smooth,
                     fcr_obj = fcr_grid_obj(),
                     outcome = input$mod_outcome,
                     covars = covars,
                     var1 = var1,
                     lrm_lims = the_lrm_lims, 
                     leg_pos = "bottom", txt_size = 14)
  })
  
  output$fcr_contour_2var <- renderPlot({
    req(input$fcr_contour2_var1)
    req(input$fcr_contour2_var2)
    validate(need(input$fcr_contour2_var1 != input$fcr_contour2_var2,
             'Variables 1 and 2 must be different.'))
    
    # Covariate set
    # LRM-50 Outcome
    if (input$mod_outcome == "LRM_50"){
      # Full set of predictors
      if (input$mod_preds == "full"){
        covars = covar_mod
      } else {
        # Top 10 predictors
        covars = covar_t10_l
      }
      covars = c(covars, "LRM_50_L1")
    }

    # LRM_Z Outcome
    if (input$mod_outcome == "LRM_Z"){
      # Full set of predictors
      if (input$mod_preds == "full"){
        covars = covar_mod
      } else {
        # Top 10 predictors
        covars = covar_t10_lz
      }
      covars = c(covars, "LRM_Z_L1")
    }
    
    # Two-variable heat map
    fcr_heatmap_2var(dat0 = rst.smooth,
                     fcr_obj = fcr_grid_obj(),
                     outcome = input$mod_outcome,
                     covars = covars,
                     var1 = input$fcr_contour2_var1, 
                     var2 = input$fcr_contour2_var2, 
                     time_index = 1,
                     leg_pos = "bottom", txt_size = 14)
  })
  
  # Model Details - LME Coefficients
  lme_obj <- reactive({
    preds_snippet = input$mod_preds
    outcome_snippet <- switch(input$mod_outcome,
                              "Overall_Performance" = "o",
                              "LRM_50" = "l",
                              "PVT_Z" = "p",
                              "LRM_Z" = "lz")
    
    fname = paste0("lme_", outcome_snippet, "_", preds_snippet, ".RDS")
    readRDS(file.path(mod.path, fname))
  })
  
  output$mod_stargazer <- renderUI({
    
    # Get the nice labels for covariates' names
    covars <- names(lme_obj()$coefficients$fixed)[-1]
    covar_labels <- get_label(covars)
    
    x <- HTML(stargazer(lme_obj(), type="html", report=('vc*p'),
                        dep.var.labels = model_outcome_name(),
                        covariate.labels = c(covar_labels, "Intercept")))
    return(x)
  })
  

# Prediction Plots --------------------------------------------------------

  
  # Load in different predictions
  ensemble_pred <- reactive({
    outcome_snippet <- switch(input$pred_outcome,
                              "Overall_Performance" = "o",
                              "LRM_50" = "l",
                              "PVT_Z" = "p",
                              "LRM_Z" = "lz")
    n_test_snip <- input$n_test_obs
    n_train_snip <- if (input$n_train_obs == "All") 200 else input$n_train_obs
    var_snippet <- input$mod_preds
    
    fname <- paste0("ens_", outcome_snippet, "_", var_snippet,
                    "_tr", n_train_snip, "_te", n_test_snip, ".RDS")
    readRDS(file.path(mod.path, fname))
  })
  
  
  output$mse_table_pred <- renderTable({
    ensemble_pred()$mse_table %>%
      mutate(Data = ifelse(grepl(pattern = "test", x = Variable), "Testing Set", "Training Set"),
             Variable = ifelse(grepl(pattern = "rmse", x = Variable), "RMSE/SD", "MSE")) %>%
      dplyr::select(Data, Variable,
             `Ensemble Model` = ensemble,
             `Linear Mixed Effects` = lme,
             `Functional Concurrent Regression` = fcr,
             `Random Forest` = forest) %>%
      arrange(desc(Data))
  }, width = "80%")
  
  # Subject-level prediction plot
  output$mod_predplot <- renderPlotly({
    
    pred_outcome_name <- get_label(input$pred_outcome)
    
    plot_subj <- rst.smooth %>%
      filter(Subject_ID == mod_subj())
    
    plot_train <- ensemble_pred()$rst_preds_train %>%
      mutate(Pred = ensemble) %>%
      inner_join(rst.rand, by = "Subject_ID") %>%
      filter(Subject_ID2 == mod_subj())
    
    split_line = max(plot_train$abs_inflight_hr)
    
    plot_test <- ensemble_pred()$rst_preds_test %>%
      mutate(Pred = ensemble) %>%
      inner_join(rst.rand, by = "Subject_ID") %>%
      filter(Subject_ID2 == mod_subj()) 
    
    plot_title = paste0("Prediction for Subject ", mod_subj())
    
    # Plot parameters
    max_x = max(plot_test$abs_inflight_hr)
    
    (ggplot(plot_train) +
        # Shaded regions representing training/test set
        geom_rect(xmin = 0, xmax = split_line, ymin = -100, ymax = 3, fill = "#9376a3",
                  alpha = 0.2) +
        geom_rect(xmin = split_line, xmax = max_x, ymin = -100, ymax = 3, fill = "#e3aa54",
                  alpha = 0.2) +
        
      # Observed outcome for training data
      geom_point(data = plot_subj,
                  aes_string(x = "abs_inflight_hr", y = input$pred_outcome, color = shQuote("Observed"))) +
      # Vertical line
      geom_vline(xintercept = split_line, linetype="dotted", color = "orange", size=1) +
      # Plot the predicted PVT and 95% CI - TEST
      geom_line(data= plot_test,
                aes_string(x = "abs_inflight_hr", y = "Pred", color = shQuote("Predicted")), size = 1) +
      geom_line(data= plot_test,
                aes_string(x = "abs_inflight_hr", y = "Pred", color = shQuote("Predicted")), size = 1) +
      geom_line(data= plot_train,
                aes_string(x = "abs_inflight_hr", y = "Pred", color = shQuote("Predicted")), size = 1) +
      theme_minimal() +
      scale_color_manual(breaks = c("Observed", "Predicted"), values = c("tomato3", "steelblue3")) +
      labs(title = plot_title,
           x = "Inflight Hours", y = pred_outcome_name,
           color = "",
           linetype = "")) %>%
      ggplotly
      
  })
  

# Turning the dials -------------------------------------------------------
  
  # Refresh the value in the dials
  observeEvent(input$reset_dials, {
    shinyjs::reset(id = "dials")
    shinyjs::reset(id = "dials")
  })
  
  # Subject for the "Turning Dials" plot
  dial_subj <- reactive({
    input$dial_subj
  })
  
  # Fitted model object for the "Turning Dials" plot
  dial_model_obj <- reactive({
    outcome_snippet <- switch(input$dial_outcome,
                              "Overall_Performance" = "o",
                              "LRM_50" = "l",
                              "PVT_Z" = "p",
                              "LRM_Z" = "lz")
    fname <- paste0("ensemble_", outcome_snippet, "_full.RDS")
    readRDS(file.path(mod.path, fname))
  })

  # Dynamic Top 10 Variable Sliders
  output$dials <- renderUI({

    
    # Make the default value the average of all values
    dat_subj <- ensemble_pred()$rst_preds_train %>%
      inner_join(rst.rand, by = "Subject_ID") %>%
      filter(Subject_ID2 == dial_subj())
    
    # This function will make it easier to create
    # sliderInput objects for any variable
    dial_input_maker <- function(var_inputid, var_name, var_label, units = "mGy"){
      # Value defaults to the mean value in the training
      var_value = mean(dat_subj[,var_name], na.rm = TRUE) %>% round(2)
      # Maximum observed value in all data
      var_max = max(rst.smooth[,var_name], na.rm = TRUE) %>% round(2)
      # Mininum observed value in all data
      var_min = min(rst.smooth[,var_name], na.rm = TRUE) %>% round(2)
      # Scale the step
      var_step = round((var_max-var_min)*0.05, 2)
      
      # Special steps
      if (var_name == "Pre_PVT"){var_step = round((var_max-var_min)*0.05, 3)}
      if (var_name == "Very_stressed"){var_step = 1}
      
      # Return sliderInput object
      the_post <- if (units == "") NULL else paste0(" ", units)
      return(sliderInput(inputId = var_inputid,
                         label = var_label, value = var_value,
                         min = var_min, max = var_max, step = var_step,
                         post = the_post))
    }
    
    # Output boxes dynamically filled with last value
    
    out = list(
      dial_input_maker(var_inputid = "dials_rad", var_name = "Radiation_Dose_CS",
                       var_label = "Radiation (mGy)", units = "mGy"),
      dial_input_maker(var_inputid = "dials_temp", var_name = "Temp_CS",
                       var_label = "Temperature (Celsius)", units = "Celsius"),
      dial_input_maker(var_inputid = "dials_noise", var_name = "Noise_dBA_CS",
                       var_label = "Noise (dBA)", units = "dBA"),
      dial_input_maker(var_inputid = "dials_co2", var_name = "CO2_CS",
                       var_label = "CO2 (mmHg)", units = "mmHg"),
      dial_input_maker(var_inputid = "dials_ppo2", var_name = "ppO2_CS",
                       var_label = "O2 (mmHg)", units = "mmHg"),
      dial_input_maker(var_inputid = "dials_prepvt", var_name = "Pre_PVT",
                       var_label = "Mean Pre-Flight PVT", units = ""),
      dial_input_maker(var_inputid = "dials_lapses", var_name = "Pred_Lapses_CS",
                       var_label = "Predicted Lapses", units = "Lapses")
    )
    
    if (input$dial_outcome == "LRM_50"){
      # For LRM-50 Outcome, add Very_stressed and LRM-50
      out <- list(out, dial_input_maker(var_inputid = "dials_stress", var_name = "Very_stressed",
                                     var_label = "Stress (Higher = More Stressed)", units = ""),
               dial_input_maker(var_inputid = "dials_lagl", var_name = "LRM_50_L1",
                                var_label = "Lagged LRM-50", units = ""))
    } else {
      # For LRM_Z Outcome, add Total_sleep_hours and LRMZ
      out <- list(out, dial_input_maker(var_inputid = "dials_sleep", var_name = "Total_sleep_hrs",
                                     var_label = "Total Sleep", units = "Hours"),
               dial_input_maker(var_inputid = "dials_laglz", var_name = "LRM_Z_L1",
                                var_label = "Lagged Standardized LRM-50", units = ""))
    }
  })
  
  # Update values depending on what's entered
  dials_rad <- reactive({
    input$dials_rad
  })
  
  dials_temp <- reactive({
    input$dials_temp
  })
  
  dials_noise <- reactive({
    input$dials_noise
  })
  
  dials_co2 <- reactive({
    input$dials_co2
  })
  
  dials_ppo2 <- reactive({
    input$dials_ppo2
  })
  
  dials_prepvt <- reactive({
    input$dials_prepvt
  })
  
  dials_lapses <- reactive({
    input$dials_lapses
  })
  
  dials_stress <- reactive({
    input$dials_stress
  })
  
  dials_lagl <- reactive({
    input$dials_lagl
  })
  
  dials_sleep <- reactive({
    input$dials_sleep
  })
  
  dials_laglz <- reactive({
    input$dials_laglz
  })
  
  
  output$dial_predplot <- renderPlotly({
    wgt_lrf = c(0.333,0.333,0.333)
    
    # Don't load the plot until the dials render
    req(input$dials_rad)

    # Plot the distribution of observed values
    plot_train <- rbind(ensemble_pred()$rst_preds_train,
                        ensemble_pred()$rst_preds_test) %>%
      inner_join(rst.rand, by = "Subject_ID") %>%
      filter(Subject_ID2 == dial_subj())
    
    # Get top 15% and worst 15% performance
    best_15 = quantile(plot_train[,input$dial_outcome], p = 0.15)
    worst_15 = quantile(plot_train[,input$dial_outcome], p = 0.85)
    
    # Data for prediction
    # First observation from testing data
    dat_new <- ensemble_pred()$rst_preds_test %>%
      inner_join(rst.rand, by = "Subject_ID") %>%
      filter(Subject_ID2 == dial_subj()) %>%
      arrange(inflight_time) %>%
      slice(1) %>%
      # Replace with values from the dials
      mutate(Radiation_Dose_CS = dials_rad(),
             Temp_CS = dials_temp(),
             Noise_dBA_CS = dials_noise(),
             CO2_CS = dials_co2(),
             ppO2_CS = dials_ppo2(),
             Pre_PVT = dials_prepvt(),
             Pred_Lapses_CS = dials_lapses())
    
    if (input$dial_outcome == "LRM_50"){
      dat_new <- dat_new %>%
        mutate(Very_stressed = dials_stress(),
               LRM_50_L1 = dials_lagl())
    }
    
    if (input$dial_outcome == "LRM_Z"){
      dat_new <- dat_new %>%
        mutate(Total_sleep_hrs = dials_sleep(),
               LRM_Z_L1 = dials_laglz())
    }
    
    # Prediction using model objects
    pred_lme = predict(object = dial_model_obj()$obj_lme, newdata = dat_new, se.fit = TRUE)
    pred_for = predict(object = dial_model_obj()$obj_forest, newdata = dat_new, se.fit = TRUE)
    pred_fcr = predict(object = dial_model_obj()$obj_fcr, newdata = dat_new, se.fit = TRUE)$insample_predictions$fit
    pred_ens = data.frame(pred_lme = pred_lme,
                          pred_for = pred_for,
                          pred_fcr = pred_fcr) %>%
      as.matrix %>%
      matrixStats::rowWeightedMeans(., w = wgt_lrf)
    
    # Plot new test data
    plot_test <- dat_new %>%
      mutate(Pred = pred_ens)
    
    # Dynamically update plot title
    plot_title = paste0("Subject ", dial_subj(),
                        "\nNext Predicted Value of ", model_outcome_name(), ": ",
                        round(plot_test$Pred,2))
      
    p <- ggplot(plot_train) +
      # Background colors showing best/worst 15%
      geom_rect(xmin = -60, xmax = best_15, ymin = 0, ymax = 80, fill = "#228833",
                alpha = 0.2) +
      geom_rect(xmin = worst_15, xmax = 30, ymin = 0, ymax = 80, fill = "#EE6677",
                alpha = 0.2) +
      # Observed Distribution of Outcome in all Data
      geom_histogram(data = plot_train, bins = 25,
                     aes_string(x = input$dial_outcome, y = "(..count..)/sum(..count..)",
                                fill = shQuote("Observed\nDistribution"),
                                color = shQuote("Observed\nDistribution"))) +
        scale_y_continuous(labels=scales::percent) +
      # Predicted Value
      geom_point(data = plot_test,
                 aes_string(x = "Pred", y = 0.005, fill = shQuote("Predicted\nValue"),
                            color = shQuote("Predicted\nValue")),
                 shape = 8, size = 3) +
      # Format legend/axes
      scale_fill_manual(breaks = c("Observed\nDistribution", "Predicted\nValue"), values = c("#4477AA", "#66CCEE")) +
      scale_color_manual(breaks = c("Observed\nDistribution", "Predicted\nValue"), values = c("#4477AA", "#66CCEE")) +
      theme_minimal() +
      labs(title = plot_title,
           x = get_label(input$dial_outcome), y = "Frequency",
           color = "", fill = "",
           linetype = "")
    
    ggplotly(p) %>%
      style(textposition = "right")
      
    
  })
  
})
