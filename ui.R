# Prediction of Adverse Cognitive and Behavioral Conditions in Space
# Danni Tu
# ui.R: make space for items defined in server.R

library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(tidyverse)

###########
# LOAD UI #
###########

trish_path <- "/Users/dtu/Library/CloudStorage/Box-Box/TRISH_Analysis/" # Danni Mac
shiny.path <- file.path(trish_path, "Shiny_TRISH")
dir.path<- file.path(trish_path, 'Shiny_TRISH/Data/' ) # Data Location

# Helper Functions --------------------------------------------------------

# Variable names and labels
vars_and_labels <- read.csv(file.path(dir.path, "vars_and_labels.csv"))
source(file.path("helpers.R"))

# Data --------------------------------------------------------------------

names(covar_mod) <- get_label(covar_mod)
# Smoothed RST data with randomized subject IDs
rst.smooth <- readRDS(paste0(dir.path, "rst_smooth_hourly_rand.rds"))
subject_ids <- sort(unique(rst.smooth$Subject_ID))


shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # Use javascript to refresh page
  shinyjs::useShinyjs(),
  
  # remove shiny "red" warning messages on GUI
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  # load page layout
  dashboardPage(
    
    # Title bar
    dashboardHeader(title="Prediction of Adverse Cognitive and Behavioral Conditions in Space", titleWidth = 800),
    
    # SIDEBAR
    # TRISH and Penn Medicine logos
    # Menu items
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://www.med.upenn.edu/' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='penn-medicine-logo.png' width = '186'></a>",
                         "<br>",
                         "<a href='https://www.bcm.edu/academic-centers/space-medicine/translational-research-institute' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='trish_logo.png' width = '186'></a>",
                         "<br>"
                       )),
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       menuItem("Individualized Predictions", tabName = "dial", icon = icon("tachometer-alt")),
                       menuItem("Ensemble Model", tabName = "model", icon = icon("chart-bar")),
                       menuItem("Exploratory Plots", tabName = "exp", icon = icon("chart-pie")),
                       menuItem("People", tabName = "people", icon = icon("user-circle")),
                       HTML(paste0(
                         "<script>",
                         "var today = new Date();",
                         "var yyyy = today.getFullYear();",
                         "</script>",
                         "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.med.upenn.edu/' target='_blank'>med.upenn.edu</a> - <script>document.write(yyyy);</script></small></p>")
                       )
                     )
                     
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        # Home tab
        tabItem(tabName = "home",
                fluidRow(
                  column(width = 8,
                         h3("Welcome to the Prediction Dashboard for Adverse Cognitive and 
                   Behavioral Conditions in Space!"),
                         h5("In this project, we utilized an ensemble model to predict cognitive performance
                   in astronauts aboard the ISS. We display our results and model predictions
                   in a dynamic Shiny application format to better address questions
                   that investigators might have.")
                         ) # end column
                )
        ), # end Home tab
        
        # Modelling tab
        tabItem(tabName = "model", 
                fluidRow(
                  column(width = 6,
                         box(title = "Ensemble Model", width = NULL,
                             h5("The 3-model ensemble prediction consists of an equally weighted
                         average of predictions based on the following models:"),
                             h5(tags$ul(
                               tags$li("Linear mixed effects model with AR1 correlation"),
                               tags$li("Random Forest"),
                               tags$li("Functional Concurrent Regression")
                             )),
                             
                             h4("Outcome"),
                             h5("The main outcome of interest is the LRM-50, a likelihood-based metric
                      based on the psychomotor vigilance test (PVT). Lower values correspond
                         to better performance, while higher values correspond to worse performance.",
                                "We also considered the standardized LRM-50 outcome, which is formed by
                         linearly scaling (i.e., z-scoring) the LRM-50 to have mean 0 and 
                         standard deviation 1."),
                             selectInput(inputId = "mod_outcome", label = "Outcome Variable",
                                         choices = c("LRM-50" = "LRM_50",
                                                     "Standardized LRM-50" = "LRM_Z"
                                         )),
                             h4("Predictors"),
                             # Choose predictor set: all predictors, or only Top 10.
                             h5("As predictors, we considered the following variables:"),
                             h5(tags$ul(
                               tags$li("Environmental factors: Radiation, Noise, Temperature CO2, O2, ISS Occupancy"),
                               tags$li("Demographic factors: Sex, Age at Dock"),
                               tags$li("Medication and Caffeine Use: Sleep Aid, Antihistamines, Caffeinated Drinks"),
                               tags$li("RST variables: Self-Report Principal Component 1, Low Workload,
                                Poor Sleep Quality, Very Stressed, Total Sleep Hours, Total Sleep Missed,
                                       Morning/Evening Test"),
                               tags$li("PVT variables: Pre-Flight Average PVT, Predicted Lapses, Lagged LRM-50
                                       (if LRM-50 outcome)/Lagged Standardized LRM-50 (if standardized LRM-50 outcome)")
                             )),
                             h5("We also considered models trained on the Top 10 most important variables,
                         as determined by a random forest. These variables are the following:"),
                             h5(tags$ul(
                               tags$li("Environmental factors: Radiation, Temperature, CO2, O2"),
                               tags$li("Demographic factors: Age at Dock, Pre-Flight Average PVT"),
                               tags$li("RST variables: Self-Report Principal Component 1, Predicted Lapses"),
                               tags$li("PVT variables: Pre-Flight Average PVT, Predicted Lapses"),
                               tags$li("Very Stressed and lagged LRM-50 (if LRM-50 outcome)"),
                               tags$li("Total Sleep Hours and lagged standardized LRM-50 (if standardized LRM-50 outcome)")
                             )),
                             h5(em("Note:"), "The Self-Report Principal Component 1 (Self-Report PC1) is a composite variable consisting of
                                the first principal component of 6 RST Self-Report variables. A higher value of the
                                Self-Report PC1 variable is associated with increased workload, worse sleep quality/feelings of
                                sleepiness, increased physical exhaustion, increased mental fatigue,
                                feeling less fresh and ready to go, and increased stress."),
                             h5(em("Note:"), "The number of total sleep hours is based on the reported value in the RST sleep diary. The
                                number of total hours of sleep missed is calculated as the total hours spent
                                lying in bed subtracted by the total sleep hours."),
    
                             selectInput(inputId = "mod_preds", label = "Predictors",
                                         choices = c("All Predictors" = "full")), 
                                   
                             # References
                             h4("References"),
                             h5("Basner, M., Mcguire, S., Goel, N., Rao, H., & Dinges, D. F. (2015). 
                         A new likelihood ratio metric for the psychomotor vigilance test and its sensitivity to sleep loss. 
                         Journal of Sleep Research, 24(6), 702–713.")),
                         box(title = "Linear Mixed Effects Model Coefficients", width = NULL,
                             h5("The linear mixed effects model estimates constant (with respect to time) effects
                         for each predictor. They are listed below:"),
                             uiOutput(outputId = "mod_stargazer"))
                         ), # end left column
                  column(width = 6,
                         box(title = "Functional Regression Contour Plots", width = NULL,
                             # Contour plot for FCR
                             h5("How do the effects of environmental conditions and other predictors of cognitive outcomes change
                      over time, or in relation to each other? These are estimated by the functional
                      concurrent regression (FCR) component of the ensemble model. Using the estimated time-varying
                      coefficients from the FCR, we generate the following single- and two-variable contour
                      plots. ", span("Darker", style = "color:#365c8d"),  "regions correspond to worse predicted outcomes,
                         while ", span("lighter", style = "color:#9bb013"), "regions correspond to better predicted outcomes."),
                             #uiOutput(outputId = "fcr_contour_var1"), # Select variable
                             # Create selectInput object based on var_choices
                             h3("Single-Variable Contour Plot"),
                             selectInput(inputId = "fcr_contour_var1",
                                         label = "Choose Variable:",
                                         choices = covar_mod,
                                         selected = covar_mod[1]),
                             plotOutput(outputId = "fcr_contour", height = "500px"), # Produce plot
                             h3("Two Variable Contour Plot"),
                             selectInput(inputId = "fcr_contour2_var1",
                                         label = "Choose x-axis Variable:",
                                         choices = covar_mod,
                                         selected = covar_mod[1]),
                             selectInput(inputId = "fcr_contour2_var2",
                                         label = "Choose y-axis Variable:",
                                         choices = covar_mod,
                                         selected = covar_mod[2]),
                             plotOutput(outputId = "fcr_contour_2var", height = "500px") # Produce plot
                             
                             )))
                
        ),
        
        # Turning the Dial Tab
        tabItem(tabName = "dial",
                fluidRow(
                  # Turning the dials
                  box(title = "Turning the Dials", width = 12,
                      h5("How does performance change as a function of relevant envionmental, operational,
                      and psychological predictors?
                      Here, we use the ensemble model (fitted on the entire data set) to make predictions
                      about the hypothetical outcome for a particular astronaut 
                      after 'turning the dials' on important predictors of cognitive performance."),
                      h5("More details on the ensemble model can be found in the 'Ensemble Model' tab!"),
                      column(width = 4,
                             selectInput(inputId = "dial_outcome", label = "Outcome Variable",
                                         choices = c("LRM-50" = "LRM_50",
                                                     "Standardized LRM-50" = "LRM_Z"
                                         )),
                             actionButton("reset_dials", "Reset Values"),
                             uiOutput(outputId = "dials"),
                             h5("Note: default values for each variable are the average for the chosen astronaut. 
                         Possible values range over all observed values in the entire data.")
                      ),
                      column(width = 8,
                             h3("Individualized Prediction"),
                             selectInput(inputId = "dial_subj", label = "Participant ID",
                                         choices = subject_ids),
                             plotlyOutput(outputId = "dial_predplot"),
                             h5("In the above graph, the ", span("red", style = "color:#EE6677"), 
                                " region indicates this individual's worst (highest) 15% performance; 
                         the ", span("green", style = "color:#228833"), 
                                " region indicates the best (lowest) 15% performance, and the ",
                                span("dark blue", style = "color:#4477AA"), 
                                " histogram displays the observed distribution of the cognitive
                                outcome for that individual."),
                             h5(em("Note:"), "the prediction is based on a statistical model
                                and should be taken with a degree of uncertainty. The goal of this
                                interactive figure is to illustrate the joint and potentially non-linear effects 
                                of these predictors on the estimated outcome for a particular person."))
                  ), # end box
                  # Prediction plots
                  box(title = "Prediction Performance", width = 12,
                      h5("How well does our model perform on unobserved data? To assess predictive
                         accuracy, we split the data into", span("Training", style = "color:#9376a3"), " and ",
                         span("Testing", style = "color:#e3aa54"), " sets. The model parameters are fit on the Training
                         set, and performance is assessed on the Testing set."),
                      column(width = 6,
                             selectInput(inputId = "pred_outcome", label = "Outcome Variable",
                                         choices = c("LRM-50" = "LRM_50",
                                                     "Standardized LRM-50" = "LRM_Z" #,
                                                     #"PVT Score" = "Overall_Performance",
                                                     #"Standardized PVT" = "PVT_Z"
                                         )),
                             # Choose prediction window from 1 to 25 days
                             h5("By default, the Testing set comprises the last 5 observations for all participants,
                                and the Training set contains all preceding observations. The number of
                                observations in the Training or Test sets can be adjusted below:"),
                             sliderTextInput(inputId = "n_test_obs", 
                                             label = "Testing Observations", 
                                             choices = c(1,5,10,15,20), selected = 5),
                             sliderTextInput(inputId = "n_train_obs",
                                             label = "Training Observations",
                                             choices = c(10, 15, 20, 25, 30, 35, "All"),
                                             selected = "All"),
                             # Train/Test Set Prediction Accuracy
                             h3("Model Performance (All Participants)"),
                             h5(tableOutput(outputId = "mse_table_pred")),
                             h5(em("Note:"), "we assessed performance accuracy using the mean squared error (MSE),
                             which is defined as the squared difference
                                in predicted and observed values, averaged over all observations. The root
                                mean squared error (RMSE) is the square root of the MSE, and the RMSE/SD
                                ratio is the ratio of the RMSE to the observed standard deviation of the 
                                observed values. In other words, the RMSE/SD measures the model error as a
                                proportion of the overall variation observed in the data.")
                             ),
                      column(width = 6,
                             # Choose subject for subject-specific prediction plots
                             h3("Individualized Predicted Trajectory"),
                             selectInput(inputId = "mod_subj", label = "Participant ID",
                                         choices = subject_ids),
                             h5("The model was fit on the Training Data", span("(purple region)", style = "color:#9376a3"), "and 
                                prediction was performed on the Testing Data", span("(orange region).", style = "color:#e3aa54")),
                             plotlyOutput(outputId = "mod_predplot"))
                  )
                ) # end fluidRow
        ), # end tabItem
        
        # Exploratory Plots tab
        tabItem(tabName = "exp",
                fluidRow(
                  # Cognitive Outcome Plots
                  box(title = "Cognitive Outcome Plots", width = 8,
                      collapsible = TRUE,
                      selectInput(inputId = "outcome", label = "Outcome Variable",
                                  choices = c("LRM-50" = "LRM_50",
                                              "Standardized LRM-50" = "LRM_Z",
                                              "PVT Score" = "Overall_Performance",
                                              "Standardized PVT" = "PVT_Z")),
                      h5("These plots include data from all participants in our data and over the entire study period, including pre-flight,
                         inflight, and post-flight time periods."),
                      plotlyOutput(outputId = "plot1"),
                      plotlyOutput(outputId = "plot2"),
                      h5("Note: dashed lines indicate pre-flight measurements, 
                         solid lines indicate in-flight measurements, 
                         and dotted lines indicate post-flight measurements.")
                      ), # end box
                  # Environmental Plots
                  box(title = "Smoothed Environmental Data", width = 8,
                      collapsible = TRUE,
                      selectInput(inputId = "env_var", label = "Environmental Variable",
                                  choices = c("Radiation" = "Radiation_Dose",
                                              "Temperature" = "Temp",
                                              "Noise" = "Noise_dBA",
                                              "CO2" = "CO2",
                                              "O2" = "ppO2")),
                      plotOutput(outputId = "plot_smoothed",
                                 height = "600px")
                  )
                )
                
        ), # end Exploratory Plots tab
        
        # Notes tab
        tabItem(tabName = "people", 
                fluidRow(
                  column(width = 6,
                         h3("Project Team"),
                         tags$ul(
                           tags$li("Danni Tu, MS"),
                           tags$li("Mathias Basner, PhD"), 
                           tags$li("Michael G. Smith, PhD"),
                           tags$li("E. Spencer Williams, PhD"),
                           tags$li("Valerie E. Ryder, PhD"),
                           tags$li("Amelia A. Romoser, PhD"),
                           tags$li("Adrian Ecker, PhD"),
                           tags$li("Daniel Aeschbach, PhD"),
                           tags$li("Alexander Stahn, PhD"),
                           tags$li("Christopher Jones, PhD"), 
                           tags$li("Kia Howard"),
                           tags$li("Marc Kaizi-Lutu"),
                           tags$li("David Dinges, PhD"), 
                           tags$li("Haochang Shou, PhD")),
                         br(),
                         h5("This R Shiny application was developed by Danni Tu. For questions and 
                            feedback, please send an email to danni.tu at pennmedicine.upenn.edu."))
                )
                
        ) # end Notes tab
        
      ) # end tabItems
      
    ) # end dashboardBody
    
  )# end dashboardPage
  
) # end fluidPage
) # end ShinyUI
