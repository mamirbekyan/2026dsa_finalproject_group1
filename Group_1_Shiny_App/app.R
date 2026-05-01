#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------
# DATA PREP
# ------------------------------------------------
merged_submission_boxplot_main <- merged_submission_boxplot %>%
  mutate(
    yield_mg_ha = if_else(is.na(yield_mg_ha), pred_yield, yield_mg_ha)
  )

vars_to_plot <- c(
  "n_days",
  "sum_gdd",
  "sum_precip",
  "mean_dayl_s",
  "mean_srad_w_m_2",
  "mean_tmax_deg_c",
  "mean_tmin_deg_c",
  "mean_vp_pa"
)

# ------------------------------------------------
# UI
# ------------------------------------------------
ui <- navbarPage(
  "Crop Modeling App",
  
  # ---------------- EDA ----------------
  tabPanel(
    "EDA",
    fluidPage(
      titlePanel("Exploratory Data Analysis"),
      
      fluidRow(
        column(6, plotOutput("yield_hist")),
        column(6, plotOutput("yield_site"))
      )
    )
  ),
  
  # ---------------- PREDICTOR ----------------
  tabPanel(
    "Predictor vs Yield",
    fluidPage(
      titlePanel("Weather & Soil Effects"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("var", "Select predictor:", choices = vars_to_plot)
        ),
        
        mainPanel(
          plotOutput("predictor_plot")
        )
      )
    )
  ),
  
  # ---------------- INTERACTIVE ----------------
  tabPanel(
    "Interactive Exploration",
    fluidPage(
      titlePanel("Custom Variable Exploration"),
      
      sidebarLayout(
        sidebarPanel(
          selectInput("xvar", "X-axis:", choices = vars_to_plot),
          selectInput("yvar", "Y-axis:", choices = c("yield_mg_ha", vars_to_plot))
        ),
        
        mainPanel(
          plotOutput("xy_plot")
        )
      )
    )
  ),
  
  # ---------------- MODEL PERFORMANCE ----------------
  tabPanel(
    "Model Performance",
    fluidPage(
      titlePanel("Observed vs Predicted"),
      
      fluidRow(
        column(6, plotOutput("obs_pred")),
        column(6, textOutput("metrics"))
      )
    )
  ),
  
  # ---------------- MODEL INTERPRETATION ----------------
  tabPanel(
    "Model Interpretation",
    fluidPage(
      titlePanel("Variable Importance"),
      
      selectInput(
        "vip_plot",
        "Select Model:",
        choices = c(
          "LGBM" = "vip_lgbm.png",
          "XGB"  = "vip_xgb.png"
        )
      ),
      
      tags$img(
        src = NULL,
        id = "vip_img",
        style = "width:100%; max-width:900px;"
      )
    )
  )
)

# ------------------------------------------------
# SERVER
# ------------------------------------------------
server <- function(input, output, session) {
  
  # ---------------- EDA ----------------
  output$yield_hist <- renderPlot({
    ggplot(merged_submission_boxplot_main,
           aes(x = yield_mg_ha)) +
      geom_histogram(bins = 30) +
      theme_minimal()
  })
  
  output$yield_site <- renderPlot({
    ggplot(merged_submission_boxplot_main,
           aes(x = site, y = yield_mg_ha)) +
      geom_boxplot() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---------------- PREDICTOR ----------------
  output$predictor_plot <- renderPlot({
    
    ggplot(merged_submission_boxplot_main,
           aes(x = .data[[input$var]], y = yield_mg_ha)) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "loess") +
      theme_minimal()
  })
  
  # ---------------- INTERACTIVE ----------------
  output$xy_plot <- renderPlot({
    
    ggplot(merged_submission_boxplot_main,
           aes(x = .data[[input$xvar]],
               y = .data[[input$yvar]])) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "loess") +
      theme_minimal()
  })
  
  # ---------------- MODEL PERFORMANCE (FIXED) ----------------
  output$obs_pred <- renderPlot({
    
    data <- merged_submission_boxplot_main %>%
      filter(!is.na(yield_mg_ha), !is.na(pred_yield))
    
    validate(need(nrow(data) > 1, "No valid data available"))
    
    ggplot(data,
           aes(x = yield_mg_ha, y = pred_yield)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      theme_minimal()
  })
  
  # ---------------- METRICS ----------------
  output$metrics <- renderText({
    
    data <- merged_submission_boxplot_main %>%
      filter(!is.na(yield_mg_ha), !is.na(pred_yield))
    
    validate(need(nrow(data) > 1, "Not enough data"))
    
    r2 <- cor(data$yield_mg_ha, data$pred_yield)^2
    rmse <- sqrt(mean((data$yield_mg_ha - data$pred_yield)^2))
    
    paste0("R² = ", round(r2, 3),
           " | RMSE = ", round(rmse, 2))
  })
}

# ------------------------------------------------
# RUN APP
# ------------------------------------------------
shinyApp(ui, server)