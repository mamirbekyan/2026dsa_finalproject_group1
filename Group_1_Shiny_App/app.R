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
      fluidRow(
        column(6, plotOutput("yield_hist")),
        column(6, plotOutput("yield_site"))
      )
    )
  ),
  
  # ---------------- PREDICTOR VS YIELD ----------------
  tabPanel(
    "Predictor vs Yield",
    fluidPage(
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
  
  # ---------------- INTERACTIVE SCATTER ----------------
  tabPanel(
    "Interactive Exploration",
    fluidPage(
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
  
  # ---------------- NEW: INTERACTIVE BOXPLOT ----------------
  tabPanel(
    "Interactive Yield Boxplot",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "box_var",
            "Group yield by:",
            choices = c("site", "year", "hybrid", vars_to_plot)
          )
        ),
        mainPanel(
          plotOutput("yield_box_interactive")
        )
      )
    )
  ),
  
  # ---------------- MODEL PERFORMANCE ----------------
  tabPanel(
    "Model Performance",
    fluidPage(
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
      selectInput(
        "vip_plot",
        "Select Model:",
        choices = c(
          "LGBM" = "vip_lgbm.png",
          "XGB"  = "vip_xgb.png"
        )
      ),
      imageOutput("vip_image")
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
  
  # ---------------- INTERACTIVE SCATTER ----------------
  output$xy_plot <- renderPlot({
    ggplot(merged_submission_boxplot_main,
           aes(x = .data[[input$xvar]],
               y = .data[[input$yvar]])) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "loess") +
      theme_minimal()
  })
  
  # ---------------- NEW: INTERACTIVE BOXPLOT ----------------
  output$yield_box_interactive <- renderPlot({
    
    var <- input$box_var
    
    plot_data <- merged_submission_boxplot_main %>%
      filter(!is.na(yield_mg_ha)) %>%
      mutate(group_var = .data[[var]])
    
    ggplot(plot_data,
           aes(x = factor(group_var), y = yield_mg_ha)) +
      geom_boxplot() +
      geom_jitter(width = 0.2, alpha = 0.3) +
      theme_minimal() +
      labs(
        x = var,
        y = "Yield (mg/ha)"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---------------- MODEL PERFORMANCE (FIXED) ----------------
  output$obs_pred <- renderPlot({
    
    data <- merged_submission_boxplot_main %>%
      transmute(
        yield_mg_ha = as.numeric(yield_mg_ha),
        pred_yield  = as.numeric(pred_yield)
      ) %>%
      filter(is.finite(yield_mg_ha), is.finite(pred_yield))
    
    validate(need(nrow(data) > 1, "No valid data for model performance"))
    
    ggplot(data,
           aes(x = yield_mg_ha, y = pred_yield)) +
      geom_point(alpha = 0.5) +
      geom_abline(slope = 1, intercept = 0, color = "red") +
      theme_minimal()
  })
  
  output$metrics <- renderText({
    
    data <- merged_submission_boxplot_main %>%
      transmute(
        yield_mg_ha = as.numeric(yield_mg_ha),
        pred_yield  = as.numeric(pred_yield)
      ) %>%
      filter(is.finite(yield_mg_ha), is.finite(pred_yield))
    
    validate(need(nrow(data) > 2, "Not enough data"))
    
    r2 <- cor(data$yield_mg_ha, data$pred_yield)^2
    rmse <- sqrt(mean((data$yield_mg_ha - data$pred_yield)^2))
    
    paste0(
      "R² = ", round(r2, 3),
      " | RMSE = ", round(rmse, 2)
    )
  })
  
  # ---------------- VIP IMAGE ----------------
  output$vip_image <- renderImage({
    list(
      src = file.path("www", input$vip_plot),
      contentType = "image/png",
      alt = "Variable Importance",
      width = "100%"
    )
  }, deleteFile = FALSE)
}

# ------------------------------------------------
# RUN APP
# ------------------------------------------------
shinyApp(ui, server)