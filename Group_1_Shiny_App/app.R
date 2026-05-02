#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

ui <- fluidPage(
  
  titlePanel("Crop Yield EDA & Model Insights"),
  
  tabsetPanel(
    
    # ===================== BOXPLOTS =====================
    tabPanel(
      "EDA - Boxplots",
      fluidPage(
        uiOutput("boxplots_ui")
      )
    ),
    
    # ===================== DISTRIBUTIONS =====================
    tabPanel(
      "EDA - Distributions",
      fluidPage(
        uiOutput("dist_ui")
      )
    ),
    
    # ===================== EDA SOIL & WEATHER =====================
    tabPanel(
      "EDA - Soil & Weather Drivers",
      fluidPage(
        
        selectInput(
          "eda_driver",
          "Select Variable:",
          choices = c(
            "Soil pH" = "soilpH.png",
            "Soil K (ppm)" = "soilk_ppm.png",
            "Sum GDD" = "sum_gdd.png",
            "Sum Precipitation" = "sum_precip.png"
          )
        ),
        
        imageOutput("driver_img")
      )
    ),
    
    # ===================== MODEL INTERPRETATION =====================
    tabPanel(
      "Model Interpretation",
      fluidPage(
        
        h3("Variable Importance"),
        
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
    ),
    
    # ===================== PREDICTION PERFORMANCE =====================
    tabPanel(
      "Prediction Performance",
      fluidPage(
        
        selectInput(
          "pred_plot",
          "Select Model:",
          choices = c(
            "LGBM" = "Publication_ready_lgbm.png",
            "XGB"  = "Publication_ready_xgb.png"
          )
        ),
        
        imageOutput("pred_image")
      )
    )
    
  )
)

server <- function(input, output, session) {
  
  # ===================== BOXPLOTS =====================
  output$boxplots_ui <- renderUI({
    
    files <- list.files("www/boxplots", full.names = FALSE)
    
    tagList(
      lapply(files, function(f) {
        tags$img(
          src = paste0("boxplots/", f),
          style = "width:320px; margin:10px;"
        )
      })
    )
  })
  
  # ===================== DISTRIBUTIONS =====================
  output$dist_ui <- renderUI({
    
    files <- list.files("www/plots_raw_yield", full.names = FALSE)
    
    tagList(
      lapply(files, function(f) {
        tags$img(
          src = paste0("plots_raw_yield/", f),
          style = "width:320px; margin:10px;"
        )
      })
    )
  })
  
  # =====================SOIL & WEATHER =====================
  output$driver_img <- renderImage({
    
    req(input$eda_driver)
    
    list(
      src = paste0("www/eda/", input$eda_driver),
      contentType = "image/png",
      width = 750
    )
    
  }, deleteFile = FALSE)
  
  # ===================== VARIABLE OF IMPORTANCE =====================
  output$vip_image <- renderImage({
    
    req(input$vip_plot)
    
    list(
      src = paste0("www/", input$vip_plot),
      contentType = "image/png",
      width = 700
    )
    
  }, deleteFile = FALSE)
  
  # ===================== OBSERVED VS PREDICTED =====================
  output$pred_image <- renderImage({
    
    req(input$pred_plot)
    
    list(
      src = paste0("www/", input$pred_plot),
      contentType = "image/png",
      width = 700
    )
    
  }, deleteFile = FALSE)
  
}

shinyApp(ui, server)