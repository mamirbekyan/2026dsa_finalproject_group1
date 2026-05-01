#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

library(shiny)

ui <- navbarPage(
  "Crop Modeling App",
  
  tabPanel(
    "Experimental Sites",
    
    fluidPage(
      titlePanel("Experimental Site Distribution Across the U.S."),
      
      fluidRow(
        column(
          width = 12,
          
          p("This map shows all experimental sites used in the training dataset. 
            Points are colored by yield (mg/ha)."),
          
          div(
            style = "text-align: center;",
            tags$img(
              src = "distributions_usa.png",
              style = "max-width: 800px; width: 100%; height: auto;"
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)