library(babynames)
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Baby Names"),
  sidebarLayout(
    sidebarPanel(
      # place inputs here
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

server <- function(input, output) {
  
  output$main_plot <- renderPlot({
# place plot code here
  })
}

shinyApp(ui = ui, server = server)
