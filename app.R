library(babynames)
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Baby Names"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "names",
                label = "Names",
                value = "Hannes",
                placeholder = "Enter the names seperated by spaces or plusses to combine"
      ),
      radioButtons(inputId = "sex",
                   label = "Sex",
                   choiceNames = c("Both", "Female", "Male"),
                   choiceValues = c(TRUE, "F", "M"),
                   inline = TRUE
      ),
      sliderInput(inputId = "xrange",
                  label = "X-axis range",
                  min = 1880, 
                  max = as.integer(format(Sys.Date(), "%Y")),
                  value = c(1900, 2015),
                  round = TRUE,
                  sep = ""
                  ),
      radioButtons(inputId = "yaxis",
                   label = "Y-axis values",
                   choiceNames = c("Proportion", "Number"),
                   choiceValues = c("prop", "n"),
                   inline = TRUE
      ),
      numericInput(inputId = "minbirths",
                   label = "Minimum number of births",
                   value = 10,
                   min = 5) 
    ),
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

server <- function(input, output) {
 
}

shinyApp(ui = ui, server = server)
