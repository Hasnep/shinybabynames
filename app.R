library(babynames)
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(scales)

ui <- fluidPage(
  titlePanel("Baby Names"),
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "names",
                label = "Names",
                value = "Mary"
      ),
      radioButtons(inputId = "sex",
                   label = "Sex",
                   choiceNames = c("Both", "Female", "Male"),
                   choiceValues = c("B", "F", "M"),
                   inline = TRUE
      ),
      sliderInput(inputId = "xrange",
                  label = "X-axis range",
                  min = 1880, 
                  max = 2015,
                  value = c(1900, 2015),
                  step = 5,
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
                   value = 5,
                   min = 5) 
    ),
    mainPanel(
      plotOutput("main_plot"),
      DT::dataTableOutput("show_data")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    plot_names <- input$names %>% 
      strsplit(., "\\s") %>% 
      unlist(.)
    
    all_plot_names <- plot_names %>% 
      strsplit(., "\\+") %>% 
      unlist(.)
    
    data <- babynames %>%
      filter(is.element(name, all_plot_names))
    
    if (input$sex != "B") {data <- data %>%
      filter(sex == input$sex)
    }
    
    data <- data %>%
      filter(n >= input$minbirths) %>% 
      filter(input$xrange[1] <= year & year <= input$xrange[2])
    

      data <- data %>%
        mutate(name = paste0(name, " (", sex, ")"))

    
    return(data)
  })
  
  output$show_data <- DT::renderDataTable(filtered_data())
  
  output$main_plot <- renderPlot({
    
    data <- filtered_data()
    
    
    
    
    if (input$yaxis == "prop") {
      yaxis_labels <- scales::percent
    } else if (input$yaxis == "n") {
      yaxis_labels <- waiver()
    }
    
    ggplot(data = data, mapping = aes_string(x = "year", y = input$yaxis, group = "name")) +
      geom_line(aes(colour = name)) +
      scale_x_continuous(limits = input$xrange,
                         breaks = seq(input$xrange[1], input$xrange[2], 10)) +
      scale_y_continuous(labels = yaxis_labels) +
      scale_color_brewer(palette = "Set1")
    
  })
}

shinyApp(ui = ui, server = server)
