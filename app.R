library(babynames)
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(scales)
library(stringr)
library(tidyr)

# TODO: Add the ability to combine names
# TODO: Add a download image button
# TODO: Add an option to have log scale
# TODO: Add an age distribution calculator
# TODO: Make slider have better ticks
# TODO: Make x-axis ticks better for smaller gaps
# TODO: Add an explanantion of the names input
# TODO: Error checking for no names or names not in the dataset
# TODO: Add a point whenever a line starts/ends
# TODO: Split into two files
# TODO: Use linetype when > 9 colours are needed

ui <- fluidPage(
  titlePanel("Baby Names"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        inputId = "names",
        label = "Names",
        choices = NULL,
        multiple = TRUE
      ),
      radioButtons(
        inputId = "sex",
        label = "Sex",
        choiceNames = c("Both", "Female", "Male"),
        choiceValues = c("B", "F", "M"),
        inline = TRUE
      ),
      sliderInput(
        inputId = "xrange",
        label = "X-axis range",
        min = 1880,
        max = 2020,
        value = c(1920, 2020),
        step = 5,
        sep = ""
      ),
      radioButtons(
        inputId = "yaxis",
        label = "Y-axis values",
        choiceNames = c("Proportion", "Number"),
        choiceValues = c("prop", "n"),
        inline = TRUE
      ),
      numericInput(
        inputId = "minbirths",
        label = "Minimum number of births",
        value = 5,
        min = 5
      ),
      hr(),
      p(
        "Baby name data for the USA from 1880 to 2017 provided by the SSA.",
        "This includes all names with at least 5 uses.",
        "Source code is available on ",
        a(href = "https://github.com/Hasnep/shinybabynames", "Github"),
        "."
      )
    ),
    mainPanel(
      plotOutput("main_plot"),
      DT::dataTableOutput("show_data")
    )
  )
)

server <- function(input, output, session) {
  # Update the select input's choices
  updateSelectizeInput(
    session = session,
    inputId = "names",
    choices = unique(babynames$name),
    selected = "Mary",
    server = TRUE
  )

  # Filter the data
  filtered_data <- reactive({
    plot_names <- tolower(input$names)

    all_plot_names <- plot_names %>%
      strsplit("\\+") %>%
      unlist() %>%
      str_to_title()

    data <- babynames %>%
      filter(is.element(name, all_plot_names)) %>%
      mutate(n = case_when(n >= input$minbirths ~ n)) %>%
      mutate(prop = case_when(n >= input$minbirths ~ prop)) %>%
      filter(input$xrange[1] <= year & year <= input$xrange[2])

    if (input$sex != "B") {
      data <- data %>%
        filter(sex == input$sex)
    }

    # for (i in length(plot_names)) {
    #   this_name_group <- strsplit(plot_names[i], "\\+") %>%
    #     unlist(.)
    #   if (length(this_name_group) > 1) {
    #     data_n <- data %>%
    #       filter(is.element(name, this_name_group))
    #     for (this_year in unique(data_n$year)) {
    #       for (this_sex in unique(data_n$sex)) {
    #         data_nys <- data_n %>%
    #           filter(year == this_year) %>%
    #           filter(sex == this_sex)
    #
    #         data <- data.frame(year = this_year,
    #                            sex = this_sex,
    #                            name = paste(this_name_group, collapse = " + "),
    #                            n = sum(data_nys$n),
    #                            prop = sum(data_nys$prop)) %>%
    #           rbind(., data)
    #       }
    #     }
    #     #data <- data %>% filter(!is.element(name, this_name_group))
    #   }
    # }

    data <- data %>%
      mutate(name = paste0(name, " (", sex, ")"))

    return(data)
  })

  # Create plot
  output$main_plot <- renderPlot({
    if (input$yaxis == "prop") {
      yaxis_labels <- scales::percent
    } else if (input$yaxis == "n") {
      yaxis_labels <- waiver()
    }

    ggplot(data = filtered_data(), mapping = aes_string(x = "year", y = input$yaxis, group = "name")) +
      geom_line(aes(colour = name)) +
      scale_x_continuous(
        limits = input$xrange,
        breaks = seq(input$xrange[1], input$xrange[2], 10)
      ) +
      scale_y_continuous(limits = c(0, NA), labels = yaxis_labels) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal()
  })

  # Create dataframe
  output$show_data <- DT::renderDataTable(drop_na(filtered_data(), "n"))
}

shinyApp(ui = ui, server = server)
