#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(sf)
library(plotly)
data_by_dist <- read_rds("data/diverse_data_by_dist.rds")
data_by_year <- read_csv("data/diverse_data_by_year.csv")
metro_names <- data_by_dist |> pull(metro_name) |> unique()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Locating neighborhood in the American metropolis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "selected_city",
            label = "Choose a city:",
            choices = metro_names,
            selected = "New York"
          ),
          sliderInput(
            inputId = "selected_span",
            label = "Choose a span:",
            value = 0.5,
            min = 0,
            max = 1
          )
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput(outputId = "distPlot")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
  output$distPlot <- renderPlotly({ # Note the curly braces that enclose the R code below
    p <- data_by_dist %>%
    st_drop_geometry() %>%
    filter(metro_name == input$selected_city) %>%
      ggplot(aes(x = distmiles, y = entropy)) +
      geom_point() +
      geom_smooth(method ="loess", span = input$selected_span, color = "red") +
      theme_minimal() +
      labs(x = "Diversity Score", y = "Distance in Miles from City Hall")

   ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
