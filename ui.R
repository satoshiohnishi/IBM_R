# Load required libraries
library(shiny)
library(leaflet)

city_list <- c("All", "Seoul", "Suzhou", "London", "New York", "Paris")


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput(outputId="city_bike_map", height=1000),
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
      # select drop down list to select city
      selectInput(inputId="city_dropdown", label="Select a city", choices=city_list, selected="All"),
      
      # show temperature line plot
      plotOutput(outputId="temp_line", height=280),
      
      # add a plot output with in sidebarPanel() with id bike_line and a click event click = "plot_click"
      
      plotOutput(outputId="bike_line", click = "plot_click", height=280),
      
      # add a verbatimTextOutput("bike_date_output") text output with id bike_date_output. So that the point you click on the plot can be shown in here.
      verbatimTextOutput("bike_date_output"),
      
      plotOutput(outputId="humidity_pred_chart", height=280),
      
      
    ))
))