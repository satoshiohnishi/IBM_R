# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  city_list <- c("All", "Seoul", "Suzhou", "London", "New York", "Paris")
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  
  # Test generate_city_weather_bike_data() function
  city_weather_bike_df <- test_weather_data_generation()

  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  # Open server.R and locate the shiny server function within. In this function, you need to create a new data frame called cities_max_bike to store the city location and max bike prediction values.
  #The new data frame is an aggregated version of city_weather_bike_df. You may use group_by with any ‘finding max’ logic to calculate the max bike prediction for each city.
  # Simulated data (replace with actual aggregation)
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII) %>%
    summarize(LAT = first(LAT),
              LNG = first(LNG),
              max_bike_pred = max(BIKE_PREDICTION),
              bike_level = first(BIKE_PREDICTION_LEVEL),
              TEMPERATURE = first(TEMPERATURE),
              HUMIDITY = first(HUMIDITY),
              FORECASTDATETIME = first(FORECASTDATETIME),
              label = first(LABEL))
  
  output$city_bike_map <- renderLeaflet({
    leaflet(data = cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(~LNG, ~LAT, 
                       radius = ~ifelse(bike_level == "small", 6, ifelse(bike_level == "medium", 10, 12)),
                       color = ~ifelse(bike_level == "small", "green", ifelse(bike_level == "medium", "yellow", "red")),
                       popup = ~label) %>%
      addMarkers(~LNG, ~LAT, 
                 label = ~label, 
                 labelOptions = labelOptions(noHide = TRUE))
  })
  
  
  observeEvent(input$city_dropdown, {
    if (input$city_dropdown != "All") {
      
      selected_city <- filter(cities_max_bike, CITY_ASCII == input$city_dropdown)
      city_lat <- selected_city$LAT
      city_lng <- selected_city$LNG
      
leafletProxy("city_bike_map") %>%
  clearMarkers() %>%
  setView(lng = city_lng, lat = city_lat, zoom = 15) %>%
  addCircleMarkers(data = cities_max_bike, 
                   lng = ~LNG, lat = ~LAT, 
                   radius = ~ifelse(bike_level == "small", 6, ifelse(bike_level == "medium", 10, 12)),
                   color = ~ifelse(bike_level == "small", "green", ifelse(bike_level == "medium", "yellow", "red")),
                   popup = ~paste0("<strong>City: </strong>", CITY_ASCII, "<br>",
                                   "<strong>Temperature: </strong>", TEMPERATURE, "<br>",
                                   "<strong>Humidity: </strong>", HUMIDITY, "%<br>",
                                   "<strong>Label: </strong>", label))
      
      
      
      # Add a line plot for temperature selected city
      weather_data <- city_weather_bike_df %>%
        filter(CITY_ASCII == input$city_dropdown)
      
      weather_data$FORECASTDATETIME <- as.POSIXct(weather_data$FORECASTDATETIME)
      
      
      output$temp_line <- renderPlot({
        ggplot(data = weather_data, aes(x = FORECASTDATETIME, y = TEMPERATURE)) +
          labs(title = "Temperature over time", x = "Forecast Date Time", y = "Temperature") +
          # too add point and connect them with line
          geom_point(color = "red") +
          geom_line(color = "blue") +
          # set x-axis units 1 day
          scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d %H:%M") +
          # add text
          geom_text(aes(label = TEMPERATURE), hjust = 0, vjust = 0)
        
      })
      
      # add a line plot for bike prediction
      output$bike_line <- renderPlot({
        ggplot(data = weather_data, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          labs(title = "Bike Prediction over time", x = "Forecast Date Time", y = "Bike Prediction") +
          geom_point(color = "blue") +
          geom_line(color = "red") +
          scale_x_datetime(date_breaks = "1 day", date_labels = "%m-%d %H:%M") +
          geom_text(aes(label = BIKE_PREDICTION), hjust = 0, vjust = 0)
      })
      
      # Users then should able to click on the trend line to show the bike-sharing prediction and datetime values so let's add a text output rendering here.
      # verbatimTextOutput("bike_date_output")inui.R
      # show datetime and bike prediction(integers) when user click on the plot
      output$bike_date_output <- renderText({
        req(input$plot_click)
        click <- input$plot_click

        x <- click$x 
        y <- click$y
        # convert x from unixtime to string yyyy-mm-dd hh:mm:ss and y to integer
        paste("Time: ", as.character(as.POSIXct(x, origin="1970-01-01")), "Bike Prediction: ", as.integer(y))
      })
      

      
      # regression plot for humidity and bike prediction with poly4 model
      output$humidity_pred_chart <- renderPlot({
        ggplot(data = weather_data, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          labs(title = "Humidity vs Bike Prediction", x = "Humidity", y = "Bike Prediction") +
          geom_point(color = "green") +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4), se = TRUE) +
          geom_text(aes(label = BIKE_PREDICTION), hjust = 0, vjust = 0)
      })
      
        

      
      
                         
    } else {
      leafletProxy("city_bike_map") %>%
        clearMarkers() %>%
        addCircleMarkers(data = cities_max_bike, 
                         lng = ~LNG, lat = ~LAT, 
                         radius = ~ifelse(bike_level == "small", 6, ifelse(bike_level == "medium", 10, 12)),
                         color = ~ifelse(bike_level == "small", "green", ifelse(bike_level == "medium", "yellow", "red")),
                         popup = ~label)
      

  
      
      }
  })
})
  
      