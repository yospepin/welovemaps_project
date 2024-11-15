#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)
merged_data_clean <- readRDS("../../data/mn_map_data.rds")
minnesota_counties <- counties(state = "MN", cb = TRUE, class = "sf")


ui <- fluidPage(
  titlePanel("Minnesota Counties: Income and Racial Makeup Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      p("Click on a county to view income and racial demographics over time.")
    ),
    
    mainPanel(
      leafletOutput("countyMap"),
      h3("Income Trends"),
      plotOutput("incomePlot"),
      h3("Racial Makeup Trends"),
      plotOutput("racePlot")
    )
  )
)

server <- function(input, output, session) {
  
  # Interactive Map
  output$countyMap <- renderLeaflet({
    leaflet(merged_data_clean) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("viridis", `Income per capita`)(`Income per capita`),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        highlight = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE),
        label = ~paste(NAME, ": $", round(`Income per capita`, 2)),
        layerId = ~NAME
      )
  })
  
  selected_county <- reactive({
    req(input$countyMap_shape_click$id)
    merged_data_clean %>% filter(NAME == input$countyMap_shape_click$id)
  })
  
  # Income Plot
  output$incomePlot <- renderPlot({
    county_data <- selected_county()
    
    ggplot(county_data, aes(x = YEAR, y = `Income per capita`)) +
      geom_line(color = "blue", size = 1) +
      labs(
        title = paste("Income Trends for", county_data$NAME[1]),
        x = "Year",
        y = "Income per Capita ($)"
      ) +
      theme_minimal()
  })
  
  # Race Composition Plot
  output$racePlot <- renderPlot({
    county_data <- selected_county()
    
    county_data_long <- county_data %>%
      select(YEAR, White, Black, Hispanic, Asian, Indian_Alaska, Hawaiian_PI, Two_or_more)
    
    ggplot(county_data, aes(x = YEAR)) +
      geom_line(aes(y = White, color = "White Population")) +
      geom_line(aes(y = Black, color = "Black Population")) +
      geom_line(aes(y = Hispanic, color = "Hispanic Population")) +
      geom_line(aes(y = Asian, color = "Asian Population")) +
      geom_line(aes(y = Indian_Alaska, color = "Indian/Alaska Population")) +
      geom_line(aes(y = Hawaiian_PI, color = "Hawaiian/PI Population")) +
      geom_line(aes(y = Two_or_more, color = "Two or More")) +
      labs(
        title = paste("Trends for", county_data$GeoName[1]),
        y = "Value",
        color = "Legend"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)