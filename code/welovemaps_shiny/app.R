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
library(tidyr)
merged_data_clean <- readRDS("../../data/mn_map_data.rds")
this_one_instead <- readRDS("../../data/mn_income_map.rds") %>% 
  rename("2012" = "Real_2012", "2013" = "Real_2013", "2014" = "Real_2014", "2015" = "Real_2015", "2016" = "Real_2016", "2017" = "Real_2017", "2018" = "Real_2018", "2019" = "Real_2019", "2020" = "Real_2020", "2021" = "Real_2021", "2022" = "Real_2022") 

this_one_instead <- this_one_instead %>% 
  pivot_longer(cols = 15:25, names_to = "YEAR", values_to = "Income per capita") %>% 
  mutate(YEAR = as.numeric(YEAR))

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
  
  output$countyMap <- renderLeaflet({
    leaflet(this_one_instead) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("viridis", `Income per capita`)(`Income per capita`),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
        label = ~paste(name, ": $", round(`Income per capita`, 2)),
        layerId = ~name
      )
  })
  
  selected_county <- reactive({
    req(input$countyMap_shape_click$id)
    merged_data_clean %>% filter(NAME == input$countyMap_shape_click$id)
  })
  
  output$incomePlot <- renderPlot({
    #browser()
    req(input$countyMap_shape_click$id)
    selected_county_name <- input$countyMap_shape_click$id
    this_one_instead <- this_one_instead %>%
      mutate(isSelected = ifelse(name == selected_county_name, "Selected", "Other"))
    
    ggplot(this_one_instead, aes(x = YEAR, y = `Income per capita`,  color = isSelected)) +
      geom_line(aes(group = name),size = 1, alpha = 0.3) + 
      geom_line(data = this_one_instead %>% filter(isSelected == 'Selected'),size = 1) +
      scale_color_manual(
        values = c("Selected" = "blue", "Other" = "grey")) +
      labs(
        title = paste("Income Trends for", selected_county_name),
        x = "Year",
        y = "Income per Capita ($)") +
      theme_minimal()
  })
  
  year_percentage_change <- function(x){
    (x - x[1]) / x[1] * 100}
  
  output$racePlot <- renderPlot({
    county_data <- selected_county()
    
    county_data <- county_data %>%
      select(YEAR, White, Black, Hispanic, Asian, Indian_Alaska, Hawaiian_PI, Two_or_more)
   
    county_data <- county_data %>%
      mutate(Non_White = Black + Hispanic + Asian + Indian_Alaska + Hawaiian_PI + Two_or_more)
      
    ggplot(county_data, aes(x = YEAR)) +
     geom_line(aes(y = year_percentage_change(White), color = "White Population")) +
     # geom_line(aes(y = year_percentage_change(Non_White), color = "Non-White Population")) +
     geom_line(aes(y = year_percentage_change(Hispanic), color = "Hispanic Population")) +
     geom_line(aes(y = year_percentage_change(Asian), color = "Asian Population")) +
     geom_line(aes(y = year_percentage_change(Indian_Alaska), color = "Indian/Alaska Population")) +
     geom_line(aes(y = year_percentage_change(Hawaiian_PI), color = "Hawaiian/PI Population")) +
     geom_line(aes(y = year_percentage_change(Two_or_more), color = "Two or More")) +
     labs(title = paste("Population Composition Trends for", county_data$GeoName[1]),
          y = "Percentage Change (%)",
          color = "Legend") +
     theme_minimal()
  })
}
shinyApp(ui = ui, server = server)