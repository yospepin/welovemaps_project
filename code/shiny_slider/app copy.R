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
library(stringr)
merged_data_clean <- readRDS("/Users/elliespangler/Desktop/STAT112/challenges/welovemaps_project/data/mn_map_data.rds")
this_one_instead <- readRDS("/Users/elliespangler/Desktop/STAT112/challenges/welovemaps_project/data/mn_income_map.rds") %>% 
  dplyr::rename("2012" = "Real_2012", "2013" = "Real_2013", "2014" = "Real_2014", "2015" = "Real_2015", "2016" = "Real_2016", "2017" = "Real_2017", "2018" = "Real_2018", "2019" = "Real_2019", "2020" = "Real_2020", "2021" = "Real_2021", "2022" = "Real_2022") 
this_one_instead <- this_one_instead %>% 
  pivot_longer(cols = 15:25, names_to = "YEAR", values_to = "Income per capita") %>% 
  mutate(YEAR = as.numeric(YEAR))
coordinates <- st_centroid(this_one_instead) %>% 
  select(-YEAR, -`Income per capita`)
coordinates <- coordinates %>% 
  mutate(LON = st_coordinates(.)[,1],
         LAT = st_coordinates(.)[,2]) %>% 
  group_by(name) %>% 
  summarize(LON = mean(LON), LAT = mean(LAT))
# Load the tract-level income data
tract_income_data <- readRDS("/Users/elliespangler/Desktop/STAT112/challenges/welovemaps_project/code/income_race_data_2012_2022.rds")
minnesota_counties <- counties(state = "MN", cb = TRUE, class = "sf")
ui <- fluidPage(
  titlePanel("Minnesota Counties: Income and Racial Makeup Over Time"),
  tabsetPanel(
    tabPanel("Introduction",
             div(style = "text-align: center;",
                 img(src = "mapanimate.gif", height = "400px", width = "600px")
             ),
             p("Welcome to our interactive app for exploring income and racial 
         demographics in Minnesota counties. This tool allows you to analyze 
         demographic trends and patterns over time.In the County-Level Analysis 
         tab, you can click on any county to view detailed income and racial 
         demographic data over the years. Recognizing the need for a more 
         detailed perspective, we have also included a Tract-Level Analysis tab 
         that focuses on urban areas. We believe this detailed analysis at the 
         tract level offers deeper insights into the patterns and trends of 
         gentrification in Minnesota's urban communities.")),
    tabPanel("County-Level Analysis",
             sidebarLayout(
               sidebarPanel(
                 p("Click on a county to view income and racial demographics over time."),
                 sliderInput(
                   "year",
                   "Year Input",
                   min = 2012,
                   max = 2022,
                   value = 2012
                 )
               ),
               mainPanel(
                 leafletOutput("countyMap"),
                 h3("Income Trends"),
                 plotOutput("incomePlot"),
                 h3("Racial Makeup Trends"),
                 plotOutput("racePlot")
               ))),
    tabPanel("Tract-Level Analysis",
             sidebarLayout(
               sidebarPanel(
                 p("Use the slider to select a year and view income trends by census tract."),
                 sliderInput(
                   "tract_year",
                   "Select Year:",
                   min = 2012,
                   max = 2022,
                   value = 2012,
                   step = 1,
                   animate = TRUE
                 )
               ),
               mainPanel(
                 leafletOutput("tractMap"),
                 h3("Income Trends by Tract"),
                 plotOutput("tract_incomePlot"),
                 h3("Racial Makeup Trends by Tract"),
                 plotOutput("tract_racePlot")
               )
             ))
  )
)
server <- function(input, output, session) {
  
  output$countyMap <- renderLeaflet({
    filtered_income <- this_one_instead %>% filter(YEAR == input$year)
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = c(30000, 95000)
    )
    leaflet(filtered_income) %>%
      addTiles() %>%
      addPolygons(
        #fillColor = ~colorNumeric("viridis", `Income per capita`)(`Income per capita`),
        fillColor = ~pal(`Income per capita`),
        fillOpacity = 0.7,
        stroke = TRUE,  # borders
        color = "black",
        weight = 2,
        highlight = highlightOptions(weight = 3, color = "red", bringToFront = TRUE),
        label = ~paste(name, ": $", round(`Income per capita`, 2)),
        layerId = ~name
      ) %>% 
      addLegend("topright", pal = pal, values = c(30000, 95000),
                title = str_c("Per Capita Income ", input$year),
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.8
      )
  })
  
  observe(
    {
      click <- input$countyMap_shape_click
      if(is.null(click)){
        return()
      }else{
        sub <- coordinates[coordinates$name==click$id, c("LAT", "LON", "name")]
        latitude <- sub$LAT
        longitude <- sub$LON
        county_name <- sub$name
        
        leafletProxy("countyMap") %>%
          clearMarkers() %>%
          addMarkers(lng = longitude, lat = latitude, popup = county_name)
      }
    }
  )
  
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
  output$tractMap <- renderLeaflet({
    filtered_tract_income <- tract_income_data %>% filter(Year == input$tract_year)
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = filtered_tract_income$estimate
    )
    
    leaflet(filtered_tract_income) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(estimate),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE),
        label = ~paste0(tract, ": $", format(round(estimate, 0), big.mark = ","))
      ) %>%
      addLegend("bottomright", pal = pal, values = filtered_tract_income$estimate,
                title = paste("Median Income", input$tract_year),
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.7)
  })
  
  # Tract-Level Income Plot
  output$tractMap <- renderLeaflet({
    filtered_tract_income <- tract_income_data %>% filter(Year == input$tract_year)
    
    pal <- colorNumeric(
      palette = "viridis",
      domain = filtered_tract_income$estimate_adjusted
    )
    
    leaflet(filtered_tract_income) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(estimate_adjusted),
        fillOpacity = 0.7,
        color = "black",
        weight = 1,
        highlight = highlightOptions(weight = 2, color = "red", bringToFront = TRUE),
        label = ~paste0(tract.x, ": $", format(round(estimate_adjusted, 0), big.mark = ","))
      ) %>%
      addLegend("bottomright", pal = pal, values = filtered_tract_income$estimate_adjusted,
                title = paste("Median Income", input$tract_year),
                labFormat = labelFormat(prefix = "$"),
                opacity = 0.7)
  })
  
  # Reactive expression for selected tract
  selected_tract <- reactive({
    req(input$tractMap_shape_click$id)
    tract_income_data %>%
      filter(GEOID == input$tractMap_shape_click$id) %>%
      select(GEOID, tract.x, Year, estimate_adjusted, White, Black, Asian, Hispanic,
             American_Indian_Alaska_Native, Native_Hawaiian_Pacific_Islander, Two_or_More)
  })
  
  # Tract-Level Income Plot
  output$tract_incomePlot <- renderPlot({
    req(input$tractMap_shape_click$id)
    selected_tract_data <- selected_tract()
    selected_tract_name <- selected_tract_data$tract.x[1]
    if (is.na(selected_tract_name) || is.null(selected_tract_name)) {
      selected_tract_name <- selected_tract_data$GEOID[1]
    }
    
    ggplot(selected_tract_data, aes(x = Year, y = estimate_adjusted, group = GEOID)) +
      geom_line(size = 1, color = "blue") +
      geom_point(size = 2, color = "blue") +
      labs(
        title = paste("Income Trends for", selected_tract_name),
        x = "Year",
        y = "Median Income (Adjusted to 2022 $)"
      ) +
      theme_minimal()
  })
  
  # Tract-Level Race Plot
  output$tract_racePlot <- renderPlot({
    req(input$tractMap_shape_click$id)
    selected_tract_data <- selected_tract()
    selected_tract_name <- selected_tract_data$tract.x[1]
    if (is.na(selected_tract_name) || is.null(selected_tract_name)) {
      selected_tract_name <- selected_tract_data$GEOID[1]
    }
    
    race_columns <- c("White", "Black", "Asian", "Hispanic", 
                      "American_Indian_Alaska_Native", 
                      "Native_Hawaiian_Pacific_Islander", "Two_or_More")
    
    race_data_long <- selected_tract_data %>%
      select(Year, all_of(race_columns)) %>%
      pivot_longer(cols = all_of(race_columns), names_to = "Race", values_to = "Population")
    
    ggplot(race_data_long, aes(x = Year, y = Population, color = Race)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = paste("Racial Makeup Trends for", selected_tract_name),
        x = "Year",
        y = "Population",
        color = "Race"
      ) +
      theme_minimal()
  })
}
  
shinyApp(ui = ui, server = server)