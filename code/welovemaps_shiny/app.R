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

#Data Loading and Cleaning
merged_data_clean <- readRDS("../../data/mn_map_data.rds")
with_inflation <- readRDS("../../data/mn_income_map.rds") %>% 
  dplyr::rename("2012" = "Real_2012", "2013" = "Real_2013", "2014" = "Real_2014", "2015" = "Real_2015", "2016" = "Real_2016", "2017" = "Real_2017", "2018" = "Real_2018", "2019" = "Real_2019", "2020" = "Real_2020", "2021" = "Real_2021", "2022" = "Real_2022") 

with_inflation <- with_inflation %>% 
  pivot_longer(cols = 15:25, names_to = "YEAR", values_to = "Income per capita") %>% 
  mutate(YEAR = as.numeric(YEAR))

coordinates <- st_centroid(with_inflation) %>% 
  select(-YEAR, -`Income per capita`)
coordinates <- coordinates %>% 
  mutate(LON = st_coordinates(.)[,1],
         LAT = st_coordinates(.)[,2]) %>% 
  group_by(name) %>% 
  summarize(LON = mean(LON), LAT = mean(LAT))

minnesota_counties <- counties(state = "MN", cb = TRUE, class = "sf")

#User Interface
ui <- fluidPage(
  titlePanel("The Geography of Wealth and Identity in Minnesota Counties"),
  tabsetPanel(
    tabPanel("Introduction",
             fluidRow(
               column(6,
                      h3("Research Focus"),
                      h4("How Space Entrenches Inequality"),
                      p("Space and place actively influence access to resources, jobs, healthcare, and education rather than serving as a passive 
                        backdrop for economic activity. These spatial dynamics have the potential to perpetuate inequality since families in wealthier 
                        regions are better equipped to amass wealth, while those in lower-income neighborhoods encounter barriers."),
                      h4("Objectives of This Study"),
                      p("The aim of this study is to create an interactive tool to visualize how income and racial demographics have changed over the years. 
                         By providing a dynamic platform for exploration, this tool aims to offer insights into how these two factors correlate and uncover 
                         patterns that may help our audiences to better understand the dynamics of economic and racial inequality in Minnesota."),
                      h4("Key Research Questions"),
                      tags$ul(
                        tags$li("How Do Racial and Ethnic Demographic Shifts Correlate with Income Changes?"),
                        tags$li("How have rural vs. urban counties in Minnesota been affected by demographic changes in relation to income 
                                distribution?")
                      ),
                      h4("Mapping and Findings"),
                      p("Plotting income trends alongside population changes in important racial and ethnic groupings will help us better 
                        understand how changes in these demographics relate to changes in income. Additionally, a choropleth map will be 
                        used to highlight areas of concentrated wealth and poverty across Minnesota."),
                      p("Key findings include correlations between racial demographic shifts and income changes, the role of education in 
                        economic resilience, and the need for targeted policies in rural areas experiencing demographic decline.")
               ),
               column(6,
                      h4("Map Animation"),
                      p("Below is a preview of our interactive visualization showcasing an animation of income trends over the years of 2012-2022 in Minnesota counties."),
                      div(style = "text-align: center;",
                          img(src = "mapanimate.gif", height = "400px", width = "600px")
                      )
               )
             )
    ),
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
               )
             )
    ),
    tabPanel("Tract-Level Analysis",
             sidebarLayout(
               sidebarPanel(
                 p("Click on a tract to view income and racial demographics over time.")
               ),
               mainPanel(
                 leafletOutput("tractMap"),
                 h3("Income Trends"),
                 plotOutput("tract_incomePlot"),
                 h3("Racial Makeup Trends"),
                 plotOutput("tract_racePlot")
               )
             )
    ),
    tags$footer(
      style = "text-align: center; padding: 10px; margin-top: 20px; background-color: #f8f9fa; border-top: 1px solid #dee2e6;",
      p("© Yosephine, Ellie, Iris. All rights reserved."),
      p("Data sources: US Census Bureau, Bureau of Economic Analysis.")
    )
  )
)


server <- function(input, output, session) {
  
  output$countyMap <- renderLeaflet({
    filtered_income <- with_inflation %>% filter(YEAR == input$year)
    
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
    with_inflation <- with_inflation %>%
      mutate(isSelected = ifelse(name == selected_county_name, "Selected", "Other"))
    
    ggplot(with_inflation, aes(x = YEAR, y = `Income per capita`,  color = isSelected)) +
      geom_line(aes(group = name),size = 1, alpha = 0.3) + 
      geom_line(data = with_inflation %>% filter(isSelected == 'Selected'),size = 1) +
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
  
  # output$tractMap
  # output$tract_incomePlot
  # output$tract_racePlot
}
shinyApp(ui = ui, server = server)