# Load packages ----
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(maps)
library(lubridate)

# Load data ----
stations <- readRDS("data/stations.rds")
zips <- readRDS("data/mn_zips_ids.rds")
counties <- readRDS("data/mn_counties.rds")
data <- readRDS("data/mn_weather_2018_11_05.rds")

# User interface ----
ui <- fluidPage(
  titlePanel("Minnesota Weather Stations"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize the distribution of weather stations based upon criteria."),

      pickerInput("county",
                  label = "Counties:", choices = sort(unique(zips$county_name)),
                  multiple = TRUE, 
                  selected = unique(zips$county_name),
                  options = list(`actions-box` = TRUE)),
      
      pickerInput("zip",
                   label = "zip code", choices = sort(unique(zips$zipcode)),               
                   multiple = TRUE,
                   selected = unique(zips$zipcode), 
                  options = list(`actions-box` = TRUE)),
      sliderInput("pct_obs", 
                  label = "Minimum percent obs:",
                  min = .56, max = 1, value = .56 ),
      sliderInput("yrs",
                  label = "Select mininum number of years (1970-2018):",
                  min = 12, max = 48, value = 12),
      # Button
      downloadButton("downloadData", "Download selected data")
      ),
    
    mainPanel(plotOutput("map"), 
              dataTableOutput("table"))
  )
  )

# Server logic ----
server <- function(input, output) {
  output$map <- renderPlot({
    ggplot(data = counties, mapping = aes(x = long, y = lat,group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(color = "black", fill = "gray") +
      geom_point(
        data = filter(stations, pct_obs >= input$pct_obs,
                      yrs_range >= input$yrs,
                      id %in% subset(zips, zipcode %in% input$zip)$stn_id &
                        id %in% subset(zips, county_name %in% input$county)$stn_id),
        inherit.aes = FALSE, aes(x = longitude, y = latitude))
  })
  
  output$table <- renderDataTable({
    filter(stations, pct_obs >= input$pct_obs,
           yrs_range >= input$yrs, 
           id %in% subset(zips, zipcode %in% input$zip)$stn_id &
             id %in% subset(zips, county_name %in% input$county)$stn_id)
    
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(filename = "weatherdata.csv",
    content = function(file) {
    stnids <- filter(stations, pct_obs >= input$pct_obs,
                     yrs_range >= input$yrs, 
                     id %in% subset(zips, zipcode %in% input$zip)$stn_id &
                     id %in% subset(zips, county_name %in% input$county)$stn_id)
    wx<-filter(data, id %in% stnids$id)
      write.csv(wx, file, row.names = FALSE)
    })
}

# Run app ----
shinyApp(ui, server)