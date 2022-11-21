library(shiny)
library(shinythemes)
library(geojsonio)
library(broom)
library(sf)
library(osmdata)
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(mapproj)
wf <- read.csv("https://raw.githubusercontent.com/Nancycia/Shiny_group7/main/Wicked_Free_WiFi_Locations.csv")
spdf_file <- geojson_read( "https://raw.githubusercontent.com/Nancycia/Shiny_group7/main/ZIP_Codes.geojson",what = "sp")
nwf <- wf%>%
  dplyr::select(lon=device_long,lat=device_lat,group=neighborhood_name,id=neighborhood_id)
stats_df <- as.data.frame(spdf_file)
spdf_file <- tidy( spdf_file,region="ZIP5" )
p <- unique(wf[4])
p1 <- p$neighborhood_name


ui <- 
  navbarPage("Ananlysis on Boston Wicked_Free_WiFi_Locations", collapsible = TRUE, inverse = TRUE, 
             theme = shinytheme("flatly"),
             # First tab for the map
             tabPanel("Map", plotOutput("plot1")),
             # Second tab for all data related to Wifi locations
             tabPanel("Datatable without location selection",
                      dataTableOutput("datatable1")
                        ),
             # Third tab for data related to different locations based on your choice
             tabPanel("With location selection",
                      fluidPage(
                        selectInput("neighborhood", "Choose your location", p1),
                        textOutput("text1"),
                        plotOutput("plot2"),
                        dataTableOutput("datatable2")
  )))

server <- function(input, output) {
  # Map under the first tab
  output$plot1 <- renderPlot({
    ggplot() +
      geom_polygon(data=spdf_file,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.5) +
      geom_point(data=nwf,
                 aes(x=lon,
                     y=lat),
                 fill="red",
                 alpha=.6,
                 size=1,
                 shape=22) +
      theme_void() +
      coord_map() +
      labs(title="Wicked Free Wifi in Boston")
  })
  # Data table under the second tab
  output$datatable1 <- renderDataTable({
    wf
  })
  # Data table under the third tab 
  output$datatable2 <- renderDataTable(filter(wf,neighborhood_name==input$neighborhood),options = list(pageLength = 5))
  # Place for choosing the location you want
  output$text1 <- renderText({ 
    paste("You have selected", input$neighborhood)})
  # Map under the third tab
  output$plot2 <- renderPlot(
    ggplot() +
      geom_polygon(data=spdf_file,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.2) +
      geom_point(data=filter(nwf,group==input$neighborhood),
                 aes(x=lon,
                     y=lat),
                 fill="red",
                 alpha=.6,
                 size=3,
                 shape=22) +
      theme_void() +
      coord_map() +
      labs(title="Wicked Free Wifi in Boston"))
  
}

shinyApp(ui = ui, server = server)