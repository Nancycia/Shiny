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
neighborhood_name<- c("Charlestown", "South Boston", "Dorchester", "East Boston Senior Center", "Hyde Park", "Alston Brighton"," Roxbury","Roslindale","East Boston","Maintenance","Bolling","BCYF Indoor","Main Streets Staging","Jamaica Plain","Strand Theatre - External","Strand Theatre - Internal","OPAT","City Hall Plaza and Pavilion","Parks")


ui <- 
  navbarPage("Ananlysis on Boston Wicked_Free_WiFi_Locations", collapsible = TRUE, inverse = TRUE, 
             theme = shinytheme("flatly"),
             tabPanel("Map", plotOutput("plot")),
             tabPanel("Without location",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Datatable"),
                          dataTableOutput("datatable1"),
                          radioButtons("neighborhood_name", "What's the neighborhood name?", neighborhood_name)
                        ))),
             tabPanel("With location",
                      fluidPage(
                        tabsetPanel(
                          tabPanel("Datatable"),dataTableOutput("datatable2")
                        )))
  )

server <- function(input, output) {
  output$plot <- renderPlot({
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
  output$datatable1 <- renderDataTable({
    
  })
  output$datatable2 <- renderDataTable({
    
  })
}

shinyApp(ui = ui, server = server)


    

