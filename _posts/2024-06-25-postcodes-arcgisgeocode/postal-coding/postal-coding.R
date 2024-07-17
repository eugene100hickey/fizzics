library(tidyverse)
library(leaflet)
library(shiny)
library(bslib)
library(sf)
library(arcgisgeocode)

ui <- page_fillable(
  card(
    card_title(textOutput("rev_result")),
    leafletOutput("map", width = "100%", height = "100%"),
  )
)

server <- function(input, output, session){
  observeEvent(input$map_click, {
    click <- input$map_click
    x <- click$lng
    y <- click$lat
    loc <- c(x, y)
    dput(loc)
    
    geocoded <- reverse_geocode(loc)
    
    output$rev_result <- renderText(
      glue::glue("{geocoded$long_label}\n{geocoded$postal}")
    )
    
    leafletProxy("map", data = st_geometry(geocoded)) |> 
      clearMarkers() |> 
      addMarkers()
  })
  
  output$map <- renderLeaflet({
    leaflet() |> 
      addProviderTiles(providers$Esri.WorldGrayCanvas) |> 
      setView(lat = 53.32041, lng = -6.23956, zoom = 14)
  })
}


shinyApp(ui, server)

make_label <- function(geocoded){
  name <- geocoded$place_name
  
  if(nzchar(name)){
    name = "unknown address"
  }
}


