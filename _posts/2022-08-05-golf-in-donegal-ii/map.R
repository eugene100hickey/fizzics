library(tidyverse)
library(tm)
library(ggrepel)
library(leaflet)
library(htmltools)
library(geonames)
library(showtext)
library(tidygeocoder)

options(geonamesUsername="eugene100hickey")
z1 <- readRDS("_posts/2022-08-03-golf-courses-in-donegal/data/donegal")
z2 <- map(1:length(z1), function(x) z1[[x]]$result)
z3 <- data.table::rbindlist(z2) |> 
  mutate(par = as.factor(par)) |> 
  group_by(course) |> 
  summarise(total_length = sum(length)) |> 
  ungroup()
my_font <- "Josefin Slab"
my_font <- "Indie Flower"
font_add_google(my_font, family = my_font)
showtext_auto()
theme_set(theme_minimal() +
            theme(text = element_text(family = my_font, size = 28)))

geos <- geo(address = z3$course, method = "osm")

pal <- colorNumeric(
  palette = "Reds",
  domain = z3$total_length
)

z3 %>%
  left_join(geos, by = c("course" = "address")) |> 
  drop_na() |> 
  leaflet() %>%
  addProviderTiles("Esri.NatGeoWorldMap") %>%
  addMarkers(
    label = ~ course,
    labelOptions = labelOptions(noHide = F, direction = "top",
                                style = list(
                                  "color" = "red",
                                  "font-family" = "serif",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)"
                                )),
    
    lng = ~long,
    lat = ~lat
  ) |> 
  addMarkers(
    label = ~ ifelse(course == "Dunfanaghy Golf Club", "Dunfanaghy Golf", ""),
    labelOptions = labelOptions(noHide = T, direction = "top",
                                style = list(
                                  "color" = "red",
                                  "font-family" = "serif",
                                  "font-style" = "italic",
                                  "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                  "font-size" = "12px",
                                  "border-color" = "rgba(0,0,0,0.5)"
                                )),
    
    lng = ~long,
    lat = ~lat
  ) %>%
  setView(lng = -8, lat = 55, zoom = 8)
