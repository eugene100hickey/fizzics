library(tidyverse)
library(ggmap)
library(tidygeocoder)

z <- read_csv("_posts/2021-09-24-university-rankings/data/2020-QS-World-University-Rankings.csv",
              skip = 1)

z <- tibble(university = z$...3, rank = 1:nrow(z))

geos <- tidygeocoder::geo(address = z$university, method = "osm")
geos1 <- ggmap::geocode(location = z$university, output = "latlona")
geos2 <- bind_cols(geos, geos1)
geos2 <- geos2 %>%
  mutate(lat = ifelse(is.na(lat...2), lat...5, lat...2),
         long = ifelse(is.na(long), lon, long)) %>%
  select(university = address...1, address = address...6, long, lat)
