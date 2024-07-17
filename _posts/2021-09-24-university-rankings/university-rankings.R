library(tidyverse)
library(ggmap)
library(tidygeocoder)

z <- read_csv("_posts/2021-09-24-university-rankings/data/2020-QS-World-University-Rankings.csv",
              skip = 1,
              locale = readr::locale(encoding = "latin1"))


z <- tibble(university = z$...3, rank = 1:nrow(z))

# geos0 <- tidygeocoder::geo(address = z$university, method = "osm")
# geos1 <- ggmap::geocode(location = z$university, output = "latlona")
# geos2 <- bind_cols(geos0, geos1)
# geos <- geos2 %>%
#   mutate(lat = ifelse(is.na(lat...2), lat...5, lat...2),
#          long = ifelse(is.na(long), lon, long)) %>%
#   select(university = address...1, address = address...6, long, lat)

geos <- readRDS("_posts/2021-09-24-university-rankings/data/geos")
z1 <- z %>% left_join(geos)

z1 %>% 
  ggplot(aes(x= long, y = lat, colour = -rank)) + 
  geom_point(show.legend = F, size = 2.5, alpha = 0.5) + 
  theme_void()
