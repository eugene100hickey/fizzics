library(tidyverse)
library(rvest)
library(showtext)
library(ggokabeito)
library(viridis)
library(geomtextpath)

font_add_google(name =  "Fuzzy Bubbles", family = "Fuzzy Bubbles")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 20, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 20),
          axis.title = element_text(face = "bold", size = 24),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}
# library(stringi)
# require(handlr)

# url <- "https://www.golfpass.com/travel-advisor/courses/19714-letterkenny-golf-club#scorecard"
# url <- "https://www.golfpass.com/travel-advisor/courses/19713-dunfanaghy-golf-club#scorecard"
# url <- "https://www.golfpass.com/travel-advisor/destinations/201-dublin-ireland#courses"
# xml <- "td"

url_donegal <- "https://www.golfpass.com/travel-advisor/course-directory/8812-county-donegal/"

w <- read_html(url_donegal)
z <- html_attr(html_nodes(w, "a"), "href") |> 
  as_tibble() |> 
  filter(str_detect(value, "/courses/"),
         !str_detect(value, "#write-review")) |> 
  distinct()




course_card <- function(url) {
  course <- sub(".*courses/\\d+-", "", url) |> 
    str_replace_all("-", " ") |> 
    str_to_title()
  
  w <- read_html(url)
  pathway_data_html <- html_nodes(w, "td")
  card <- html_text(pathway_data_html)
  
  unit <- card[3] |> str_extract("[a-z]+")
  total_length <- card[3] |> str_extract("\\d+") |> as.numeric()
  
  lengths <- card[(which(card |> str_detect(": \\d\\d\\.\\d"))[1]+1):(which(card |> str_detect(": \\d\\d\\.\\d"))[1]+20)]
  indexs <- card[(which(card |> str_detect("Handicap$"))+1):(which(card |> str_detect("Handicap$"))+20)]
  par <- card[(which(card |> str_detect("Par"))+1):(which(card |> str_detect("Par"))+20)]
  my_card <- tibble(course = rep(course, 18),
                        hole = 1:18,
                        par = c(par[1:9], par[11:19]) |> as.numeric(),
                        length = c(lengths[1:9], lengths[11:19]) |> as.numeric(),
                        # unit = rep(unit, 18),
                        index = c(indexs[1:9], indexs[11:19]) |> as.numeric(),
                        # yards = ifelse(unit == "yards", length, (length * 1.09361) |> round())
                    )
  my_card <- my_card |> drop_na()
  if(dim(my_card)[1] < 18) {
    my_card = filter(my_card, hole<=9)
  } 
  hole_lengths <- sum(my_card$length)
  if(dim(my_card)[1] == 9) {
    hole_lengths = 2 * hole_lengths
  }
  # total length given in yards but holes in meters
  if(total_length > hole_lengths * 1.05){
    my_card <- my_card |> 
      mutate(unit = "meters",
             yards = length * 1.09361)
  }
  # total length given in meters but holes in yards
  if(total_length < hole_lengths * 1.05){
    my_card <- my_card |> 
      mutate(unit = "yards",
             yards = length)
  }

  my_card |> select(course, hole, par, length, unit, index, yards)
}

z1 <- map(z$value, safely(course_card))
z2 <- map(1:length(z1), function(x) z1[[x]]$result)
z3 <- data.table::rbindlist(z2) |> 
  mutate(par = as.factor(par))

donegal_golf <- map(scorecard_urls$value, safely(course_card)) |> 
  map(1:length(scorecard_urls$value), function(x) donegal_golf[[x]]$result) |> 
  data.table::rbindlist()

z3 |> ggplot(aes(yards, col = par, group = par)) +
  geom_density(show.legend = F, size = 2) +
  geom_textpath(aes(label = glue::glue("Par {par}")), stat = "density",
                size = 12, fontface = 2, hjust = 0.2, vjust = 0.1, gap = T,
                show.legend = F, linewidth = 2, family = "Fuzzy Bubbles") +
  geom_rug(show.legend = F) +
  scale_colour_okabe_ito() +
  theme_clean() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())


z3 |> ggplot(aes(length, index, col = par, group = par)) + 
  geom_point(show.legend = F) + 
  geom_smooth(se = F, show.legend = F)

z3 |> group_by(par) |> 
  summarise(short = min(yards),
            long = max(yards))


