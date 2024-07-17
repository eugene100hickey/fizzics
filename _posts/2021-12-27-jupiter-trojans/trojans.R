# https://ssd.jpl.nasa.gov/tools/sbdb_query.html#!#results

library(tidyverse)
library(rvest)
library(lubridate)
library(ggokabeito)
library(ggtext)
library(ggpointdensity)
library(showtext)
font_add("Fuzzy Bubbles", regular = "_posts/2021-12-27-jupiter-trojans/fonts/FuzzyBubbles-Regular.ttf")
showtext_auto()

theme_clean <- function() {
  theme_minimal(base_family = "Fuzzy Bubbles") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 28, family = "Fuzzy Bubbles"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 32),
          axis.title = element_text(face = "bold", size = 36),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 24))
}


z <- read_csv("_posts/2021-12-27-jupiter-trojans/data/nasa-small-bodies-database.csv") %>%
  mutate(first_obs = as.Date(first_obs),
         designation = str_extract(full_name, pattern = "(?<=\\().*(?=\\))")) %>%
  dplyr::select(designation, pdes, name, spkid, diameter, albedo,
                rot_per, ma, n, first_obs)

url <- "https://www.minorplanetcenter.net/iau/lists/JupiterTrojans.html"

xml <- "pre"

w <- rvest::read_html(url)
pathway_data_html <- rvest::html_nodes(w, xml)
ev <- rvest::html_text(pathway_data_html) %>%
  str_split("\n") %>%
  unlist()
tjn <- ev[-c(1, 3, 10794)] %>%
  str_sub(start = 1, end = 8)
tjn <- tjn[-1] %>% 
  str_remove("\\(") %>% 
  str_remove("\\)") %>% 
  str_remove(" +")
ev <- ev[-c(1, 3)] %>%
  str_sub(start = 28) %>%
  str_replace_all(" +", " ")

trojans <- read_table(ev) %>%
  janitor::clean_names() %>%
  unite(col = design, prov:des, sep = " ", remove = F) %>% 
  mutate(des = str_sub(des, start=1, end = 1),
         pdes = tjn)
z <- z %>% left_join(trojans %>% select(pdes, design))
trojans <- trojans %>%
  select(-pdes) %>% 
  left_join(z)

# sizing
url_size <- "https://www.minorplanetcenter.net/iau/Sizes.html"
w <- rvest::read_html(url_size)
pathway_data_html <- rvest::html_nodes(w, xml)
ev <- rvest::html_text(pathway_data_html) %>%
  str_split("\n") %>%
  unlist()
ev <- ev[-c(1:4, 6, 37:47)] %>% 
  str_replace_all(" - ", "   ") %>% 
  str_replace_all(" +", " ")
ev[1] <- "H1 D_min_icy D_min D_max H2 H3"
trojan_dist <- read_table(ev) %>% 
  pivot_longer(cols = c(H1, H2, H3)) %>% 
  mutate(new_diameter = case_when(name == "H1" ~ (D_min + D_max)/2,
                           name == "H2" ~ (D_min + D_max)/2/1e3,
                           name == "H3" ~ (D_min + D_max)/2/1e6)) %>% 
  arrange(name, value) %>% 
  select(h = value, everything()) %>% 
  select(-name)

calibration <- loess(new_diameter ~ h, data = trojan_dist, span = 0.25)
trojans <- trojans %>% mutate(new_diam = predict(calibration, newdata = trojans),
                              diameter = ifelse(is.na(diameter), new_diam, diameter))

# trojans <- trojans %>% 
#   mutate(h_real = h,
#          h = round(h/0.5)*0.5) %>% 
#   left_join(trojan_dist, by = c("h" = "abs_mag"))

trojans %>% ggplot(aes(first_obs, fill = ln)) +
  geom_histogram(position = "dodge") +  
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               limits=c(dmy("1-1-1960"), dmy("1-4-20")))

trojans %>% ggplot(aes(first_obs, diameter, col = ln)) +
  geom_pointdensity(show.legend = F) +
  scale_y_log10() +
  scale_x_date(limits = c(ymd("1960-01-01"), ymd("2021-01-01"))) +
  geom_smooth(se = F)

trojans %>% ggplot(aes(rot_per)) +
  geom_histogram(bins = 50, position = "dodge") +
  scale_x_log10()

trojans %>% ggplot(aes(a, fill = ln)) +
  geom_histogram(bins = 50, position = "dodge") +
  scale_x_log10()

trojans %>% ggplot(aes(incl, fill = ln)) +
  geom_histogram(position = "dodge") +
  scale_x_log10()

trojans %>% 
  ggplot() +
  geom_histogram(aes(e, fill = ln, y = ..ncount..), position = "dodge") 

trojans %>% 
  filter(ln == "L4") %>% 
  ggplot(aes(a, e, col = ln)) +
  geom_point(size = 0.1)

trojans %>% 
  ggplot() +
  geom_histogram(aes(diameter, fill = ln, y = ..ncount..),
                 position = "dodge", 
                 show.legend = F) +
  scale_fill_okabe_ito() +
  scale_x_log10() +
  labs(title = "Rotational Period distributions<br>for the <span style='color:#E69F00;'>L4</span> and <span style='color:#56B4E9;'>L5</span> Jupiter Trojans.",
       x = "Albedo") +
  theme_clean() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        title = element_markdown())

trojans %>% 
  ggplot() +
  geom_histogram(aes(h, fill = ln, y = ..ncount..),
                 position = "dodge", 
                 show.legend = F) +
  scale_fill_okabe_ito() +
  scale_x_log10(breaks = seq(8, 15, by = 1)) +
  labs(title = "Diameter distributions<br>for the <span style='color:#E69F00;'>L4</span> and <span style='color:#56B4E9;'>L5</span> Jupiter Trojans.",
       x = "Albedo") +
  theme_clean() + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        title = element_markdown())

trojans %>% ggplot(aes(incl, a, col = ln)) +
  geom_pointdensity(show.legend = F, size= 0.13) +
  theme_minimal()

z %>% ggplot(aes(diameter, GM)) +
  geom_pointdensity(show.legend = F, size= 0.1)

trojans %>% ggplot(aes(q, q_2, col = ln)) +
  geom_point(size = 0.1)


# sf_sphere <-
#   rnaturalearth::ne_download(
#     scale = 10,
#     category = "physical",
#     type = "wgs84_bounding_box",
#     returnclass = "sf"
#   )
#
# sf_bbox <-
#   sf_sphere %>%
#   st_bbox() %>%
#   st_as_sfc()
#
# sf_outside <- st_difference(sf_bbox, sf_sphere)
