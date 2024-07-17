library(tidyverse)
library(pdftools)
library(ggokabeito)   # Colorblind-friendly color palette
library(showtext)
library(geomtextpath)

font_add("Fuzzy Bubbles", regular = "_posts/2021-12-15-university-points/fonts/FuzzyBubbles-Regular.ttf")
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

years <- c("07", "08", "09", "10", "13", 14:19 )
colleges <- tribble(~college, ~name,
                    "BN", "Blanchestown",
                    "CK", "UCC",
                    "CW", "Carlow IT",
                    "DC", "DCU",
                    "DK", "Dundalk IT",
                    "DN", "UCD",
                    "GY", "Univ Galway",
                    "LC", "Limerick IT",
                    "LM", "Univ Limerick",
                    "MH", "Maynooth",
                    "SG", "Sligo IT",
                    "TL", "Tralee IT",
                    "TR", "Trinity",
                    "TU", "TUD",
                    "WD", "W'ford IT")
cao_points_year <- function(year) {
  cao_pdf <- glue::glue("http://www2.cao.ie/points/lvl8_{year}.pdf")
  z <- pdf_text(cao_pdf) %>% 
    str_split("\n") %>% 
    unlist()
  z <- z[!str_detect(z, "^ ") & z != "" & !str_detect(z, "Course Code") & str_count(z, "  +") == 3] %>% # gets rid of non-data rows
    str_split("  +") %>% # splits rows based on runs of several spaces
    unlist() %>% 
    str_remove("#") %>% 
    str_remove("\\*") # deletes some annoying characters
  z <- tibble(year = glue::glue("20{year}"), 
              code = z[c(T, F, F, F)], 
              course = z[c(F, T, F, F)], 
              final = z[c(F, F, T, F)], 
              medium = z[c(F, F, F, T)]) 
  # original list made bunches of four elements that together described a course. This tibble winds them up to one row
  z
}

z <- map_df(years, cao_points_year) %>% 
  mutate(year = as.numeric(year))

z21 <- readxl::read_excel("_posts/2021-12-15-university-points/data/CAOPointsCharts2021.xlsx", 
                          sheet = "EOS_2021", skip = 10) %>% 
  janitor::clean_names() %>% 
  filter(course_level == 8) %>% 
  mutate(year = 2021) %>% 
  select(year,
         code = course_code, 
         course = course_title,
         final = eos_points,
         medium = eos_midpoints) %>% 
  mutate(year = 2021,
         medium = as.character(medium))
z20 <- readxl::read_excel("_posts/2021-12-15-university-points/data/CAOPointsCharts2020.xlsx", 
                          sheet = "PointsCharts2020_V2", skip = 9) %>% 
  janitor::clean_names() %>% 
  filter(level == 8) %>% 
  mutate(year = 2020) %>%  
  select(year,
         code = course_code2, 
         course = course_title,
         final = eos,
         medium = eos_mid_point) %>% 
  mutate(year = 2020)

z <- bind_rows(z, z20, z21) %>% 
  mutate(final = as.numeric(final), medium = as.numeric(medium)) %>%
  filter(!is.na(final), !is.na(medium)) %>% 
  mutate(college = str_sub(code, 1, 2)) %>% 
  left_join(colleges)

z %>% filter(college %in% c("TR", "TU", "MH")) %>% 
  ggplot(aes(x = final, colour = name)) +
  geom_textpath(aes(label = name), stat = "density",
                size = 12, fontface = 2, hjust = 0.2, vjust = 0.3,
                show.legend = F, linewidth = 2, family = "Fuzzy Bubbles") +
  scale_color_okabe_ito() +
  theme_clean() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  coord_cartesian(xlim = c(200, 700))
