library(tidyverse)
library(rvest)
library(showtext)
library(ggokabeito)
library(viridis)
library(geomtextpath)
library(ggtext)
library(patchwork)

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

# z1 <- map(z$value, safely(course_card))
z1 <- readRDS("_posts/2022-08-03-golf-courses-in-donegal/data/donegal")
z2 <- map(1:length(z1), function(x) z1[[x]]$result)
z3 <- data.table::rbindlist(z2) |> 
  mutate(par = as.factor(par))

z3 |> ggplot(aes(yards, col = par, group = par)) +
  # geom_density(show.legend = F, size = 2) +
  geom_textdensity(aes(label = glue::glue("Par\n{par}"), fill = par), stat = "density",
                size = 12, fontface = 2, hjust = "ymax", vjust = 0.3, gap = T,
                show.legend = F, linewidth = 2, family = "Fuzzy Bubbles",
                lineheight = 0.5) +
  geom_rug(show.legend = F) +
  coord_cartesian(ylim=c(0, 0.015)) +
  scale_colour_okabe_ito() +
  theme_clean() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())


z3 |> 
  filter(par != 5 | yards > 400) |> 
  ggplot(aes(length, index, col = par, group = par)) + 
 # geom_point(show.legend = F) + 
  geom_smooth(se = F, show.legend = F) +
  labs(x = "Hole Length in Yards",
       y = "Easiness of Hole") +
  theme_clean()

z4 <- z3 |>
  group_by(course) |> 
  summarise(total_length = sum(yards),
            total_par = sum(par |> as.character() |> as.numeric()),
            highlight = ifelse(n() < 10, "nine", "eighteen")) |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish(),
         highlight = ifelse(course == "Dunfanaghy", "yes", highlight),
         course = course |> str_remove("Links"),
         course = ifelse(course == "Dunfanaghy", glue::glue("<strong><p style = 'color:#56B4E9'>{course}</p></strong>"), course))

nines <- z4 |> 
  filter(highlight == "nine")

z5 <- z3 |>
  group_by(course) |> 
  summarise(total_length = sum(yards),
            total_par = sum(par |> as.character() |> as.numeric()),
            highlight = ifelse(n() < 10, "nine", "eighteen")) |> 
  mutate(length_par = total_length |> round(0),
         length_par = ifelse(highlight == "nine", length_par*2, length_par),
         norm_par = ifelse(highlight == "nine", total_par*2, total_par),
         course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish(),
         highlight = ifelse(course == "Dunfanaghy", "yes", highlight),
         course = course |> str_remove("Links"),
         course = ifelse(course == "Dunfanaghy", glue::glue("<strong><p style = 'color:#56B4E9'>{course}</p></strong>"), course),
         course = fct_reorder(course, length_par),
         length_label = ifelse(length_par == max(length_par), 
                               glue::glue("{length_par |> format(nsmall = 0)} yards: Par = {norm_par}"), 
                               glue::glue("{length_par |> format(nsmall = 0)}: {norm_par}")))

# total length
z5 |> 
  ggplot(aes(course, length_par, fill = highlight)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = length_label), 
            size = 12, y=2000,
            family = "Fuzzy Bubbles",
            fontface = "bold") +
  scale_fill_manual(values = c("yes" = "#56B4E9", 
                               "nine" = "#E69F00", 
                               "eighteen" = "#D55E00")) +
  coord_flip() +
  theme_clean() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_markdown(),
        )

# par 3 average length
label_size <- 8
par_3 <- z3 |>
  filter(par == 3) |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish()) |> 
  group_by(course) |> 
  summarise(total_length = mean(yards),
            highlight = ifelse(course %in% nines$course, "nine", "eighteen")) |> 
  ungroup() |> 
  mutate(length_par = total_length |> round(1),
         highlight = ifelse(course == "Dunfanaghy", "yes", highlight),
         course = course |> str_remove("Links"),
         course = ifelse(course == "Dunfanaghy", glue::glue("<strong><p style = 'color:#56B4E9'>{course}</p></strong>"), course),
         course = fct_relevel(course, levels(z5$course)),
         length_label = ifelse(course == levels(z5$course) |> tail(1), 
                               glue::glue("{length_par |> format(nsmall = 1)} yards"), 
                               length_par |> format(nsmall = 1)))|> 
  distinct() |> 
  ggplot(aes(course, length_par, fill = highlight)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = length_label), 
            size = label_size, y=100,
            family = "Fuzzy Bubbles",
            fontface = "bold") +
  scale_fill_manual(values = c("yes" = "#56B4E9", 
                               "nine" = "#E69F00", 
                               "eighteen" = "#D55E00")) +
  coord_flip() +
  theme_clean() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_markdown(size=24),
        panel.grid = element_blank()
  )

# par 4 average length
par_4 <- z3 |>
  filter(par == 4) |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish()) |> 
  group_by(course) |> 
  summarise(total_length = mean(yards),
            highlight = ifelse(course %in% nines$course, "nine", "eighteen")) |> 
  ungroup() |> 
  mutate(length_par = total_length |> round(1),
         highlight = ifelse(course == "Dunfanaghy", "yes", highlight),
         course = course |> str_remove("Links"),
         course = ifelse(course == "Dunfanaghy", glue::glue("<strong><p style = 'color:#56B4E9'>{course}</p></strong>"), course),
         length_label = ifelse(course == levels(z5$course) |> tail(1), 
                               glue::glue("{length_par |> format(nsmall = 1)} yards"), 
                               length_par |> format(nsmall = 1)))|> 
  distinct() |> 
  add_row(course = "Foyle Centre Woodlands", 
          total_length = 0, 
          highlight = "nine", 
          length_par = 0, 
          length_label = "") |>
  mutate(course = fct_relevel(course, levels(z5$course))) |> 
  ggplot(aes(course, length_par, fill = highlight)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = length_label), 
            size = label_size, y=150,
            family = "Fuzzy Bubbles",
            fontface = "bold") +
  scale_fill_manual(values = c("yes" = "#56B4E9", 
                               "nine" = "#E69F00", 
                               "eighteen" = "#D55E00")) +
  coord_flip() +
  theme_clean() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()
  )

# par 5 average length
par_5 <- z3 |>
  filter(par == 5) |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish()) |> 
  group_by(course) |> 
  summarise(total_length = mean(yards),
            highlight = ifelse(course %in% nines$course, "nine", "eighteen")) |> 
  ungroup() |> 
  mutate(length_par = total_length |> round(1),
         highlight = ifelse(course == "Dunfanaghy", "yes", highlight),
         course = course |> str_remove("Links"),
         course = ifelse(course == "Dunfanaghy", glue::glue("<strong><p style = 'color:#56B4E9'>{course}</p></strong>"), course),
         length_label = ifelse(course == levels(z5$course) |> tail(1), 
                               glue::glue("{length_par |> format(nsmall = 1)} yards"), 
                               length_par |> format(nsmall = 1)))|> 
  distinct() |> 
  add_row(course = "Otway", 
          total_length = 0, 
          highlight = "nine", 
          length_par = 0, 
          length_label = "") |> 
  add_row(course = "Cruit Island", 
          total_length = 0, 
          highlight = "nine", 
          length_par = 0, 
          length_label = "") |>
  add_row(course = "Foyle Centre Woodlands", 
          total_length = 0, 
          highlight = "nine", 
          length_par = 0, 
          length_label = "") |>
  mutate(course = fct_relevel(course, levels(z5$course))) |> 
  ggplot(aes(course, length_par, fill = highlight)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = length_label), 
            size = label_size, y=200,
            family = "Fuzzy Bubbles",
            fontface = "bold") +
  scale_fill_manual(values = c("yes" = "#56B4E9", 
                               "nine" = "#E69F00", 
                               "eighteen" = "#D55E00")) +
  coord_flip() +
  theme_clean() + 
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()
  )

layout <- "AABBBCCCC"
par_3 + par_4 + par_5 + plot_layout(design = layout)


z3 |> group_by(par) |> 
  summarise(short = min(yards),
            long = max(yards)) |> 
  gt::gt()

nine_cards <- z3 |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish()) |> 
  filter(course %in% nines$course)

nine_type <- nine_cards |> 
  group_by(course) |> 
  summarise(index_type = sum(index)) |> 
  ungroup()

nine_cards <- nine_cards |> 
  left_join(nine_type) |> 
  mutate(index = case_when(index_type == 81 ~ index+1,
                           index_type == 90 ~ index-1,
                           index_type == 45 ~ index*2),
         hole = hole + 9) |> 
  select(-index_type)

otway_style_indices <- nine_type |> filter(index_type == 45) |> pull(course)

z30 <- z3 |> 
  mutate(course = course |> str_remove("Golf") |> 
           str_remove("Links") |>
           str_remove("Club") |> 
           str_remove("And") |> 
           str_remove("Hotel") |> 
           str_remove("International") |> 
           str_squish()) |> 
  mutate(index = ifelse(course %in% otway_style_indices, index*2 - 1, index)) |> 
  bind_rows(nine_cards)

z30 |> 
  filter(!(course %in% nines$course)) |> 
  group_by(index, par) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  ggplot(aes(x = index, y = n, fill = par)) +
  geom_col(show.legend = F) +
  scale_x_continuous(breaks = c(3, 6, 9, 12, 15, 18)) +
  scale_fill_manual(values = c("#009E73", "#0072B2", "#D55E00")) +
  labs(y = "",
       title = "Variation in Hole Index for 
       <strong><p style = 'color:#009E73'>Par 3's</p></strong>, 
       <strong><p style = 'color:#0072B2'>Par 4's</p></strong>, and 
       <strong><p style = 'color:#D55E00'>Par 5's</p></strong>") +
  theme_clean() +
  theme(plot.title = element_markdown(size = 36),
        axis.text.y = element_blank())
  
  