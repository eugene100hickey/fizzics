# https://github.com/WhyR2020/workshops/blob/master/satellite/whyr_satellite.R
# https://www.gsi.ie/en-ie/data-and-maps/Pages/Bedrock.aspx#100k
library(tidyverse)
library(sf)
library(sen2r)
library(raster)
library(RStoolbox)
library(exactextractr)
# library(terra)
library(showtext)
library(ggokabeito)

font_add(family = "my_font", regular = "fonts/AnnieUseYourTelescope-Regular.ttf")
showtext_auto()
theme_clean <- function() {
  theme_minimal(base_family = "my_font") +
    theme(panel.grid.minor = element_blank(),
          text = element_text(size = 16, family = "my_font"),
          plot.background = element_rect(fill = "white", color = NA),
          axis.text = element_text(size = 24),
          axis.title = element_text(face = "bold", size = 28),
          strip.text = element_text(face = "bold", size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.text = element_text(size = 16))
}

ref <- st_read("_posts/2023-01-17-satellites-and-bedrock/data/Shapefiles/Bedrock_Polygons_ITM_2018.shp",
               quiet = TRUE)
my_proj <- "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs"
ref <- st_transform(ref, crs = my_proj)

prefix <- "../"

my_file_red_june <- glue::glue("{prefix}data/spring-2022/T29UNA_20220604T114401_B02.jp2")
my_file_green_june <- glue::glue("{prefix}data/spring-2022/T29UNA_20220604T114401_B03.jp2")
my_file_blue_june <- glue::glue("{prefix}data/spring-2022/T29UNA_20220604T114401_B04.jp2")
my_file_nir_june <- glue::glue("{prefix}data/spring-2022/T29UNA_20220604T114401_B08.jp2")

my_file_red_aug <- glue::glue("{prefix}data/summer-2022/T29UNA_20220811T115409_B02.jp2")
my_file_green_aug <- glue::glue("{prefix}data/summer-2022/T29UNA_20220811T115409_B03.jp2")
my_file_blue_aug <- glue::glue("{prefix}data/summer-2022/T29UNA_20220811T115409_B04.jp2")
my_file_nir_aug <- glue::glue("{prefix}data/summer-2022/T29UNA_20220811T115409_B08.jp2")

my_file_red_dec <- glue::glue("{prefix}data/december-2022/T29UNA_20221204T115451_B02.jp2")
my_file_green_dec <- glue::glue("{prefix}data/december-2022/T29UNA_20221204T115451_B03.jp2")
my_file_blue_dec <- glue::glue("{prefix}data/december-2022/T29UNA_20221204T115451_B04.jp2")
my_file_nir_dec <- glue::glue("{prefix}data/december-2022/T29UNA_20221204T115451_B08.jp2")

s1 = stack(my_file_red_june,
           my_file_green_june,
           my_file_blue_june,
           my_file_nir_june,
           my_file_red_aug,
           my_file_green_aug,
           my_file_blue_aug,
           my_file_nir_aug,
           my_file_red_dec,
           my_file_green_dec,
           my_file_blue_dec,
           # my_file_swir1,
           # my_file_swir2,
           my_file_nir_dec) 
names(s1) <- c("red_june", "green_june", "blue_june", "nir_june",
              "red_aug", "green_aug", "blue_aug", "nir_aug",
              "red_dec", "green_dec", "blue_dec",
               "nir_dec")


# ref <- st_transform(ref, crs = st_crs(s1))
my_extent <- c(xmin=540000, xmax=585000, ymin=6050000, ymax=6090000)
my_extent <- c(xmin=510000, xmax=610000, ymin=6030000, ymax=6100000)

# ref <- st_transform(ref, crs = st_crs(s1))
ref1 <- ref |> 
  st_crop(extent(my_extent)) |> 
  mutate(
    class = case_when(
      str_detect(DESCRIPT, "[Gg]ranite") ~ "granite",
      str_detect(DESCRIPT, "[Qq]uar[tz][tz]") ~ "quartz",
      str_detect(DESCRIPT, "[Ss]andstone") ~ "sandstone",
      str_detect(DESCRIPT, "[Ss]chist") ~ "schist",
      str_detect(DESCRIPT, "[Ll]imestone") ~ "limestone",
      str_detect(DESCRIPT, "[Ss]hale") ~ "shale",
      TRUE ~ "other"
    ),
    .before = NEWCODE) 
# |> 
#   filter(class != "other")

bedrock_plot <- ref1 |> 
  ggplot(aes(fill = class)) +
  geom_sf(show.legend = TRUE) + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
  scale_fill_okabe_ito() +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.title = element_blank())

z <- ref |> 
  st_drop_geometry() |> 
  dplyr::select(-c(OBJECTID_2, OBJECTID, SHEETNO, STRATCODE, LITHCODE, SHAPE_LEN)) |> 
  head() |> 
  gt::gt()

ref1 <- ref1 |> 
  mutate(
    class = case_when(
      str_detect(DESCRIPT, "[Gg]ranite") ~ "granite",
      str_detect(DESCRIPT, "[Qq]uar[tz][tz]") ~ "quartz",
      str_detect(DESCRIPT, "[Ss]andstone") ~ "sandstone",
      str_detect(DESCRIPT, "[Ss]chist") ~ "schist",
      str_detect(DESCRIPT, "[Ll]imestone") ~ "limestone",
      str_detect(DESCRIPT, "[Ss]hale") ~ "shale",
      TRUE ~ "other"
    )
  ) |> 
  filter(class != "other")
table(ref1$class)
s2 <- s1 |> crop(my_extent)
s2_plot <- ggRGB(s2, r = 4, g = 3, b = 2) + 
  theme_void()

ndvi_aug <- (s2[["nir_aug"]] - s2[["red_aug"]]) / (s2[["nir_aug"]] + s2[["red_aug"]])
ndvi_aug_small <- ndvi_aug |> terra::aggregate(10)
ndvi_aug_small |> 
  as.data.frame(xy = TRUE) |> 
  mutate(layer = ifelse(layer < 0, 0, layer)) |> 
  ggplot(aes(x=x, y=y, fill=layer^1.5)) +
  geom_raster(show.legend = F) +
  scale_fill_gradient(low = "#FFFFFF", high = "darkgreen") +
  theme_void()

pairs(s2, maxpixels = 200)

ref_values <- exact_extract(s2, ref1, fun = "mean") |> as.data.frame()
ref_values$class <- ref1$class #add class attribute to a dataframe
names(ref_values) <- c("red_june", "green_june", "blue_june", "nir_june",
                       "red_aug", "green_aug", "blue_aug", "nir_aug", 
                       "red_dec", "green_dec", "blue_dec", "nir_dec", 
                       "class")
ref_values <- ref_values |> 
  mutate(ndvi_june = (nir_june - red_june)/(nir_june+red_june),
         ndvi_aug = (nir_aug - red_aug)/(nir_aug+red_aug),
         ndvi_dec = (nir_dec - red_dec)/(nir_dec+red_dec))

#some visualization with ggplot2 package - scatterplots:
ggplot(ref_values, aes(ndvi_aug, y = ndvi_dec, color = class))+
  geom_point(size = 1) +
#  scale_y_log10() +
  stat_ellipse(size = 3) +
  theme_clean()

mean_spectra <- group_by(ref_values, class) |> #we group ref_values by class
  summarise_all(mean) |> #calculate mean value for each class
  pivot_longer(-class, names_to = "colour", values_to = "mean") |> #transform the df to "long" format
  mutate(colour = factor(colour, levels = c("red_june", "green_june", "blue_june", "nir_june",
                                            "red_aug", "green_aug", "blue_aug", "nir_aug", 
                                            "red_dec", "green_dec", "blue_dec", "nir_dec")))

#and plot sepctral curves:
ggplot(mean_spectra, aes(colour, mean, color = class, group = class))+
  geom_point()+
  geom_line(linewidth = 1.8, alpha = 0.6) +
  theme_clean()

ref2 <- ref1[st_geometry_type(ref1) == "POLYGON",]
ref2 <- ref2[100:335,]
ref2 |> 
  ggplot(aes(fill = class)) +
  geom_sf(show.legend = T) + 
  theme_void()

Sys.time()
classification_rf = superClass(s2, ref2, set.seed(42), trainPartition = 0.7, responseCol = "class", #random forest classification
                               model = "rf", mode = "classification", tuneLength = 5, kfold = 10)
Sys.time()
classification_rf

plot(classification_rf$map)
Sys.time()
classification_svm = superClass(s2, ref2, set.seed(42), trainPartition = 0.7, responseCol = "class", #random forest classification
                                model = "svmLinear", mode = "classification", tuneLength = 5, kfold = 10)
Sys.time()
classification_svm

varImp_rf = caret::varImp(classification_rf$model)

plot(varImp_rf)
varImp_rf

control = rfeControl(functions=rfFuncs, method="cv", number=10)
results = rfe(ref_values[,1:12], ref_values[,13], sizes=c(1:6), rfeControl=control)

