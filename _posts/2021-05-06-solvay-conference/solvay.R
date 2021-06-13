library(data.table)
library(jsonlite)
library(httr)
library(tidyverse)
library(showtext)

myauth <- readRDS("../myauth_faceplusplus")

facepp <- function(fullpath, auth) {
  ## Initilize Object to store API output for single image
  face <- NULL
  ## create empty table to fill with API output
  faces <- data.table(anger = as.numeric(NA), 
                      disgust = as.numeric(NA),
                      fear = as.numeric(NA), 
                      happiness = as.numeric(NA),
                      neutral = as.numeric(NA), 
                      sadness = as.numeric(NA), 
                      surprise = as.numeric(NA),
                      gender = as.character(NA),
                      top = as.numeric(NA),
                      left = as.numeric(NA),
                      facecount = as.numeric(NA), 
                      fullpath = fullpath)
  
  ## run counts the number of image at testing
  ## go over each fullpath and send to API
  ## write API-output in face
  run <- 0
  for (i in 1:length(fullpath)) {
    run <- run + 1
    cat(run, "\n")
    while(is.null(face)) {
      try(
        face <- as.character(httr::RETRY("POST", "https://api-us.faceplusplus.com/facepp/v3/detect",
                                         body = list(api_key  = auth$api_key,
                                                     api_secret = auth$api_secret,
                                                     image_file = upload_file(fullpath[i]),
                                                     return_landmark = 0,
                                                     return_attributes = "emotion,gender"),
                                         times = 2, 
                                         encode = "multipart")),
        silent = FALSE
      )
    }
    
    ## if face is found, extract information and write into data.table
    facecount <- length(fromJSON(face)$faces$face_token)
    
    if (facecount != 0) {
      emotion <- fromJSON(face)$faces$attributes$emotion
      gender <- fromJSON(face)$faces$attributes$gender
      top <- fromJSON(face)$faces$face_rectangle$top
      left <- fromJSON(face)$faces$face_rectangle$left
      
      ## write info to data.table
      faces[faces$fullpath == fullpath[i],][,1:11] <- c(emotion[1,], gender[1,], top[1], left[1], facecount)
      
      ## if more than one face found in image, make df with all info and merge
      if(facecount > 1) {
        faces <- dplyr::union(x = faces, 
                              y = data.table(anger = emotion[,1], 
                                             disgust = emotion[,2],
                                             fear = emotion[,3], 
                                             happiness = emotion[,4],
                                             neutral = emotion[,5], 
                                             sadness = emotion[,6], 
                                             surprise = emotion[,7],
                                             gender = gender[,1],
                                             top = top,
                                             left = left,
                                             facecount = facecount,
                                             fullpath = fullpath[i])) 
      }
      
      face <- NULL
      Sys.sleep(2)
    } else {
      face <- NULL
      Sys.sleep(2)
    }
  }
  return(faces)
}


mypaths <- list.files(path = "_posts/2021-05-06-solvay-conference/groups/")
mypaths <- glue::glue("_posts/2021-05-06-solvay-conference/groups/{mypaths}")
file.exists(mypaths)

## Call the function
faces <- facepp(fullpath = mypaths, auth = myauth)

faces <- faces %>% 
  arrange(fullpath, left)


scientists <- faces$fullpath %>% 
  str_remove("_posts/2021-05-06-solvay-conference/groups/") %>% 
  str_remove(".png") %>% 
  str_split("-") %>% 
  unique() %>% 
  unlist()

faces$scientist <- scientists

font_add_google("Lemonada", "Lemonada")

showtext_auto()

theme_set(theme_minimal())
theme_update(text = element_text(size = 28, family = "Lemonada"))

my_colours <- paletteer::paletteer_d("yarrr::eternal", n = 7)

p <- faces %>% 
  select(scientist, anger:surprise) %>% 
  pivot_longer(-scientist, names_to = "emotion", values_to = "percentage") %>% 
  ggplot(aes(emotion, percentage %>% gtools::logit(min = -0.0001, max = 100))) +
  geom_boxplot(aes(fill = emotion), show.legend = F) +
  scale_fill_manual(values = my_colours) +
  labs(x = "", y = "Percentage %") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = my_colours)) +
  coord_flip()

dat <- ggplot_build(p)$data[[1]]

p + geom_segment(data=dat, aes(x=xmin, xend=xmax, 
                               y=middle, yend=middle), colour="grey80", size=1)

faces %>% 
  select(scientist, anger:surprise) %>% 
  pivot_longer(-scientist, names_to = "emotion", values_to = "percentage") %>% 
  filter(percentage > 10, emotion != "neutral") %>% 
  View()
