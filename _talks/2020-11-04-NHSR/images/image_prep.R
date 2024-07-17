library(tidyverse)
library(magick)

image_read("images/adelie-Leksele.jpg") %>% 
  image_crop("370x440+30+75") %>% 
  image_scale(250) %>% 
  image_border("black", "5x5") %>% 
  image_write("images/adelie-Leksele_big.jpg")

image_read("images/gentoo-Vittaya_Pinpan.jpg") %>% 
  image_crop("380x444+135") %>% 
  image_scale(250) %>% 
  image_border("blue", "5x5") %>% 
    image_write("images/gentoo-Vittaya_Pinpan_big.jpg")

image_read("images/chinstrap-Alexey_Seafarer.jpg") %>% 
  image_crop("1000x1250+0+180") %>% 
  image_scale(250) %>% 
  image_border("grey", "5x5") %>% 
  image_write("images/chinstrap-Alexey_Seafarer_big.jpg")
