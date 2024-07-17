library(tidyverse)
library(matlab)

numbers <- tibble(x = 10000:99999) %>% 
  filter(matlab::isprime(x) == TRUE)
numbers <- numbers %>% 
  mutate(x_split = str_split(x, ""))

indices_no_repeats <- map(numbers$x_split, function(x){table(x) %>% max() == 1}) %>% unlist()
numbers <- numbers[indices_no_repeats,]

begin <- 36001
end <- 36100
my_step <- 10

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]

x <- 10000:99999
x <- x[isprime(x) == 1]
x <- str_split(x, "")
x1 <- map(x, function(x){table(x) %>% max() == 1}) %>% unlist()
x <- x[x1]
x1 <- map(1:length(x), function(n){c("0", "2", "4", "6", "8") %in% x[[n]] %>% sum() == 0}) %>% 
  unlist()
x <- x[x1]
z <- map(1:length(x), function(n){str_flatten(x[[n]])}) %>% 
  unlist() %>% 
  as.numeric()

begin <- 19207
end <- 99999
my_step <- 10000

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1 & between(numbers %% 1000, 200, 300)]
