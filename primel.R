library(tidyverse)
library(matlab)

numbers <- tibble(x = 10000:99999) %>% 
  filter(matlab::isprime(x) == TRUE)

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

begin <- 12005
end <- 13000
my_step <- 1

numbers <- seq(begin, end, by = my_step)

numbers[isprime(numbers)==1]
