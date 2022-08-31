library(tidyverse)
library(collapsibleTree)
library(ABAData)
library(ABAEnrichment)

# data("dataset_adult")
# z <- dataset_adult |> pull(structure) |> table() |> names()
z <- readRDS("_posts/2022-08-14-brain-areas/data/ABA-structures")
z1 <- get_name(z)
z2 <- map(z, get_superstructures)
n <- max(lengths(z2))
z3 <- lapply(z2, `length<-`, n)
z3 <- as.data.frame(z3)
z3 <- t(z3) |> as.data.frame()
rownames(z3) <- NULL
brain_names <- function(x) ifelse(!is.na(x), get_name(x), NA)
brain_names_list <- function(x) map(z3[, x], brain_names) |> 
  unlist() |> 
  str_replace(", Left", " (L)") |> 
  str_replace(", left", " (L)") |> 
  str_replace(", Right", " (R)") |> 
  str_replace(", right", " (R)") |> 
  str_remove("^.*?_")
brain <- data_frame(level_2 = brain_names_list(2),
                 level_3 = brain_names_list(3),
                 level_4 = brain_names_list(4),
                 level_5 = brain_names_list(5),
                 level_6 = brain_names_list(6),
                 level_7 = brain_names_list(7),
                 level_8 = brain_names_list(8),
                 level_9 = brain_names_list(9))
collapsibleTree(brain, hierarchy = names(brain), width = 1000)


z3 <- unlist(z2)
z4 <- map(1:length(z2), function(x) rep(z[x], length(z2[[x]])))
z5 <- unlist(z4)
z6 <- tibble(structure = z3, substructure = z5)

