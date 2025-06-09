###---Libraries---###
library(tidyverse)

###---Input---###
pos_rev <- read_csv('data/positive_reviews.csv')
neg_rev <- read_csv('data/negative_reviews.csv')

###---Transform---###
pos_rev %>% 
  group_by(`Airline Name`) %>% 
  count(sort = TRUE)

neg_rev %>% 
  group_by(`Airline Name`) %>% 
  count(sort = TRUE)
