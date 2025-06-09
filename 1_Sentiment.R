###---Libraries---###
library(tidyverse)
library(vader)
library(yardstick)
library(textdata)

###---Input---###
tokens <- read_csv('data/tokenized_reviews.csv')
data_raw <- read_csv('data/Airline_review.csv') %>% 
  rowid_to_column('id')

reviews <- data_raw %>% 
  select(id, `Airline Name`, Overall_Rating, Review_Title, `Review Date`,
         Review, Verified, `Type Of Traveller`, Recommended)

###---Transform---###

##--Bing sentiment--##
tokens_bing <- tokens %>% 
  left_join(get_sentiments('bing')) %>% 
  mutate(sentiment = replace_na(sentiment, 'neutral'))

sentiment_bing <- tokens_bing %>% 
  group_by(id) %>% 
  count(sentiment) %>% 
  ungroup() %>% 
  pivot_wider( # you will normally Google this
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) %>% 
  mutate(
    sentiment_bing = case_when(
      positive > negative ~ "positive",
      negative > positive ~ "negative",
      .default = "neutral"
    )
  ) %>% 
  select(id, sentiment_bing)

reviews <- reviews %>% 
  left_join(sentiment_bing, by = 'id')

reviews <- reviews %>% 
  mutate(Recommended = case_when(
    Recommended == 'yes' ~ 'positive',
    Recommended == 'no' ~ 'negative'
  ))



accuracy_bing <- reviews %>% 
  filter(sentiment_bing != 'neutral') %>% 
  mutate(sentiment_bing = as.factor(sentiment_bing),
         Recommended  = as.factor(Recommended))

accuracy(accuracy_bing,
         Recommended, sentiment_bing) # 83% accuracy compared to recommended


##--anfinn sentiment--##


