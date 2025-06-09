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

vader_sentiment <- read_csv('data/vader_sent.csv')


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
  mutate(Recommended_2 = case_when(
    Recommended == 'yes' ~ 'positive',
    Recommended == 'no' ~ 'negative'
  ))

accuracy_bing <- reviews %>% 
  filter(sentiment_bing != 'neutral') %>% 
  mutate(sentiment_bing = as.factor(sentiment_bing),
         Recommended_2  = as.factor(Recommended_2))

bing_acc <- accuracy(accuracy_bing,
         Recommended_2, sentiment_bing) %>% 
  pull(.estimate) %>%
  { round(. * 100, 2) }

##--afinn sentiment--##
tokens_afinn <- tokens %>% 
  left_join(get_sentiments('afinn')) %>% 
  mutate(value = replace_na(value, 0))

sentiment_afinn <- tokens_afinn %>% 
  group_by(id) %>% 
  summarise(score = sum(value, na.rm = TRUE)) %>% 
  ungroup %>% 
  mutate(sentiment_afinn = case_when(
    score > 0 ~ 'positive',
    score < 0 ~ 'negative',
    .default = 'neutral'
  )) %>% 
  select(!score)

reviews <- reviews %>% 
  left_join(sentiment_afinn, by = 'id')

accuracy_afinn <- reviews %>% 
  filter(sentiment_afinn != 'neutral') %>% 
  mutate(sentiment_afinn = as.factor(sentiment_afinn),
         Recommended_2  = as.factor(Recommended_2))

afinn_acc <- accuracy(accuracy_afinn,
         Recommended_2, sentiment_afinn) %>% 
  pull(.estimate) %>%
  { round(. * 100, 2) }


##--VADER sentiment--##

vader_sent2 <- vader_sentiment %>% 
  rowid_to_column("id") %>% 
  mutate(sentiment_vader = case_when(
    compound > 0.05 ~ 'positive',
    compound < -0.05 ~ 'negative',
    .default = 'neutral'
  )) %>% 
  select(id, sentiment_vader) %>% 
  mutate(
    id = as.numeric(id),
    sentiment_vader = as.factor(sentiment_vader))

reviews <- reviews %>% 
  left_join(vader_sent2, by = 'id')

accuracy_vader <- reviews %>% 
  select(id, Recommended_2, sentiment_vader) %>% 
  filter(sentiment_vader %in% c('positive', 'negative'),
         Recommended_2 %in% c('positive', 'negative')) %>%
  mutate(
    sentiment_vader = factor(sentiment_vader, levels = c('positive', 'negative')),
    Recommended_2 = factor(Recommended_2, levels = c('positive', 'negative'))
  )

vader_acc <- accuracy(accuracy_vader,
                      Recommended_2, sentiment_vader) %>%
  pull(.estimate) %>%
  { round(. * 100, 2) }

positive_reviews <- reviews %>% 
  filter(sentiment_vader == 'positive')

negative_reviews <- reviews %>% 
  filter(sentiment_vader == 'negative')

###---Output---###
write_csv(positive_reviews, 'data/positive_reviews.csv')
write_csv(negative_reviews, 'data/negative_reviews.csv')