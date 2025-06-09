###---Libraries---###
library(tidyverse)
library(vader)
library(yardstick)
library(textdata)

###---Input---###
# load processed tokens
tokens <- read_csv('data/tokenized_reviews.csv')

# load raw data
data_raw <- read_csv('data/Airline_review.csv') %>% 
  rowid_to_column('id')

# select only important columns
reviews <- data_raw %>% 
  select(id, `Airline Name`, Overall_Rating, Review_Title, `Review Date`,
         Review, Verified, `Type Of Traveller`, Recommended)

# load vader sentiment (due to process time of over 2 hours)
vader_sentiment <- read_csv('data/vader_sent.csv')


###---Transform---###

##--Bing sentiment--##
# assign sentiment to tokens
tokens_bing <- tokens %>% 
  left_join(get_sentiments('bing')) %>% 
  mutate(sentiment = replace_na(sentiment, 'neutral'))

# label full comments as positive, neutral, or negative
sentiment_bing <- tokens_bing %>% 
  group_by(id) %>% 
  count(sentiment) %>% 
  ungroup() %>% 
  pivot_wider( 
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

# Merge to reviews dataframe
reviews <- reviews %>% 
  left_join(sentiment_bing, by = 'id')

# Create new column allowing for comparison with the recommended variable
reviews <- reviews %>% 
  mutate(Recommended_2 = case_when(
    Recommended == 'yes' ~ 'positive',
    Recommended == 'no' ~ 'negative'
  ))

# Create df for accuracy measurement
accuracy_bing <- reviews %>% 
  filter(sentiment_bing != 'neutral') %>% 
  mutate(sentiment_bing = as.factor(sentiment_bing),
         Recommended_2  = as.factor(Recommended_2))

# asign variable with bing accuracy as percentage
bing_acc <- accuracy(accuracy_bing,
         Recommended_2, sentiment_bing) %>% 
  pull(.estimate) %>%
  { round(. * 100, 2) }

##--afinn sentiment--##
# Assing value to each token
tokens_afinn <- tokens %>% 
  left_join(get_sentiments('afinn')) %>% 
  mutate(value = replace_na(value, 0))

# Transform back to a positive, neutral, or negative label for each full comment
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

# Merge to the reviews dataframe
reviews <- reviews %>% 
  left_join(sentiment_afinn, by = 'id')

# Create df allowing to measure accuracy fo the afinn lexicon
accuracy_afinn <- reviews %>% 
  filter(sentiment_afinn != 'neutral') %>% 
  mutate(sentiment_afinn = as.factor(sentiment_afinn),
         Recommended_2  = as.factor(Recommended_2))

# Pull percentage accuracy compared to recommended variable
afinn_acc <- accuracy(accuracy_afinn,
         Recommended_2, sentiment_afinn) %>% 
  pull(.estimate) %>%
  { round(. * 100, 2) }


##--VADER sentiment--##
# Transfrom vader sentiment score to a lable
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

# Merge to reviews dataframe
reviews <- reviews %>% 
  left_join(vader_sent2, by = 'id')

# Transform to allow accuracy measurement
accuracy_vader <- reviews %>% 
  select(id, Recommended_2, sentiment_vader) %>% 
  filter(sentiment_vader %in% c('positive', 'negative'),
         Recommended_2 %in% c('positive', 'negative')) %>%
  mutate(
    sentiment_vader = factor(sentiment_vader, levels = c('positive', 'negative')),
    Recommended_2 = factor(Recommended_2, levels = c('positive', 'negative'))
  )

# Pull percentage accuracy compared to recommended variable
vader_acc <- accuracy(accuracy_vader,
                      Recommended_2, sentiment_vader) %>%
  pull(.estimate) %>%
  { round(. * 100, 2) }

##--Create new dataframes--##
# create df with only positive reviews (vader)
positive_reviews <- reviews %>% 
  filter(sentiment_vader == 'positive')

# create df with only negative reviews (vader)
negative_reviews <- reviews %>% 
  filter(sentiment_vader == 'negative')

###---Output---###
write_csv(positive_reviews, 'data/positive_reviews.csv')
write_csv(negative_reviews, 'data/negative_reviews.csv')