###---Libraries---###
library(tidytext)
library(tidyverse)
library(tibble)
library(textstem)

###---Input---###
data_raw <- read_csv('data/Airline_review.csv') %>% 
  rowid_to_column('id')

###---Transform---###
reviews <- data_raw %>% 
  select(id, `Airline Name`, Overall_Rating, Review_Title, Review, Recommended)

# tokenize reviews
tokens <- reviews %>% 
  unnest_tokens(word, Review) # create row for each individual word

# Check for most common words
common_words <- tokens %>% 
  group_by(word) %>% 
  count(sort = TRUE)

# remove stopwords and digits
tokens_no_stop <- tokens %>% 
  anti_join(stop_words) %>% # remove stopwords
  filter(!str_detect(word, "[[:digit:]]+")) # remove digits
  
# Check again for most common words
common_words_no_stop <- tokens_no_stop %>% 
  group_by(word) %>% 
  count(sort = TRUE)

# Create tibble with customs topwords
custom_stopwords <- tibble( # Create custom stopwords that have no real meaning
  word = c('flight',
           'airline',
           'airport',
           'flights',
           'plane',
           'planes',
           'airlines'
           ),
  lexicon = 'airline'
)

# Remove custom stopwords
tokens_no_stop <- tokens_no_stop %>% 
  anti_join(custom_stopwords)

# Final check on most common words
common_words_no_stop2 <- tokens_no_stop %>% 
  group_by(word) %>% 
  count(sort = TRUE)

# Lemmatize and stem words
lemmatized_tokens <- tokens_no_stop %>% 
  mutate(word_stem = stem_words(word), # create stemmed column
         word_lemma = lemmatize_words(word)) # create lemmatized column


###---Output---###
write_csv(lemmatized_tokens, 'data/tokenized_reviews.csv')
