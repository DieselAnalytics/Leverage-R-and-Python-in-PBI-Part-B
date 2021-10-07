library(tidyverse)
library(stringr)
library(lubridate)

dfTweets <- dataset

# Regular expression to identify the string patterns that needs to be remvoed
clean_tweet <- "(RT:\\s\\(\\w+\\))|(RT(\\w|\\s|@)*:)|(https:(/|\\w|\\.)*\\s)|(https:(/|\\w|\\.)*$)"

# Regular expression to identify string patterns for hash tags in tweets
hashtag <- "(#\\w+\\s)|(#\\w+$)"

# dplyr workflow to clean the tweets
dfTweets_Cleaned <- 
  dfTweets %>%
  transmute(
    id,
    Date,
    Location,
    `Twitter Handle`,
    retweetcount,
    `Old Tweet` = Tweet,
    # Creates a new column that removes substrings in the "Tweet"
    # column that matches the regular expression identified in 
    # the clean_tweet variable.
    `Cleaned Tweet` = str_trim(str_remove_all(Tweet, clean_tweet))
  ) %>%
  # The rowise() verb is used becausue the str_extract_all is not
  # a vectorized function
  rowwise() %>%
  mutate(
    # Creates a column that contains all of the hashtags contained 
    # in the tweet
    hashtags = 
      paste(
        str_extract_all(`Cleaned Tweet`, hashtag, simplify = TRUE),
        collapse = " "
      )
  )
