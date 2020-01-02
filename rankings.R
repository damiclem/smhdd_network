# Dependencies
library(tidyverse)
library(readxl)


# Read rankings
rankings <- read_xls('data/rankings.xls')
rankings

# Clean dataset
rankings <- rankings %>% 
  rename(rank=Rank, university=University, score=Score, country=Country) %>%  # Reset names
  select(-c(Flag, League)) %>%  # Cut useless columns
  slice(0:225) %>%  # Get only first 225 rows
  mutate(country=factor(country))  # Factorize
rankings


# Read twitter screen names
snames <- read_delim('data/screen_names.txt', delim=' ', col_names=c('twitter_name'), col_types=cols(twitter_name=col_character()))
snames

# Add twitter screen names to rankings table
rankings <- rankings %>%
  add_column(twitter_name = snames$twitter_name)
rankings

# Filling missing Univerisities
rankings[rankings$twitter_name == "None",]$twitter_name = NA

# Save data to disk
rankings %>% write_csv('data/rankings_clean.csv')
