# Dependencies
library(tidyverse)
library(readxl)


# Read rankings
rankings <- read_xls('data/rankings.xls')
rankings

# Clean dataset
rankings <- rankings %>% 
  rename(rank=Rank, university=University, score=Score, country=Country, flag=Flag, league=League) %>%  # Reset names
  select(-flag) %>%  # Cut useless columns
  slice(0:200) %>%  # Get only first 200 rows
  mutate(country=factor(country), league=factor(league))  # Factorize
rankings


# Read twitter screen names
snames <- read_delim('data/screen_names.txt', delim=' ', col_names=c('twitter'), col_types=cols(twitter=col_character()))
snames

# Add twitter screen names to rankings table
rankings <- rankings %>%
  add_column(twitter = snames$twitter)
rankings


# Save data to disk
rankings %>% write_csv('data/rankings_clean.csv')
