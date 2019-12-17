library(tidyverse)

# Retrieve users table
users <- read_csv('data/users.csv', col_types='cccccllnn')
# Retrieve friendship relationship table
friendship <- read_csv('data/friendship.csv', col_types='cc')

# Add 'from' user info
friendship <- users %>% 
  select(id, screen_name) %>%
  inner_join(friendship, by=c('id' = 'from'))

# Filter friendship by known users
friendship <- friendship %>%
  inner_join(
      users %>% select(id, screen_name),
  by=c('to' = 'id'),
  suffix=c('.from', '.to')) %>%
  rename(id.from = id, id.to = to)

# Get every friendship betweem users
View(friendship)

# Get every university which has at least one friend in users
View(friendship %>% distinct(from))
