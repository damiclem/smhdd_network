# Dependencies
library(tidyverse)
library(httr)
library(rjson)


# Execute an authentication request to Twitter APIs
# Returns return an authentication token if authentication was successfull
# Otherwise, if authentication went wrong, return NULL 
req.auth <- function(api_key, api_secret_key) {
  # Request API access token
  req <- POST('https://api.twitter.com/oauth2/token', 
              authenticate(api_key, api_secret_key),
              body=list('grant_type'='client_credentials'))
  # Read response content
  res.status <- status_code(req)
  res.data <- content(req)
  # Case authentication was successfull
  if (res.status == 200) {
    return(res.data$access_token)
  }
  # Case authentication went wrong
  else {
    return(NULL)
  }
}


# Authentication
# Read credentials
auth.file <- './auth.json'  # Define path for file containing API keys
auth.data <- fromJSON(file=auth.file)  # Read file containing API keys
# Save API credentials
auth.api_key <- auth.data$api_key
auth.api_secret_key <- auth.data$api_secret_key
# Save bearer token (needed to access APIs in the subsequent calls)
auth.bearer_token <- req.auth(auth.api_key, auth.api_secret_key)


# Query twitter APIs for retrieving friends of each user
# First cursor is -1, each cursor identifies a page of results
# Last cursor for each user is 0, i.e. zero value means last page
# 5000 is the maximum number of rows per page
# A maximum number of 15 requests can be sent in a 15 min window, hence one per minute

# Retrieve users table
users <- read_csv('data/users.csv', col_types=cols(
    rank=col_number(),
    university=col_character(),
    score=col_number(),
    country=col_character(),
    location=col_character(),
    twitter_name=col_character(),
    id=col_character(),
    language=col_character(),
    verified=col_logical(),
    n_posts=col_number(),
    followers_count=col_number(),
    friends_count=col_number()
))

# Define a table where to store friendhip relationship
friendship <- tibble(
  from=character(),
  to=character()
)

# Retireve users ids
users.ids <- users %>%
  select('id') %>%
  pull() %>%
  as.character()
# Index of the current user
curr.index <- 1
# Index of the current cursor, for the current user (-1 means first page)
curr.cursor <- -1
# Define a continuous loop: break condition is checked inside
while(TRUE) {
  # Define current user id
  user.id <- users.ids[curr.index]
  # user.id <- '18918483'
  # curr.cursor <- -1
  # Make request to Twitter friends API
  req <- GET(
    'https://api.twitter.com/1.1/friends/ids.json',
    add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')),
    query=list(
      user_id = user.id,
      stringify_ids=1,
      cursor = curr.cursor,
      count = 5000
    ))
  # Retrieve response data
  res.status <- status_code(req)
  res.data <- content(req)
  
  # Check status 200: request successfull
  if (res.status == 200) {
    # Handle empty array
    if (!is.null(unlist(res.data$ids))) {
      # Get friends ids
      friends.ids <- unlist(res.data$ids)
      # Update friendship table
      for (i in 1:length(friends.ids)) {
        # Define current friend id
        friend.id <- friends.ids[i]
        # Update table
        friendship <- friendship %>%
          add_row(from=user.id, to=friend.id)
      }
    }
    # Get next cursor
    next.cursor <- res.data$next_cursor
    # Debug
    print(summary(friendship))
  }
  # Check 404: not found
  else if (res.status == 404) {
    # Skip current user
    next.cursor <- 0
  }
  # Case request limit exceeded
  else if (res.status == 429) {
    # Sleep for two minutes
    Sys.sleep(120)
    # Do current iteration again
    next
  }
  # Case unknown error
  else {
    # Report unknown error
    print('Exiting due to unknown error!')
    print(res.status)
    print(res.data)
    break
  }
  
  # Loop exit condition: last cursor of last user
  if(curr.index >= length(users.ids) && next.cursor == 0) {
    break  # Exit the loop
  }
  # Case final page for current user
  else if(next.cursor == 0) {
    # Reset cursor
    curr.cursor <- -1
    # Go to next user id
    curr.index <- curr.index + 1
  }
  # Next cursor for same user condition
  else {
    # Update cursor
    curr.cursor <- next.cursor
  }
  
  # Wait for next request (one minute waiting time)
  print(curr.index)
  Sys.sleep(60)
}

# Save friendship table to disk
friendship %>% write_csv('data/friendship.csv')

