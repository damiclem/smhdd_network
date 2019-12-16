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

# # Get univeristies with twitter ids
# rankings <- read_csv('data/rankings_clean.csv')
# 
# 
# # Define a table for twitter accounts
# users <- tibble(
#   id=character(),
#   screen_name=character(),
#   location=character(),
#   url=character(),
#   description=character(),
#   protected=logical(),
#   verified=logical(),
#   followers_count=numeric(),
#   friends_count=numeric()
# )
# 
# # Retrieve users info from their screen name through the Twitter APIs
# # Only 100 users can be retrieved at each API call
# # A maximum number of 300 users can be retrieved in a 15 minutes window
# users.ids <- rankings %>%
#   select('twitter') %>%  # Get user ids
#   pull()  # Turn user ids into plain array
# # Define user batches of size 100
# users.batches <- split(users.ids, ceiling(seq_along(users.ids)/100))
# # Loop through each 100 users batch
# for (i in 1:length(users.batches)) {
#   # Make API request
#   req <- GET(
#     'https://api.twitter.com/1.1/users/lookup.json',
#     add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')),
#     query=list(
#       screen_name = paste(users.batches[[i]], collapse=','),
#       include_entities=0,
#       tweet_mode=0
#     ))
#   # Get results
#   res.status <- status_code(req)
#   res.data <- content(req)
#   # Check status: if 404 authenticate again
#   
#   # Loop through each retrieved user
#   for (j in 1:length(res.data)) {
#     # Get current user data
#     curr.user <- res.data[[j]]
#     # Add user data to table
#     users <- users %>% add_row(
#       id=as.character(curr.user$id_str),
#       screen_name=curr.user$screen_name,
#       location=curr.user$location,
#       # url=curr.user$url,
#       description=curr.user$description,
#       protected=as.logical(curr.user$protected),
#       verified=as.logical(curr.user$verified),
#       followers_count=as.numeric(curr.user$followers_count),
#       friends_count=as.numeric(curr.user$friends_count)
#     )
#   }
# }
# 
# # Save users table to disk
# users %>% write_csv('data/users.csv')


# Query twitter APIs for retrieving friends of each user
# First cursor is -1, each cursor identifies a page of results
# Last cursor for each user is 0, i.e. zero value means last page
# 5000 is the maximum number of rows per page
# A maximum number of 15 requests can be sent in a 15 min window, hence one per minute

# Retrieve users table
users <- read_csv('data/users.csv', col_types=cols(
    id=col_character(),
    screen_name=col_character(),
    location=col_character(),
    # url=col_character(),
    description=col_character(),
    protected=col_logical(),
    verified=col_logical(),
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
  pull()
# Index of the current user
curr.index <- 1
# Index of the current cursor, for the current user (-1 means first page)
curr.cursor <- -1
# Define a continuous loop: break condition is checked inside
while(TRUE) {
  # Define current user id
  user.id <- users.ids[curr.index]
  # Make request to Twitter friends API
  req <- GET(
    'https://api.twitter.com/1.1/friends/ids.json',
    add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')),
    query=list(
      user_id = user.id,
      stringify_ids=1,
      cursor = -1,
      count = 5000
    ))
  # Retrieve response data
  res.status <- status_code(req)
  res.data <- content(req)
  # Debug
  print(res.status)
  print(res.data)
  # Check status 200: request successfull
  if (res.status == 200) {
    # Debug
    print(curr.index)
    print(curr.cursor)
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
    # Debug
    print(summary(friendship))
    # Get next cursor
    next.cursor <- res.data$next_cursor
  }
  # Check 404: not found
  else if (res.status == 404) {
    # Skip current user
    next.cursor <- 0
  }
  # Case request limit exceeded
  else {
    # Wait for a quarter hour
    Sys.sleep(60)
    # Execute the current step again
    next
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
  Sys.sleep(60)
}

# Save friendship table to disk
friendship %>% write_csv('data/friendship.csv')
