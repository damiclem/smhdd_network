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

# Get univeristies with twitter ids
universities <- read.csv("data/rankings_clean.csv", stringsAsFactors=FALSE)


# Define a table for twitter accounts
users <- tibble(
  id=character(),
  screen_name=character(),
  location=character(),
  description=character(),
  verified=logical(),
  followers_count=numeric(),
  friends_count=numeric(),
  n_posts=numeric(),
  language=character()
)

# Retrieve users info from their screen name through the Twitter APIs
# Only 100 users can be retrieved at each API call
# A maximum number of 300 users can be retrieved in a 15 minutes window
users.ids <- universities %>%
  select(twitter_name) %>%  # Get user ids
  pull()  # Turn user ids into plain array
# Define user batches of size 100
users.batches <- split(users.ids, ceiling(seq_along(users.ids)/100))
# Loop through each 100 users batch
for (i in 1:length(users.batches)) {
  # Make API request
  req <- GET(
    'https://api.twitter.com/1.1/users/lookup.json',
    add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')),
    query=list(
      screen_name = paste(users.batches[[i]], collapse=','),
      include_entities=0,
      tweet_mode=0
    ))
  # Get results
  res.status <- status_code(req)
  res.data <- content(req)
  # Check status: if 404 authenticate again
  
  # Loop through each retrieved user
  for (j in 1:length(res.data)) {
    # Get current user data
    curr.user <- res.data[[j]]
    # Add user data to table
    users <- users %>% add_row(
      id=as.character(curr.user$id_str),
      screen_name=curr.user$screen_name,
      location=curr.user$location,
      description=curr.user$description,
      verified=as.logical(curr.user$verified),
      followers_count=as.numeric(curr.user$followers_count),
      friends_count=as.numeric(curr.user$friends_count),
      n_posts=as.numeric(curr.user$statuses_count),
      language=curr.user$status$lang
    )
  }
}


# Merge Users and Rankings
users <-merge(x=universities,y=users, by.x="twitter_name", by.y="screen_name", all.x = TRUE)

# Reorder columns
users <- users[c("rank","university","score","country","location",
                 "twitter_name","id","language","verified","description",
                 "n_posts","followers_count","friends_count")]

users <- users[order(users$rank),]


# Filling missing location 
# a contains universities with missing location
a<- users[users$location=="",]
a

# filling missing location
users[users$rank==7,"location"] <- "Zürich, Switzerland"
users[users$rank==42,"location"] <- "Tokyo"
users[users$rank==55,"location"] <- "Munich, Germany"
users[users$rank==76,"location"] <- "Hangzhou, China"
users[users$rank==134,"location"] <- "Barcelona"
users[users$rank==137,"location"] <- "Nagoya, Japan"


# Save users table to disk
users %>% write_csv('data/users.csv')
