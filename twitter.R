# Twitter requests handling


# Dependencies
library(httr)
library(rjson)


# Authentication
# Read credentials
auth.file <- './auth.json'  # Define path for file containing API keys
auth.data <- fromJSON(file=auth.file)  # Read file containing API keys
auth.data
# Save API credentials
auth.api_key <- auth.data$api_key
auth.api_secret_key <- auth.data$api_secret_key
# Request API access token
req <- POST('https://api.twitter.com/oauth2/token', 
         authenticate(auth.data$api_key, auth.data$api_secret_key),
         body=list('grant_type'='client_credentials'))
res.code <- status_code(req)
res.code
res.data <- content(req)
res.data
# Save bearer token (needed to access APIs in the subsequent calls)
auth.bearer_token <- res.data$access_token
auth.bearer_token




# Get user info from API
req <- GET('https://api.twitter.com/1.1/users/lookup.json?screen_name=TUeindhoven',
           add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')))
# Get data from response
res.code <- status_code(req)
res.code
res.data <- content(req)
res.data



# Get friends from API
req <- GET('https://api.twitter.com/1.1/friends/list.json?cursor=-1&screen_name=TUeindhoven&skip_status=true&include_user_entities=false&count=200',
           add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')))

for (i in 1:200){
  print(content(req)$users[[i]]$id_str)
}
# come si usano i cursori?
