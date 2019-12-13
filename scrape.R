library(readxl)
library(httr)
library(rjson)
##########
# HELPER #
##########

auth.file <- './auth.json'  # Define path for file containing API keys
auth.data <- fromJSON(file=auth.file)  # Read file containing API keys
req <- POST('https://api.twitter.com/oauth2/token', 
            authenticate(auth.data$api_key, auth.data$api_secret_key),
            body=list('grant_type'='client_credentials'))
auth.bearer_token <- content(req)$access_token


# query for twitter ids 
get_id <- function(s) {
  query_string <- paste('https://api.twitter.com/1.1/users/lookup.json?screen_name=',
                        s, 
                        sep='')
  req <- GET(query_string, add_headers(Authorization = paste('Bearer', auth.bearer_token, sep=' ')))
  if (!is.null(content(req)[[1]]$id_str) && status_code(req)==200){
    return(content(req)[[1]]$id_str)
  }
  else if (status_code(req)!=200){
    stop(paste("Times Exceded at index:",i,"\nWait."))
  }
  else{
    return(0)
  }
}


#################################################################################################

##################
# IMPORT DATASET #
##################
Rankings <- read_excel("Dataset/Rankings.xls")
str(Rankings)

# drop useless columns
drops <- c("Flag","League")
Rankings <- Rankings[ , !(names(Rankings) %in% drops)]

# keep only first 200 universities
Rankings <- Rankings[c(1:200),]

# convert rank to numeric
Rankings[, 1] <- sapply(Rankings[, 1], as.numeric)

# adding empty columns for twitter name, id and the list of friends
Rankings$Twitter_Name <-  rep(0, 200)
Rankings$Twitter_ID <-  rep(0, 200)
Rankings$Friends <-  rep(0, 200)

# adding twitter names
screennames <- read.csv("Dataset/screennames.txt", sep="", stringsAsFactors=FALSE, header=FALSE)
Rankings$Twitter_Name <- screennames$V1

#################################################################################################

########################
# QUERYING FOR THE IDS #
########################

for (i in 1:200){
  Rankings$Twitter_ID[i] <- get_id(Rankings$Twitter_Name[i])
}


#################### WILL BE USELESS AFTER FILLING THE REMAINING COLUMN ########################
write.csv(Rankings,"Dataset/Rankings_cleaned.csv", row.names=FALSE)

Rankings <- read.csv("Dataset/Rankings_cleaned.csv",
              colClasses = c("numeric", "character", "numeric", "character", "character", "character", "character") )

################################################################################################

########################
# QUERYING FOR FRIENDS #
########################


