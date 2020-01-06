library(geonames)

# Retrieve data
users <- read.csv("data/users.csv", stringsAsFactors=FALSE)

# Create dataframe with Universities, IDs, Locations and Countries
positions <- data.frame(university=users[,"university"])
positions$id <- users$id
positions$country <- users$country
positions$location <- users$location

# Keeping only City
positions$location <- sub(',.*$','', positions$location)

# Manually curating
####################
positions[22,"location"] <-"Pisa"
positions[27,"location"] <-"Copenhagen"
positions[33,"location"] <-"Pohang"
positions[47,"location"] <-"Bern"
positions[61,"location"] <-"Southampton"
positions[63,"location"] <-"Vancouver"
positions[66,"location"] <-"Paris"
positions[67,"location"] <-"Munich"
positions[72,"location"] <-"Geneva"
positions[79,"location"] <-"Shangai"
positions[86,"location"] <-"Hefei"
positions[97,"location"] <-"Lyon"
positions[108,"location"] <-"Paris"
positions[112,"location"] <-"São Paulo"
positions[116,"location"] <-"Hong Kong"
positions[121,"location"] <-"Groningen"
positions[132,"location"] <-"Durham"
positions[134,"location"] <-"Barcelona"
positions[135,"location"] <-"Karlsruhe"
positions[144,"location"] <-"Yokohama"
positions[149,"location"] <-"New York"
positions[156,"location"] <-"Kuala Lumpur"
positions[157,"location"] <-"Moscow"
positions[160,"location"] <-"Lawrence"
positions[167,"location"] <-"Leicester"
positions[175,"location"] <-"Hanyang "
positions[176,"location"] <-"State College"
positions[178,"location"] <-"Tomsk"
positions[184,"location"] <-"Nuremberg"
positions[190,"location"] <-"Espoo"
positions[215,"location"] <-"Dublin"
positions[216,"location"] <-"Moscow"
positions[219,"location"] <-"Guildford"
positions[220,"location"] <-"Douglas"
positions[222,"location"] <-"Halifax"
positions[223,"location"] <-"Lincoln"
positions[224,"location"] <-"Kuala Lumpur"


positions$country[positions$country=="USA"] <- "US"
positions$country[positions$country=="Italy"] <- "IT"
positions$country[positions$country=="Switzerland"] <- "CH"
positions$country[positions$country=="Sweden"] <- "SE"
positions$country[positions$country=="Germany"] <- "DE"
positions$country[positions$country=="Republic of Korea"] <- "KR"
positions$country[positions$country=="Denmark"] <- "DK"
positions$country[positions$country=="China"] <- "CN"
positions$country[positions$country=="Canada"] <- "CA"
positions$country[positions$country=="Netherlands"] <- "NL"
positions$country[positions$country=="Japan"] <- "JP"
positions$country[positions$country=="Australia"] <- "AU"
positions$country[positions$country=="Finland"] <- "FI"
positions$country[positions$country=="Singapore"] <- "SG"
positions$country[positions$country=="France"] <- "FR"
positions$country[positions$country=="India"] <- "IN"
positions$country[positions$country=="Norway"] <- "NO"
positions$country[positions$country=="Austria"] <- "AT"
positions$country[positions$country=="Brazil"] <- "BR"
positions$country[positions$country=="Russia"] <- "RU"
positions$country[positions$country=="Hong Kong"] <- "HK"
positions$country[positions$country=="Israel"] <- "IL"
positions$country[positions$country=="Taiwan"] <- "TW"
positions$country[positions$country=="Spain"] <- "ES"
positions$country[positions$country=="Slovenia"] <- "SI"
positions$country[positions$country=="Ireland"] <- "IE"
positions$country[positions$country=="South Africa"] <- "ZA"
positions$country[positions$country=="Belgium"] <- "BE"
positions$country[positions$country=="Malaysia"] <- "MY"
positions$country[positions$country=="Iran"] <- "IR"
####################

# Adding colums for latitude and longitude
positions$lat <- rep(0,225)
positions$lng <- rep(0,225)

# Fill lat and lng
options(geonamesUsername = "scarpa")
for (i in 1:225){
  print(i)
  if (!is.na(positions[i,"id"])){
    temp <-GNsearch(name_equals = positions[i,"location"], country=positions[i, "country"])
    positions[i,"lat"] <- temp[1,"lat"]
    positions[i,"lng"] <- temp[1,"lng"]
  }
}

# Add colors
pal <- colorRampPalette(c("red", "blue"))
positions$color <- pal(225)

#Saving
positions %>% write_csv('data/positions.csv')