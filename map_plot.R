library(tidyverse)
library(data.table)
library(purrr)
library(geonames)
library(gridExtra)
library(igraph)

# Filling missing location (now useless, already filled)
##############################
users <- read.csv("data/users.csv", stringsAsFactors=FALSE)

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

# saving results
users %>% write_csv('data/users.csv')
##############################


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




##############
# EDGES
##############
# Get files
friendship <- read.csv("data/friendship.csv", stringsAsFactors=FALSE)
users <- read.csv('data/users.csv', stringsAsFactors=FALSE) 
positions <- read.csv("data/positions.csv", stringsAsFactors=FALSE)

# removing NAs from positions
positions <- positions[!is.na(positions$id),]

# Add 'from' user info
friendship <- users %>% 
  select(id) %>%
  inner_join(friendship, by=c('id' = 'from'))

# Filter friendship by known users
friendship <- friendship %>%
  inner_join(
    users %>% select(id),
    by=c('to' = 'id'),
    suffix=c('.from', '.to')) %>%
  rename(id.from = id, id.to = to)

friendship <- friendship %>% distinct()


# creating graph object
g <- graph_from_data_frame(friendship, directed = TRUE, vertices = positions$id)

# coverting id to char
positions$id <- as.numeric(positions$id)

# getting positions for the edges
edges_for_plot <- friendship %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.from' = 'id')) %>%
  rename(x = lng, y = lat) %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.to' = 'id')) %>%
  rename(xend = lng, yend = lat)

# rimuovo archi tra università nella stessa città
edges_for_plot<- edges_for_plot[!(edges_for_plot$x==edges_for_plot$xend & edges_for_plot$y==edges_for_plot$yend),]




###########
# PLOT
###########

# base map
maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#596673")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data('world'),
                               fill = "#CECECE", color = "#515151",
                               size = 0.15)

mapcoords <- coord_fixed(xlim = c(-150, 180), ylim = c(-55, 80))


ggplot(positions) + country_shapes +
  geom_curve(aes(x=x, y=y, xend=xend, yend=yend),     # draw edges as arcs
             data=edges_for_plot, curvature=0.33, alpha = 0.3,
             arrow = arrow(length = unit(0.01, "npc")))+
  geom_point(aes(x=lng, y=lat),           # draw nodes
             shape=16, color=positions$color, stroke=0.5) +    # scale for node size
  mapcoords +
  maptheme









