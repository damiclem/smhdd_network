library(tidyverse)
library(igraph)
library(ggmap)
library(viridis)


##################
# RETRIEVE EDGES
##################

# Get files
friendship <- read.csv("data/friendship.csv", stringsAsFactors=FALSE)
users <- read.csv('data/users.csv', stringsAsFactors=FALSE) 
positions <- read.csv("data/positions.csv", stringsAsFactors=FALSE)

# Remove NAs from positions
positions <- positions %>% 
  filter(!is.na(id)) %>%
  select(id, everything())

# Add 'from' user info
friendship <- users %>% 
  select(id) %>%
  inner_join(friendship, by=c('id' = 'from'))

# Filter friendship by known users
friendship <- friendship %>%
  inner_join(users %>% select(id),
             by=c('to' = 'id'),
             suffix=c('.from', '.to')) %>%
  rename(id.from = id, id.to = to) %>%
  distinct

# Create network object
g <- graph_from_data_frame(friendship, directed = TRUE, vertices = positions)

# Get positions for the edges
edges_for_plot <- friendship %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.from' = 'id')) %>%
  rename(x = lng, y = lat) %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.to' = 'id')) %>%
  rename(xend = lng, yend = lat) %>%
  filter(!((x == xend) & (y == yend))) # Exclude edges between universities of the same cities


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


ggplot() + country_shapes +
  # Draw lines
  geom_curve(data=edges_for_plot,
             aes(x=x, y=y, xend=xend, yend=yend),
             curvature=0.33, alpha=0.2, color='dodgerblue') +
  # Draw points
  geom_point(data=positions, 
             aes(x=lng, y=lat),
             color=positions$color) +
  # Draw map
  mapcoords +
  maptheme
  
