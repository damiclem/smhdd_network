library(tidyverse)
library(igraph)


##################
# RETRIEVE EDGES
##################
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


# getting positions for the edges
edges_for_plot <- friendship %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.from' = 'id')) %>%
  rename(x = lng, y = lat) %>%
  inner_join(positions %>% select(id, lng, lat), by = c('id.to' = 'id')) %>%
  rename(xend = lng, yend = lat)

# remove edges between universities in the same city
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
