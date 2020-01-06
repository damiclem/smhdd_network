# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(plotly)


# Load dataset

# Read .csv tables composing dataset
users <- read_csv('data/users.csv', col_types='ncnccccclcnnn') %>%
  select(id, everything()) %>%
  filter(!is.na(id)) %>%
  distinct
friendship <- read_csv('data/friendship.csv', col_types='cc')
positions <- read_csv('data/positions.csv', col_types='cccccnnc')

# Keep only friendship with valid users
friendship <- friendship %>%
  inner_join(users %>% select(id), by=c('from'='id')) %>%
  inner_join(users %>% select(id), by=c('to'='id'))

# Get positions for each univeristy user
positions <- positions %>%
  select(id=id, name=university, country, location, lat, lng) %>%
  mutate(id=as.character(id)) %>%
  distinct

# Remove duplicated positions
positions <- positions %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup

# Add position info to university data
users <- users %>%
  select(-location) %>%
  left_join(positions, by=c('id'='id'))


# # Add users attributes to friendship table
# friendship <- friendship %>%
#   left_join(users, by=c('from'='id'))


# Generate the network

# Create a network from friendship tibble
net <- graph_from_data_frame(
  # First table stores relationships (from -> to)
  d=friendship,
  # Second table stores vertices info (first column MUST be vertex id)
  vertices=users,
  # Graph is directed
  directed=TRUE
)

# Exclude xero degree vertices
net.nodeg <- delete_vertices(net, degree(net) < 1)

# Define a layout with igraph
layout.fr <- create_layout(net.nodeg, layout='fr', niter=1e03)
layout.fr
# Clustering betweenness
cluster.btw <- cluster_edge_betweenness(net.nodeg, directed=T)
members.btw <- as.factor(cluster.btw$membership)
cluster.btw

# Clustering 
cluster.wt <- cluster_walktrap(net.nodeg)
members.wt <- as.factor(cluster.wt$membership)
cluster.wt

# Plot the network

# Simplest graph
ggraph(layout.fr) +
  geom_edge_link(aes(colour=1), alpha=.1) + 
  geom_node_point(aes(size=degree(net.nodeg), colour=members.btw, alpha=.3)) +
  theme(legend.position = 'none')

# Show network: size of nodes is proportional to overall degree
ggraph(net.layout) +
  geom_edge_link(aes(colour=1), alpha=.1) + 
  geom_node_point(aes(size=degree(net.nodeg), colour=1, alpha=.3)) +
  geom_node_text(aes(label=twitter_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none')

# Show network: size of nodes is proportional to indegree (squared)
ggraph(net.nodeg, layout='kk') +
  geom_edge_link(aes(colour=1), alpha=.1) + 
  geom_node_point(aes(size=degree(net.nodeg, mode='in')**2)) +
  geom_node_text(aes(label=screen_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none') +
  labs(x=NULL, y=NULL, title='Node size proportional to (squared) indegree')

# Show network: size of nodes is proportional to outdegree (squared)
ggraph(net.nodeg, layout='kk', maxiter=1000) +
  geom_edge_link(aes(colour=1), alpha=.1) + 
  geom_node_point(aes(size=degree(net.nodeg, mode='out')**2)) +
  geom_node_text(aes(label=screen_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none') +
  labs(x=NULL, y=NULL, title='Node size proprotional to (squared) outdegree')


