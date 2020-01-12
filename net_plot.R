# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(plotly)


# Set seed for reproducibility
set.seed(42)


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

# Compute Fruchterman-Renigold layout
layout.fr <- create_layout(net.nodeg, layout='fr', niter=1e05)
layout.fr

# PageRank
net.nodeg.pr <- page.rank(net.nodeg)
net.nodeg.pr$vector


# Plot the network

# Simple FR graph
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(color='dodgerblue', size=3) +
  theme(legend.position = 'none')

# Show network: color proportional to overall degree
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(aes(color=degree(net.nodeg)), size=3) +
  scale_color_viridis() +
  theme(legend.position='none')

# Show network: color proportional to in-degree
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(aes(color=degree(net.nodeg, mode='in')), size=3) +
  scale_color_viridis() +
  theme(legend.position='none')

# Show network: color proportional to out-degree
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(aes(color=degree(net.nodeg, mode='out')), size=2) +
  scale_color_viridis() +
  theme(legend.position='none')

# Show network: color proportional to PageRank
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(aes(color=net.nodeg.pr$vector), size=2) +
  scale_color_viridis() +
  theme(legend.position='none')

# Show network: color proportional to betweenness
ggraph(layout.fr) +
  geom_edge_link(color='grey', alpha=.3) + 
  geom_node_point(aes(color=betweenness(net.nodeg)), size=2) +
  scale_color_viridis() +
  theme(legend.position='none')

# Store dataframe from graph
df.nodeg <- as_data_frame(net.nodeg, what='vertices')
df.nodeg$btw <- betweenness(net.nodeg) # Add betweenness
df.nodeg$page_rank <- net.nodeg.pr$vector # Add PageRank

# Betweenness ranking
ggplot(data=df.nodeg[1:30, ], aes(x=reorder(twitter_name, -btw), y=btw)) +
  geom_bar(aes(fill=btw), stat='identity') +
  scale_fill_viridis_c() +
  theme(legend.position='none',
        axis.text.x=element_text(angle=90, vjust=0.5)) +
  labs(x='University', y='Betweenness')

# PageRank ranking
ggplot(data=df.nodeg[1:30, ], aes(x=reorder(twitter_name, -page_rank), y=page_rank)) +
  geom_bar(aes(fill=page_rank), stat='identity') +
  scale_fill_viridis_c() +
  theme(legend.position='none',
        axis.text.x=element_text(angle=90, vjust=0.5)) +
  labs(x='University', y='PageRank')



# Interactive plot
vertices <- V(net.nodeg) # Get vertices
edges <- get.edgelist(net.nodeg, names=F) # Get edges

# Plot edges
vertices.x <- layout.fr[, 1]
vertices.y <- layout.fr[, 2]

# Create interactive plot markers
vertices.markers <- plot_ly(
  x = ~vertices.x,
  y = ~vertices.y, 
  type='scatter', 
  mode='markers',
  size=net.nodeg.pr$vector,
  color=net.nodeg.pr$vector*1000,
  text=paste(vertices$twitter_name, 
             '\nDegree: ', degree(net.nodeg), 
             '\nIndegree: ', degree(net.nodeg, mode='in'),
             '\nOutdegree: ', degree(net.nodeg, mode='out'),
             '\nPageRank (1e-03): ', round(net.nodeg.pr$vector*1000, 3)),
  hoverinfo='text'
)

# Create edges
edges.lines <- list() # Define lines list

# Loop thorugh each node
for(i in 1:length(edges[, 1])) {
  # Define start and end vertex
  v0 <- edges[i,][1]
  v1 <- edges[i,][2]
  # Create edge
  edge.line = list(
    type='line',
    line=list(color='grey', width=0.3),
    x0=vertices.x[v0],
    y0=vertices.y[v0],
    x1=vertices.x[v1],
    y1=vertices.y[v1],
    layer='below',
    opacity=0.3
  )
  # Save newly created edge to list
  edges.lines[[i]] <- edge.line
}

# Define plot settings
axis <- list(title='', showgrid=FALSE, showticklabels=FALSE, zeroline=FALSE)

# Show layout
layout(
  vertices.markers,
  title = 'Network of Universities Twitter Accounts',
  shapes = edges.lines,
  xaxis = axis,
  yaxis = axis
)