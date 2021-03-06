# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(boot)
library(ergm)
library(amen)


# Load socio matrix
setwd("~/GitHub/smhdd_network")
load('data/socio_matrix.RData')


# Load datasets
users <- read_csv('data/users.csv', col_types='ncnccccclcnnn') %>% distinct
friendship <- read_csv('data/friendship.csv', col_types='cc')
positions <- read_csv('data/positions.csv', col_types='cnccnnc')

# Removing universities with users$id=NA
users <- users[!is.na(users$id),]

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


# Create igraph from adjacency matrix
net.full = graph_from_adjacency_matrix(socio.matrix, mode='directed', weighted=NULL, diag=F)
net.full

# Show network info
str(net.full)


# Network analysis
E(net.full) # Show edges
V(net.full) # Show vertices

# Compute and show edge density
edge_density(net.full)

# Add degree column to users
users <- users %>%
  add_column(degree=degree(net.full))
# Get users which have no friends (i.e. overall degree 0)
v.isolated <- which(degree(net.full) < 1)
str(v.isolated)

# Remove isolated vertices
net = delete_vertices(net.full, v.isolated)
net

# Outdegree analysis (sociability?)
# Add outdegree column to users
users <- users %>% 
  add_column(out_degree = apply(socio.matrix, 1, sum))

# Le unità con valori più alti sembrano essere le università meno conosciute

# Indegree analysis (leader)
# Add indegree column to users
users <- users %>%
  add_column(in_degree = apply(socio.matrix, 2, sum))

# Le università più seguite sembrano essere le più prestigiose

### INFERENCE ######�
std.error <- function(x) {
  # Get the mean of x
  x.mu <- mean(x, na.rm = T)
  # Get squared deviations
  x.sq <- sqrt(sum((x-x.mu)**2, na.rm = T)/length(x))
  # Return squared deviation
  return(x.sq)
}

# Get standard errors of outdegree and indegree
std.error(users %>% select(out_degree) %>% pull) # Outdegree
std.error(users %>% select(in_degree) %>% pull) # Indegree

# Define a simple random graph (SRG) random graph model (RGM)
# It is composed of N*N i.i.d. Bernoulli, wose probabilities are given by network density
# It takes as input the adjacency matrix and a probability
rgm.gen <- function(socio.matrix, prob) {
  # Define size of the adjacency matrix
  socio.matrix.dim <- dim(socio.matrix)
  # Define number of nodes
  n.nodes <- socio.matrix.dim[1]
  # Define number of edges (directed graph)
  n.edges <- n.nodes ** 2
  # Create new adjacency matrix
  out.matrix <- matrix(data=rbinom(n=n.edges, size=1, prob=prob), ncol=n.nodes)
  diag(out.matrix) <- NA # Remove elements on the diagonal
  # Return the newly generated matrix
  return(out.matrix)
}

rce.stat <- function(Y) {
  # Compute standard error for out- and in- degree
  rgm.out <- rgm.stat(Y)
  out.std.error <- rgm.out[2]
  in.std.error <- rgm.out[1]
  # Add mutual dyads (useful for independecy of y variables evaluation)
  conc <- (Y == t(Y))
  mu.dy <- sum(conc[Y == 1], na.rm=T) / 2
  # Return standard errors and mutual dyads
  return(c(in.std.error, out.std.error, mu.dy))
}

#diag(socio.matrix)<- NA
rce.stat(socio.matrix)

# Compute paraemtrix bootstrap (using boot(...) function)

R <- 10**2 # Define number of replicates
mu <- mean(socio.matrix, na.rm = T) # Define mean mu
srg.boot <- boot(data=socio.matrix, statistic=rgm.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=mu)
hist(srg.boot$t[,4])
srg.boot$t0[4]

# Fa schifo anche nelle nuove statistiche

# creo vettore di esplicative
rankA <- c(rep(1,25),rep(0,220-25))
rankB <- c(rep(0,25),rep(1,25),rep(0,220-50))
rankC <- c( rep(0,50), rep(1,50), rep(0,220-100))

country <- (users %>% select(country.y) %>% pull)
# Define if the university is located in US
is_usa <- !is.na(country) & country== 'US'
# Define number of followers
followers <- users %>% select(followers_count) %>% pull
# Define number of friends
friends <- users %>% select(friends_count) %>% pull
# Define if user is verified
verified <- users %>% select(verified) %>% pull
# define number of post
post_num <- users %>% select(n_posts) %>% pull
# define language
language <- users %>% select(language)%>% pull

X <- cbind(rankA,rankB,rankC)

# Esplicative a livello di diadi
XlegAA<- outer(rankA,rankA)
XlegBB<- outer(rankB,rankB)
XlegCC<- outer(rankC,rankC)
XlegAB <- outer(rankA,rankB)
XlegBA <- outer(rankB,rankA)
XlegAC <- outer(rankA,rankC)
XlegCA <- outer(rankC,rankA)
XlegBC <- outer(rankB,rankC)
XlegCB <- outer(rankC,rankB)

Xleg <- array(NA, dim = c(220,220,9))
Xleg[,,1]<-XlegAA 
Xleg[,,2]<-XlegBB 
Xleg[,,3]<- XlegCC
Xleg[,,4]<- XlegAB
Xleg[,,5]<- XlegBA
Xleg[,,6]<- XlegAC
Xleg[,,7]<- XlegCA
Xleg[,,8]<- XlegBC
Xleg[,,9]<- XlegCB

# Adesso che ho visto il corrispettivo  con quello che abbiamo fatto finora passo a amen

# Modello SRG
fit_SRG<-ame(socio.matrix,model="bin",rvar=FALSE,cvar=FALSE,dcor=FALSE)
summary(fit_SRG)
plot(fit_SRG)
save(fit_SRG, file='Amen models/SRG')

# Notiamo come l'adattamento risulta povero per tutte e quattro le statistiche di nostro 
# interesse

# Modello con effetti casuali
fit_SRM<-ame(socio.matrix,model="bin")
summary(fit_SRM)
save(fit_SRM, file='Amen models/SRM')

# Commento: a livello di outdegree e indegree il modello risulta decisamente migliore. Stesso 
# discorso per la dipendenza tra diadi ( CONTROLLARE COSA VUOL DIRE EFFETTIVAMENTE QUESTA STATISTICA)
# Ci sono comunque ancora problemi a livello di triade

# Modello con effetto delle covariate
fit_AME<-ame(socio.matrix, model="bin", r=3)
summary(fit_AME)
plot(fit_AME)
save(fit_AME, file='Amen models/AME')


# provo a inserire delle covariate di diadi
# calcola tutte le possibili differenze di ranking tra i nodi
fit_AME2<-ame(socio.matrix,Xdyad = Xleg,Xr=X,Xc=X, model="bin")
summary(fit_AME2)
save(fit_AME, file='Amen models/AME2')
fit_AME2$YPM # queste in  sono probabilit� a posteriori quindi se le passiamo come un vettore 
# possiamo calcolarci le statistiche solite

R <- 10**2 # Define number of replicates
mu <- c(fit_AME2$YPM) # Define mean mu
srg.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=mu)
mean(srg.boot$t[,3]>srg.boot$t0[3])
srg.boot$t0
