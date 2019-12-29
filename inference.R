# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(boot)
library(ergm)


# Load socio matrix
load('data/socio_matrix.RData')

# Load datasets
users <- read_csv('data/users.csv', col_types='cccccllnn') %>% distinct
friendship <- read_csv('data/friendship.csv', col_types='cc')
positions <- read_csv('data/positions.csv', col_types='cnccnnc')

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
# Show outdegree histogram
ggplot(data=users, aes(x=reorder(screen_name, -out_degree), y=out_degree, fill=out_degree)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, size=6)) +
  theme(legend.position = 'none') +
  labs(x='University', y='Outdegree')
# Le unità con valori più alti sembrano essere le università meno conosciute

# Indegree analysis (leader)
# Add indegree column to users
users <- users %>%
  add_column(in_degree = apply(socio.matrix, 2, sum))
# Show indegree histogram
ggplot(data=users, aes(x=reorder(screen_name, -in_degree), y=in_degree, fill=in_degree)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, size=6)) +
  theme(legend.position = 'none') +
  labs(x='University', y='Indegree')
# Le università più seguite sembrano essere le più prestigiose

# Joint outdegree and indegree analysis (is the outdegree inversely proportional to indegree?)
# Show scatter plot of indegree and outdegree
ggplot(data=users, aes(x=out_degree, y=in_degree, colour=screen_name, label=screen_name)) +
  geom_point() +
  geom_text(aes(label=screen_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none') +
  labs(x='Outdegree', y='Indegree')

# Check shortest paths
# Note that these shortest paths take into account directionality
shortest_paths(net.full, from='39585367') # From harvard
shortest_paths(net.full, from='562781948') # From UCT_news


# Other measures
# Compute and add betweenness score to users table
users <- users %>%
  add_column(betweenness = sqrt(betweenness(net.full)))
# Show betweenness histogram
ggplot(data=users, aes(x=reorder(screen_name, -betweenness), y=betweenness, fill=betweenness)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Betweenness')

# Compute diameter
diameter(net.full)

# # Plot network
# plot(net.full, edge.arrow.size=0.2, vertex.label=NA)
# plot(net.full, layout=layout.fruchterman.reingold, vertex.label=NA)


#ggraph(net) + geom_edge_link() +   # add edges to the plot
#              geom_node_point()    # add nodes to the plot

# Create a temporary network from friendship tibble
net.tmp <- graph_from_data_frame(
  # First table stores relationships (from -> to)
  d=friendship,
  # Second table stores vertices info (first column MUST be vertex id)
  vertices=users,
  # Graph is directed
  directed=T
)

# Show network
ggraph(net.tmp, layout='nicely') +
  geom_edge_link(aes(colour = 1)) + 
  geom_node_point() +
  geom_node_text(aes(label=screen_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none')


# Inference

# SRG MODEL
# Evaluation of the model is carried out through parametric bootstrap
# (As seen in the slides)

# Define of a function for calculating standard error
std.error <- function(x) {
  # Get the mean of x
  x.mu <- mean(x)
  # Get squared deviations
  x.sq <- sqrt(sum((x-x.mu)**2)/length(x))
  # Return squared deviation
  return(x.sq)
}

# Get standard errors of outdegree and indegree
std.error(users %>% select(out_degree) %>% pull) # Outdegree
std.error(users %>% select(in_degree) %>% pull) # Indegree

# # scrivo la funzione per calcolare lo std error di outDegree e inDegree
# stat <- function(data)
# {
#     out.deg <-  apply(data, 1,sum, na.rm= T)
#     in.deg <- apply(data, 2,sum, na.rm = T)
#     out.mu <- mean(out.deg)
#     in.mu <- mean(in.deg)
#     out.sq <- sqrt(sum((out.deg-out.mu)**2)/length(data[,1]))
#     in.sq <- sqrt(sum((in.deg-in.mu)**2)/length(data[,1]))
#     c(in.sq,out.sq) 
# }

# stat(socio.matrix)

# scrivo la funzione per generare la rete secondo RGM
# in pratica genero 199*199 bernuolli indipendenti di probabilit? la densit? della rete
# e le salvo in una matrice

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

# Define statistic function for SRG
rgm.stat <- function(socio.matrix) {
  # Get outdegree and indegree
  out.deg <- apply(socio.matrix, 1, sum, na.rm=T)
  in.deg <- apply(socio.matrix, 2, sum, na.rm=T)
  # Compute standard errors
  out.std.error <- std.error(out.deg)
  in.std.error <- std.error(in.deg)
  # Return computed standard errors
  return(c(in.std.error, out.std.error))
}

# Compute paraemtrix bootstrap (using boot(...) function)
R <- 10**4 # Define number of replicates
mu <- mean(socio.matrix) # Define mean mu
srg.boot <- boot(data=socio.matrix, statistic=rgm.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=mu)

# Save the bootstrapped model
save(srg.boot, file='data/models/srg_bootstrap')

# Analysis of the distribution of the standard error statistic for indegree, under SRG hypotesis
ggplot(data=NULL, aes(x=srg.boot$t[, 1], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=srg.boot$t0[1], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped indegree coefficients distribution',
       x='Bootstrapped coefficients', y='Density')

# head(srg.boot$t) 
# hist(srg.boot$t[,1], nclass = 50, xlim = c(2,16))
# abline(v=srg.boot$t0[1],col=2)

# Analysis of the distribution of the standard error statistic for outdegree, under SRG hypotesis
ggplot(data=NULL, aes(x=srg.boot$t[, 2], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=srg.boot$t0[2], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped outdegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')

# hist(boot.SRG$t[,2], nclass = 50, xlim = c(2,16))
# abline(v=boot.SRG$t0[2],col=2)
# mean(boot.SRG$t[,2]>boot.SRG$t0[2]) # per quello che vale

# Conclusions: data seem to sightly refuse the hypotesis of the network boing a simple random graph (SRG).
# Nodes show much higher enerogeneity either in indegree and outdegree coefficeints


# ANOVA model

# bisogna classificare come fattori indivuduali sia la socialit? che l'attrativit?

# Initialize new sociomatrix
Y <- socio.matrix # Copy sociomatrix by value
diag(Y) <- NA # Set values on the diagonal to NA

# Define latent factors
# Rows matrix: each cell contains row index
row.matrix <- matrix(data=(1:nrow(Y)), nrow=nrow(Y), ncol=nrow(Y))
# Columns matrix: each cell contains column index
col.matrix <- t(row.matrix) # Just transpose previous matrix

# Ridx[1:4,1:4]
# Cidx[1:4,1:4]
# 
# Y[1:4,1:4] 

# Vectorization
y <- c(Y) # Vectorize sociomatrix
row.v <- c(row.matrix) # Vectorize rows matrix
col.v <- c(col.matrix) # vectorize columns matrix
row.v[1:20]

# Fit logistic regression
fit.rce.nocent <- glm(y ~ factor(row.v) + factor(col.v), family=binomial)
# Check fit summary
summary(fit.logreg)
summary(glm(y~1)) # RCE ha troppo adattamento
# Save the model
save(fit.logreg, file='data/models/rce_nocent')

# Estimation of the model in which individual factors are related to deviation from the mean
# C(...) function creates the contrasts
fit.rce.cent <- glm(y ~  C(factor(row.v), sum) + C(factor(col.v), sum) , family=binomial)
# Check fit summary
summary(fit.rce.cent)
# Save the model
save(fit.rce.cent, file='data/models/rce_cent')

# Individual effects
mu.hat <- fit.rce.cent$coef[1] # Estimated mean
# Estimate a
a.hat <- fit.rce.cent$coef[1 + 1:(nrow(Y) - 1)]
a.hat <- c(a.hat, -sum(a.hat))
summary(a.hat) # Summarize estimated a
# Estimate b
b.hat <- fit.rce.cent$coef[nrow(Y) + 1:(nrow(Y) - 1)]
b.hat <- c(b.hat, -sum(b.hat)) 
summary(b.hat) # Summarize estimated b
# Compute estimated probabilities for every cell
mu.ij.mle <- mu.hat + outer(a.hat, b.hat, '+') # Mu ij maximum likelihood estimation 
p.mle <- exp(mu.ij.mle) / (1 + exp(mu.ij.mle))
diag(p.mle) <- NA


# Analysis of the inferential model through parameteric bootstrap

# First: check if generator for RGM is still working for this model
rgm.gen(Y, p.mle)

# Define statistics function for RCE
rce.stat <- function(socio.matrix) {
  # Compute standard error for out- and in- degree
  rgm.out <- rgm.stat(socio.matrix)
  out.std.error <- rgm.out[2]
  in.std.error <- rgm.out[1]
  # Add mutual dyads (useful for independecy of y variables evaluation)
  conc <- (socio.matrix == t(socio.matrix))
  mu.dy <- sum(conc[socio.matrix == 1], na.rm=T) / 2
  # Return standard errors and mutual dyads
  return(c(in.std.error, out.std.error, mu.dy))
}

# stat(Y)
# # controllo che rangen1 funzioni anche per questo modello
# ran.gen1(Y,p.mle)
# # tutto ok

# Compute bootstrapped model
R <- 10**4 # Replicates
rce.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=p.mle)
# Save the bootstrapped model
save(boot.rce, file='data/models/rce_bootstrap')

# Indegree statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 1], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[1], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped indegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
rce.in.pval <- 2 * mean(boot.RCE$t[, 1]< boot.RCE$t0[1])
rce.in.pval
# head(boot.RCE$t)  
# hist(boot.RCE$t[,1], nclass = 50)
# abline(v=boot.RCE$t0[1],col=2)
# p-value bilaterale

# Outdegree statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 2], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[2], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped outdegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
rce.out.pval <- 2 * mean(boot.RCE$t[, 2] < boot.RCE$t0[2])
rce.out.pval
# hist(boot.RCE$t[,2], nclass = 50)
# abline(v=boot.RCE$t0[2],col=2)
# # p-value bilaterale
# 2*mean(boot.RCE$t[,2]< boot.RCE$t0[2])

# Mutual dyads statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 3], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[3], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped mutual dyads coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# # distribuzione del numero di diadi mutue sotto l'ipotesi di RCE
# hist(boot.RCE$t[,3], nclass = 50, xlim = c(100,600))
# abline(v=boot.RCE$t0[3],col=2)

# Conclusions: outdegree and indegree are very well modelled by RCE model.
# However, it turns out from mutual dyads distribution that there are some dependency 
# effects which are still not considered and which must therefore be investigated.


# More complex model: Exponentially parametrized Random Graph Model (ERGM)
# This model takes into account either some covariates

# # mi creo l'esplicativa 'usa' che indica le universit? americane
# usa <- c(1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
#          1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1,
#          1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
#          0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
#          1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0,
#          0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0,
#          0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
#          0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
#          0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
#          0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1,
#          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1) # 196 messo 1 e tolto uno zero in fondo dopo aver visto socio.matrix
# Define if a university is in the US

country <- (users %>% select(country) %>% pull)
is_usa <- !is.na(country) & country == 'US'
# Define ranking
ranking <- c(rep('top25', 25), rep('top50', 25), rep('top100', 50), rep('top225', 223 - (25 + 25 + 50)))


# Define number of followers
followers <- users %>% select(followers_count) %>% pull
# Define number of friends
friends <- users %>% select(friends_count) %>% pull
# Define if user is verified
verified <- users %>% select(verified) %>% pull

# cbind(rownames(socio.matrix), usa)         
rankA <- c(rep(1,25),rep(0,200))
rankB <- c(rep(0,25),rep(1,25),rep(0,175))
rankC <- c( rep(0,50), rep(1,50), rep(0,125))
rankD <- c( rep(0,100),rep(1,125))

# New network object
ergm.net <- as.network(socio.matrix[-v.isolated,-v.isolated])
diag(socio.matrix) <- NA
set.vertex.attribute(ergm.net, 'is_usa', is_usa[-v.isolated])
set.vertex.attribute(ergm.net, 'ranking', ranking[-v.isolated])
set.vertex.attribute(ergm.net, 'followers', followers[-v.isolated])
set.vertex.attribute(ergm.net, 'friends', friends[-v.isolated])
set.vertex.attribute(ergm.net, 'verified', verified[-v.isolated])
set.vertex.attribute(ergm.net, 'ranking', ranking[-v.isolated])

# creo anche questi perch� vado meglio a capire che contrasti fare
set.vertex.attribute(ergm.net, 'rank25', rankA[-v.isolated])
set.vertex.attribute(ergm.net, 'rank50', rankB[-v.isolated])
set.vertex.attribute(ergm.net, 'rank100', rankC[-v.isolated])
set.vertex.attribute(ergm.net, 'rank225', rankD[-v.isolated])

# Fit ergm equivalent to logistic regression (independece of y)
fit.ergm.0 <- ergm(ergm.net ~ edges)
# AIC: 23231 (very high)
summary(fit.ergm.0)

# Fit new ergm model
fit.ergm.1 <- ergm(ergm.net ~ edges + nodeofactor('is_usa') + nodeifactor('is_usa') +
                     nodeofactor('ranking', levels=c('top25', 'top50', 'top100')) +
                     nodeifactor('ranking', levels=c('top25', 'top50', 'top100')) +
                     nodematch('is_usa') + nodematch('ranking'))
# AIC: 21708 (still too high)
summary(fit.ergm.1)

# RCE effects modeled through ERGM
fit.ergm.2 <- ergm(ergm.net ~ edges + sender + receiver)
# AIC: 31251
summary(fit.ergm.2)

# # ho il dubbio che l'aic venga fatto su verosimiglianze con fattori moltiplicativi diversi
# # controllo se i coefficienti sono uguali al modello ottenuto con glm
# summary(fit)
# cbind(fit$coefficients,fit.ergm2$coef) # si, i modelli sono gli stessi
# # quindi va usato solo un pacchetto

# Trying new models to lower AIC score

fit.ergm.3 <- ergm(ergm.net ~ edges + sender)
summary(fit.ergm.3) # AIC: 30174

fit.ergm.4 <- ergm(ergm.net ~ edges + receiver)
summary(fit.ergm.4) # AIC: 25547

# Model with outdegree and indegree
fit.ergm.5 <- ergm(ergm.net ~ edges + mutual)
summary(fit.ergm.5) # AIC: 21757

# Best model at this time
fit.ergm.6 <- ergm(ergm.net ~ edges + mutual +
                   nodeofactor('is_usa') + nodeifactor('is_usa') + nodematch('is_usa') +
                   nodeocov('friends') + nodeicov('friends') +
                   nodeocov('followers') + nodeicov('followers') +
                   nodeocov('verified') + nodeicov('verified') + nodematch('verified') +
                   nodeofactor('ranking', levels=c('top25', 'top50', 'top100')) +
                   nodeifactor('ranking', levels=c('top25', 'top50', 'top100')) +
                   nodemix('ranking', levels=c('top25', 'top50', 'top100')),
                   control = control.ergm(MCMLE.maxit = 30))
summary(fit.ergm.6) # AIC
# Save model to disk
save(fit.ergm.6, file='data/models/ergm_6')

# Search for a relationship between ranking, outdegree and indegree
# Add ranking attribute to user
users <- users %>%
  add_column(ranking=1:(dim(users)[1]))
# Plot outdegree with respect to ranking position
ggplot(users, aes(x=reorder(screen_name, ranking), y=out_degree, fill=1)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Outdegree')
# Plot indegree with respect to ranking position
ggplot(users, aes(x=reorder(screen_name, ranking), y=in_degree, fill=1)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Indegree')

# # 
# outD <- apply(socio.matrix,1,sum,na.rm=T)
# inD <- apply(socio.matrix,2,sum,na.rm=T)

# # cerco di vedere se c'? una relazione tra posizione e outdeg e indeg
# # perch? al momento non si riesce a spiegare la variabilit?
# plot(seq(1:199),outD) # non si vede molto forse ? per questo che non riusciamo a spiegare
#                       # la variabilit? in uscita
# plot(seq(1:199),inD)
# plot(X$friends_count[1:199],inD)

# Get out degree array
out.degree <- users %>% select(out_degree) %>% pull
# Show quantiles
quantile(out.degree, probs=c(0.25,0.5,0.75,0.90))
# stampo le universit? che hanno un outdegree che supera il 75% percentile
# Show universities whose outdegree exceeds the 75-th percentile
outliers <- (out.degree > 20)
outliers

# commento outdegree: ? evidente come vi sia circa un 25% di ossservazioni anomale
# va indagato il perch?

fit.ergm.7 <- ergm(ergm.net ~ edges + mutual+
                 + nodeocov('is_usa') + nodeicov('is_usa') + nodematch('is_usa')
                 + nodeocov('friends') + nodeicov('friends')
                 + nodeocov('followers') + nodeicov('followers')
                 + nodeocov('verified') + nodeicov('verified') + nodematch('verified')
                 + nodeocov('rank25') + nodeicov('rank25')+nodematch('rank25', levels=1)
                 + nodeocov('rank50') + nodeicov('rank50')+nodematch('rank50', levels=1)
                 + nodeocov('rank100') + nodeicov('rank100')+nodematch('rank100', levels=1)
                 + nodematch('rank225', levels=1),
                 control = control.ergm(MCMLE.maxit = 30,MCMC.samplesize = 2048)
)
# AIC: 
summary(fit.ergm.7)

# Commento del modello
# Questo modello sembra il migliore e offre anche delle interpretazioni interessanti
# le universit? tendono a seguire di pi? se hanno un renking scarso mentre vengono seguite
# di pi? le universit? pi? famose. Stesso discorso tra americane e non
# scrivo la funzione per generare dal modello stimato 
# NB data=socio.matrix fit invece deve essere l'ergm che vogliomo testare
ergm.gen <- function(socio.matrix, fit) {
  # Compute new matrix
  out.matrix <- as.matrix(simulate(fit)[1:nrow(socio.matrix), 1:nrow(socio.matrix)])
  diag(out.matrix) <- NA
  # return computed matrix
  return(out.matrix)
}
# apply(ran.gen2(socio.matrix,fit.ergm7),1,sum,na.rm=T)

# Faccio il bootstrap parametrico utilizzando la funzione boot
R <- 10 # Number of replicates
ergm.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen = ergm.gen, mle=fit.ergm.7)
ergm.boot$t0
# Save the model to disk
save(ergm.boot, file='data/models/ergm_bootstrap')
# i warnigns erano previsti

# Indegree statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 1], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=ergm.boot$t0[1], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped indegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
ergm.in.pval <- 2 * mean(ergm.boot$t[, 1]< ergm.boot$t0[1])
ergm.in.pval

# Outdegree statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 2], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=ergm.boot$t0[2], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped outdegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
ergm.out.pval <- 2 * mean(ergm.boot$t[, 2]< ergm.boot$t0[2])
ergm.out.pval

# Mutual dyads statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 3], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=ergm.boot$t0[3], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped mutual dyads coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Commento: 
# In realt? il risultato ? ancora abbastanza insoddisfacente in outdegree e indegree

