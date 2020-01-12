# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(boot)
library(ergm)


# Load socio matrix
load('data/socio_matrix.RData')

# Load datasets
users <- read_csv('data/users.csv', col_types='ncnccccclcnnn') %>%
  filter(!is.na(id)) %>%
  distinct
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
  select(-location, -country) %>%
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

# Show outdegree ranking barplot
ggplot(data=users, aes(x=reorder(twitter_name, -out_degree), y=out_degree, fill=out_degree)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, size=6)) +
  theme(legend.position = 'none') +
  labs(x='University', y='Outdegree')
# Comment on the plot: units with highest outdegree seem to be the less known ones

# Show outdegree histogram
ggplot(data=users, aes(x=out_degree, y=..density.., fill=1)) +
  geom_histogram(bins=30) +
  theme(legend.position = 'none') +
  labs(x='Outdegree', y='Frequency')

# Indegree analysis (leader)
# Add indegree column to users
users <- users %>%
  add_column(in_degree = apply(socio.matrix, 2, sum))

# Show indegree ranking braplot
ggplot(data=users, aes(x=reorder(twitter_name, -in_degree), y=in_degree, fill=in_degree)) +
  geom_bar(stat='identity') +
  theme(axis.text.x = element_text(angle=90, size=6)) +
  theme(legend.position = 'none') +
  labs(x='University', y='Indegree')
# About the plot: most followed universities seem to be the better known ones

# Show indegree histogram
ggplot(data=users, aes(x=in_degree, y=..density.., fill=1)) +
  geom_histogram(bins=30) +
  theme(legend.position = 'none') +
  labs(x='Indegree', y='Frequency')

# Joint outdegree and indegree analysis (is the outdegree inversely proportional to indegree?)
# Show scatter plot of indegree and outdegree
ggplot(data=users, aes(x=out_degree, y=in_degree, colour=twitter_name, label=twitter_name)) +
  geom_point() +
  geom_text(aes(label=twitter_name), hjust=-0.2, vjust=0, size=3) +
  theme(legend.position = 'none') +
  labs(x='Outdegree', y='Indegree')

# Compute distribution of indegree
indeg.dist  <- users %>% 
  select(in_degree) %>% 
  count(in_degree) %>% 
  mutate(f=n/sum(n))

# Compute distribution of indegree
outdeg.dist  <- users %>% 
  select(out_degree) %>% 
  count(out_degree) %>% 
  mutate(f=n/sum(n))

# Plot degree distribution
ggplot(data=NULL) +
  geom_point(data=indeg.dist, aes(x=in_degree, y=f, color=as.factor('In'))) +
  geom_point(data=outdeg.dist, aes(x=out_degree, y=f, color=as.factor('Out'))) +
  labs(x='Degree', y='Frequency', color='Degree type') +
  theme(legend.position=c(.91, .91))

# Plot degree distribution
ggplot(data=NULL) +
  geom_point(data=indeg.dist, aes(x=log10(in_degree), y=log10(f), color=as.factor('In'))) +
  geom_point(data=outdeg.dist, aes(x=log10(out_degree), y=log10(f), color=as.factor('Out'))) +
  labs(x='Degree', y='Frequency', color='Degree type') +
  theme(legend.position=c(.91, .91))

# Plot cumulative degree distribution
ggplot(data=NULL) +
  geom_point(data=indeg.dist, aes(x=log10(in_degree), y=cumsum(log10(f)), color=as.factor('In'))) +
  geom_point(data=outdeg.dist, aes(x=log10(out_degree), y=cumsum(log10(f)), color=as.factor('Out'))) +
  labs(x='Degree', y='Frequency', color='Degree type') +
  theme(legend.position=c(.91, .91))
  

# Check shortest paths
# Note that these shortest paths take into account directionality
shortest_paths(net.full, from='39585367') # From harvard
shortest_paths(net.full, from='562781948') # From UCT_news


# Other measures
# Compute and add betweenness score to users table
users <- users %>%
  add_column(betweenness = sqrt(betweenness(net.full)))
# Show betweenness histogram
ggplot(data=users, aes(x=reorder(twitter_name, -betweenness), y=betweenness, fill=betweenness)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Betweenness')

# Compute diameter
diameter(net.full)

# # Plot network
# plot(net.full, edge.arrow.size=0.2, vertex.label=NA)
# plot(net.full, layout=layout.fruchterman.reingold, vertex.label=NA)


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
summary(fit.rce.nocent)
summary(glm(y~1)) # RCE ha troppo adattamento
# Save the model
save(fit.rce.nocent, file='data/models/rce_nocent')

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
save(rce.boot, file='data/models/rce_bootstrap')

# Indegree statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 1], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[1], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped indegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
rce.in.pval <- 2 * mean(rce.boot$t[, 1]< rce.boot$t0[1])
rce.in.pval

# Outdegree statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 2], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[2], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped outdegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
rce.out.pval <- 2 * mean(rce.boot$t[, 2] < rce.boot$t0[2])
rce.out.pval

# Mutual dyads statistic: analysis of the standard error distribution, under the RCE hypotesis
ggplot(data=NULL, aes(x=rce.boot$t[, 3], y=..density.., fill=1)) +
  geom_histogram(bins=200) +
  geom_vline(xintercept=rce.boot$t0[3], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped mutual dyads coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')

# Conclusions: outdegree and indegree are very well modelled by RCE model.
# However, it turns out from mutual dyads distribution that there are some dependency 
# effects which are still not considered and which must therefore be investigated.


# More complex model: Exponentially parametrized Random Graph Model (ERGM)
# This model takes into account either some covariates

# Add some covariates
country <- (users %>% select(country) %>% pull)
is_usa <- !is.na(country) & country == 'US'
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

# creo anche questi perch? vado meglio a capire che contrasti fare
set.vertex.attribute(ergm.net, 'rank25', rankA[-v.isolated])
set.vertex.attribute(ergm.net, 'rank50', rankB[-v.isolated])
set.vertex.attribute(ergm.net, 'rank100', rankC[-v.isolated])
set.vertex.attribute(ergm.net, 'rank225', rankD[-v.isolated])

# Fit ergm equivalent to logistic regression (independece of y)
fit.ergm.0 <- ergm(ergm.net ~ edges)
# AIC: 21982 (very high)
summary(fit.ergm.0)

# Fit new ergm model
fit.ergm.1 <- ergm(ergm.net ~ edges + nodeofactor('is_usa') + nodeifactor('is_usa') +
                     nodeofactor('ranking', levels=c('top25', 'top50', 'top100')) +
                     nodeifactor('ranking', levels=c('top25', 'top50', 'top100')) +
                     nodematch('is_usa') + nodematch('ranking'))
# AIC: 20445 (still too high)
summary(fit.ergm.1)

# RCE effects modeled through ERGM
fit.ergm.2 <- ergm(ergm.net ~ edges + sender + receiver)
# AIC: 23357
summary(fit.ergm.2)


# Trying new models to lower AIC score

fit.ergm.3 <- ergm(ergm.net ~ edges + sender)
summary(fit.ergm.3) # AIC: 25298

fit.ergm.4 <- ergm(ergm.net ~ edges + receiver)
summary(fit.ergm.4) # AIC: 20506

# Model with outdegree and indegree
fit.ergm.5 <- ergm(ergm.net ~ edges + mutual)
summary(fit.ergm.5) # AIC: 20404

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
ggplot(users, aes(x=reorder(twitter_name, ranking), y=out_degree, fill=1)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Outdegree')

# Plot indegree with respect to ranking position
ggplot(users, aes(x=reorder(twitter_name, ranking), y=in_degree, fill=1)) +
  geom_bar(stat='identity') +
  theme(axis.text.x=element_text(angle=90, size=6)) +
  theme(legend.position='none') +
  labs(x='University', y='Indegree')

# Get out degree array
out.degree <- users %>% select(out_degree) %>% pull
# Show quantiles
quantile(out.degree, probs=c(0.25,0.5,0.75,0.90))
# stampo le universit? che hanno un outdegree che supera il 75% percentile
# Show universities whose outdegree exceeds the 75-th percentile
outliers <- (out.degree > 20)
outliers

# There are 25% circa of anomal
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
# AIC: 17099
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
R <- 200 # Number of replicates
ergm.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=ergm.gen, mle=fit.ergm.7)
ergm.boot$t0
# Save the model to disk
save(ergm.boot, file='data/models/ergm_bootstrap')
# i warnigns erano previsti

# Indegree statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 1], y=..density.., fill=1)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept=ergm.boot$t0[1], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped indegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
ergm.in.pval <- 2 * mean(ergm.boot$t[, 1]< ergm.boot$t0[1])
ergm.in.pval

# Outdegree statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 2], y=..density.., fill=1)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept=ergm.boot$t0[2], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped outdegree coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Bilateral p-value
ergm.out.pval <- 2 * mean(ergm.boot$t[, 2]< ergm.boot$t0[2])
ergm.out.pval

# Mutual dyads statistic: analysis of the standard error distribution, under the ERGM hypotesis
ggplot(data=NULL, aes(x=ergm.boot$t[, 3], y=..density.., fill=1)) +
  geom_histogram(bins=30) +
  geom_vline(xintercept=ergm.boot$t0[3], color=2) +
  theme(legend.position='none') +
  labs(title='Bootstrapped mutual dyads coefficients distribution', 
       x='Bootstrapped coefficients', y='Density')
# Commento: in realt? il risultato ? ancora abbastanza insoddisfacente in outdegree e indegree

