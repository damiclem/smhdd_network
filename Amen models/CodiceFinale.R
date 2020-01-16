rm(list=ls())
# Dependencies
library(tidyverse)
library(igraph)
library(ggraph)
library(boot)
library(ergm)
library(amen)
library(gridExtra)

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


### INFERENCE ######à
std.error <- function(x) {
  # Get the mean of x
  x.mu <- mean(x, na.rm = T)
  # Get squared deviations
  x.sq <- sqrt(sum((x-x.mu)**2, na.rm = T)/length(x))
  # Return squared deviation
  return(x.sq)
}

# Define a simple random graph (SRG) random graph model (RGM)
# It is composed of N*N i.i.d. Bernoulli, wose probabilities are given by network density
# It takes as input the adjacency matrix and a probability

rgm.stat <- function(Y) {
  # Get outdegree and indegree
  out.deg <- apply(Y, 1, sum, na.rm=T)
  in.deg <- apply(Y, 2, sum, na.rm=T)
  # Compute standard errors
  out.std.error <- std.error(out.deg)
  in.std.error <- std.error(in.deg)
  # Return computed standard errors
  return(c(in.std.error, out.std.error))
}

# generatore rete
rgm.gen <- function(socio.matrix, prob) {
  # Define size of the adjacency matrix
  socio.matrix.dim <- dim(socio.matrix)
  # Define number of nodes
  n.nodes <- socio.matrix.dim[1]
  # Define number of edges (directed graph)
  n.edges <- n.nodes ** 2
  # Create new adjacency matrix
  out.matrix <- matrix(data=rbinom(n=n.edges, size=1, prob=prob),byrow = F, ncol=n.nodes)
  diag(out.matrix) <- NA # Remove elements on the diagonal
  # Return the newly generated matrix
  return(out.matrix)
}

rce.stat <- function(Y) {
  # Compute standard error for out- and in- degree
  rgm.out <- rgm.stat(Y)
  out.std.error <- rgm.out[2]
  in.std.error <- rgm.out[1]
  # Add dyads depency (useful for independecy of y variables evaluation)
  #dyad.dep<- suppressWarnings( cor( c(Y),c(t(Y)) , use="complete.obs") )
  # Add mutual dyads (useful for independecy of y variables evaluation)
  conc <- (Y == t(Y))
  mu.dy <- sum(conc[Y == 1], na.rm=T) / 2
  # Return standard errors and mutual dyads
  return(c(in.std.error, out.std.error, mu.dy))
}

#diag(socio.matrix)<- NA
rce.stat(socio.matrix)

# Compute paraemtrix bootstrap (using boot(...) function)

R <- 10**4 # Define number of replicates
mu <- mean(socio.matrix, na.rm = T) # Define mean mu
srg.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=mu)
save(srg.boot,file='srg_boot')
#load(file.choose())

# faccio i grafici che mi servono 
data <- as.data.frame(srg.boot$t)


p1<- ggplot(data = data, aes(x=data[,1]))+ 
        geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(1,17)+
        geom_vline( xintercept =  srg.boot$t0[1], color ='red')+
        xlab('In-degree std error')
p2<- ggplot(data = data, aes(x=data[,2]))+ 
        geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(1,15)+
        geom_vline( xintercept =  srg.boot$t0[2], color ='red')+
        xlab('Out-degree std error')
p3<- ggplot(data = data, aes(x=data[,3]))+ 
        geom_histogram(binwidth = 1,fill= 'Royalblue')+xlim(0,700)+
        geom_vline( xintercept =  srg.boot$t0[3], color ='red')+
        xlab('Dyad dependency')

grid.arrange(
  grobs = list(p1,p2,p3),
  widths = c(1,1, 1, 1),
  layout_matrix = rbind(c(1,1, 2, 2),
                        c(NA, 3,3,NA))
)

# creo vettore di esplicative
rank25 <- c(rep(1,25),rep(0,220-25))
rank50 <- c(rep(0,25),rep(1,25),rep(0,220-50))
rank100 <- c( rep(0,50), rep(1,50), rep(0,220-100))
# 220 non c'è per evitare la trappola delle dummy

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


# New network object
ergm.net <- as.network(socio.matrix)
diag(socio.matrix) <- NA
set.vertex.attribute(ergm.net, 'is_usa', is_usa)
set.vertex.attribute(ergm.net, 'followers', followers)
set.vertex.attribute(ergm.net, 'friends', friends)
set.vertex.attribute(ergm.net, 'verified', verified)
set.vertex.attribute(ergm.net, 'language', language)
set.vertex.attribute(ergm.net, 'nposts', post_num)

# creo anche questi perchè vado meglio a capire che contrasti fare
set.vertex.attribute(ergm.net, 'rank25',rank25)
set.vertex.attribute(ergm.net, 'rank50', rank50)
set.vertex.attribute(ergm.net, 'rank100', rank100)
set.vertex.attribute(ergm.net, 'rank225', rankD)


# creo la statisitica test per gli ergm 
ergm.gen <- function(socio.matrix, fit) {
  # Compute new matrix
  out.matrix <- as.matrix(simulate(fit)[1:nrow(socio.matrix), 1:nrow(socio.matrix)])
  diag(out.matrix) <- NA
  # return computed matrix
  return(out.matrix)
}


# faccio la stessa cosa per il modello con effetti fissi (ANOVA)
#fit.ergm.anova <- ergm(ergm.net ~ edges + sender + receiver)
# qua avrei bisogno dei soliti tre grafici

#load(file.choose())

# FASTER ANOVA FOR BOOT
# ANOVA model

# bisogna classificare come fattori indivuduali sia la socialit? che l'attrativit?

# Initialize new sociomatrix
Y <- socio.matrix # Copy sociomatrix by value
diag(Y) <- NA # Set values on the diagonal to NA
row.matrix <- matrix(data=(1:nrow(Y)), nrow=nrow(Y), ncol=nrow(Y))
col.matrix <- t(row.matrix) 
y <- c(Y) # Vectorize sociomatrix
row.v <- c(row.matrix) # Vectorize rows matrix
col.v <- c(col.matrix) # vectorize columns matrix
# Fit logistic regression
fit.rce.cent <- glm(y ~  C(factor(row.v), sum) + C(factor(col.v), sum) , family=binomial)
# Check fit summary
summary(fit.rce.cent)



# Individual effects
mu.hat <- fit.rce.cent$coef[1] # Estimated mean
a.hat <- fit.rce.cent$coef[1 + 1:(nrow(Y) - 1)]
a.hat <- c(a.hat, -sum(a.hat))
b.hat <- fit.rce.cent$coef[nrow(Y) + 1:(nrow(Y) - 1)]
b.hat <- c(b.hat, -sum(b.hat)) 
# Compute estimated probabilities for every cell
mu.ij.mle <- mu.hat + outer(a.hat, b.hat, '+') # Mu ij maximum likelihood estimation 
p.mle <- exp(mu.ij.mle) / (1 + exp(mu.ij.mle))
diag(p.mle) <- NA


# Analysis of the inferential model through parameteric bootstrap
R <- 10**4 # Number of replicates
anova.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=rgm.gen, mle=p.mle)
save(anova.boot ,file='boot_anova')
save(fit.rce.cent, file = 'anova_fit')

data <- as.data.frame(anova.boot$t)
data[,3]

p1<- ggplot(data = data, aes(x=data[,1]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(15,18)+
  geom_vline( xintercept =  anova.boot$t0[1], color ='red')+
  xlab('In-degree std error')
p2<- ggplot(data = data, aes(x=data[,2]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(13,17)+
  geom_vline( xintercept =  anova.boot$t0[2], color ='red')+
  xlab('Out-degree std error')
p3<- ggplot(data = data, aes(x=data[,3]))+ 
  geom_histogram(binwidth = 1,fill= 'Royalblue')+xlim(0,700)+
  geom_vline( xintercept =  anova.boot$t0[3], color ='red')+
  xlab('Dyad dependency')

grid.arrange(
  grobs = list(p1,p2,p3),
  widths = c(1,1, 1, 1),
  layout_matrix = rbind(c(1,1, 2, 2),
                        c(NA, 3,3,NA))
)


# Faccio la stessa cosa per il modello migliore degli ergm

fit.ergm.final <- ergm(ergm.net ~ edges + mutual
                   + nodeocov('is_usa') + nodeicov('is_usa') + nodematch('is_usa')
                   + nodeocov('nposts') + nodeicov('nposts')
                   + nodeocov('friends') + nodeicov('friends')
                   + nodeocov('followers') + nodeicov('followers')
                   + nodeocov('verified') + nodeicov('verified') + nodematch('verified')
                   + nodeocov('rank25') + nodeicov('rank25')+nodematch('rank25', levels=1)
                   + nodeocov('rank50') + nodeicov('rank50')+nodematch('rank50', levels=1)
                   + nodeocov('rank100') + nodeicov('rank100')+nodematch('rank100', levels=1)
                   #+ nodematch('rank225', levels=1),
                   ,control = control.ergm(MCMLE.maxit = 30,MCMC.samplesize = 1024)
)
ergm.boot.final <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen = ergm.gen, mle=fit.ergm.final)
save(ergm.boot.final ,file='boot_ergm')
save(fit.ergm.final, file = 'ergm_fit')
# qua avrei bisogno dei  soliti tre grafici

#load(file.choose())
data <- as.data.frame(ergm.boot.final$t)


p1<- ggplot(data = data, aes(x=data[,1]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(12,16.5)+
  geom_vline( xintercept =  ergm.boot.final$t0[1], color ='red')+
  xlab('In-degree std error')
p2<- ggplot(data = data, aes(x=data[,2]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(2,14.5)+
  geom_vline( xintercept =  ergm.boot.final$t0[2], color ='red')+
  xlab('Out-degree std error')
p3<- ggplot(data = data, aes(x=data[,3]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(0.30,0.50)+
  geom_vline( xintercept =  ergm.boot.final$t0[3], color ='red')+
  xlab('Dyad dependency')

grid.arrange(
  grobs = list(p1,p2,p3),
  widths = c(1,1, 1, 1),
  layout_matrix = rbind(c(1,1, 2, 2),
                        c(NA, 3,3,NA))
)

#load(file.choose())
summary(fit.ergm.final)



##### PARTE CON MODELL0 AMEN
# matrice modello ad effetti casuali 


X <- cbind(rank25,rank50,rank100,is_usa,verified)

# Esplicative a livello di diadi ( solo interazione tra ranking)
XlegAA<- outer(rank25,rank25)
XlegBB<- outer(rank50,rank50)
XlegCC<- outer(rank100,rank100)
XlegAB <- outer(rank25,rank50)
XlegBA <- outer(rank50,rank25)
XlegAC <- outer(rank25,rank100)
XlegCA <- outer(rank100,rank25)
XlegBC <- outer(rank50,rank100)
XlegCB <- outer(rank100,rank50)

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
dimnames(Xleg)[[3]] <- list("int25-25","int50-50","int100-100",
                    "int25-50","int50-25","int25-100",
                    "int100-25","int50-100","int100-50")


# stimo il modello con le covariate
fit_AME2<-ame(socio.matrix,Xdyad=Xleg,Xr=X[,4:5],Xc=X, model="bin")
summary(fit_AME2)


save(fit_AME2, file='Amen models/AME2')

fit_AME2$YPM # queste in  sono probabilità a posteriori quindi se le passiamo come un vettore 
# possiamo calcolarci le statistiche solite

# Per simulare dal modello stimato abbiamo bisogno
# di utilizzare simY

sim_amen <- function(socio.matrix,fit)
{
  rho <- mean(fit$VC[,4])
  simY_bin(fit$EZ,rho)
}
# Bisogna usare questa funzione perchè considera anche la covarianza tra gli archi


R <- 10**4 # Define number of replicates
srrg.boot <- boot(data=socio.matrix, statistic=rce.stat, R=R, sim='parametric', ran.gen=sim_amen, mle=fit_AME2)
srrg.boot$t
save(srrg.boot, file='Amen models/srrg_boot')

# faccio i soliti 3 grafici per la bontà di adattamento 
data <- as.data.frame(srrg.boot$t)


p1<- ggplot(data = data, aes(x=data[,1]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(12,16.5)+
  geom_vline( xintercept =  srrg.boot$t0[1], color ='red')+
  xlab('In-degree std error')
p2<- ggplot(data = data, aes(x=data[,2]))+ 
  geom_histogram(binwidth = 0.01,fill= 'Royalblue')+xlim(2,14.5)+
  geom_vline( xintercept =  srrg.boot$t0[2], color ='red')+
  xlab('Out-degree std error')
p3<- ggplot(data = data, aes(x=data[,3]))+ 
  geom_histogram(binwidth = 1,fill= 'Royalblue')+xlim(400,800)+
  geom_vline( xintercept =  srrg.boot$t0[3], color ='red')+
  xlab('Dyad dependency')

grid.arrange(
  grobs = list(p1,p2,p3),
  widths = c(1,1, 1, 1),
  layout_matrix = rbind(c(1,1, 2, 2),
                        c(NA, 3,3,NA))
)


#load(file.choose())

# 