# carico i dati
load('C:/GitHub/smhdd_network/socioMatrix.RData')

library(igraph)

net2 = graph_from_adjacency_matrix(socio.matrix, mode = 'directed', weighted = NULL, diag=F)
str(net)


### Analisi dei dati in quanto rete
E(net2)
V(net2)

edge_density(net2)


vuoti = which(degree(net2) < 1)
vuoti
net = delete.vertices(net2,vuoti)

# sociabilità
outDegree <- apply(socio.matrix, 1,sum)
hist(outDegree, col = 'gray')
head(sort(outDegree, decreasing = T),10)
# le unità con valori più alti sembrano essere università
# meno conosciute

# leader 
inDegree <- apply(socio.matrix, 2,sum)
hist(inDegree, col = 'gray')
head(sort(inDegree, decreasing = T),10)
# le università più seguite sono nache le più prestigiose

#?shortest_paths
shortest_paths(net2, from = 'Harvard')
shortest_paths(net2, from = 'UCT_news')
# sono shortest path che tengono conto della direzionalità


#altre misure viste a lezione
hist(sqrt(betweenness(net2)), breaks = 20, col = 'lavender')
diameter(net2)

plot(net2,edge.arrow.size = 0.2, vertex.label=NA)
plot(net2, layout=layout.fruchterman.reingold, vertex.label=NA)

# Pacchetto più carino per la visualizzazione
library(ggraph)

#ggraph(net) + geom_edge_link() +   # add edges to the plot
#              geom_node_point()    # add nodes to the plot                          

X11()
ggraph(net, layout="lgl") +
  geom_edge_fan(color="gray50", width=0.8, alpha=0.5) +geom_node_point()+ 
  theme_void()

## Inferenza ####

# Modello SRG
# per la valutazione del modello usiamo un bootstrap parametrico 
# come fatto nelle slides

library(boot)

# scrivo la funzione per calcolare lo std error di outDegree e inDegree
stat <- function(data)
{
    out.deg <-  apply(data, 1,sum, na.rm= T)
    in.deg <- apply(data, 2,sum, na.rm = T)
    out.mu <- mean(out.deg)
    in.mu <- mean(in.deg)
    out.sq <- sqrt(sum((out.deg-out.mu)**2)/length(data[,1]))
    in.sq <- sqrt(sum((in.deg-in.mu)**2)/length(data[,1]))
    c(in.sq,out.sq) 
}

stat(socio.matrix)

# scrivo la funzione per generare la rete secondo RGM
# in pratica genero 199*199 bernuolli indipendenti di probabilità la densità della rete
# e le salvo in una matrice

ran.gen1 <- function(data,param)
{
  out <- matrix(data=rbinom(n=length(data[,1])**2,size=1, prob = param),ncol =length(data[,1]) )
  diag(out) <- NA
  out
}


# Faccio il bootstrap parametrico utilizzando la funzione boot
R <- 10**4
mu <- mean(socio.matrix)
boot.SRG <- boot( data= socio.matrix, statistic = stat, R=R, sim = "parametric",ran.gen = ran.gen1,mle=mu )
save(boot.SRG,file = 'SRG_bootstrap')

# analizzo la distribuzione delle due statistiche 

# distribuzione dello str dell'inDegree sotto l'ipotesi di SRG
head(boot.SRG$t)  
hist(boot.SRG$t[,1], nclass = 50, xlim = c(2,16))
abline(v=boot.SRG$t0[1],col=2)

# distribuzione dello str dell'outDegree sotto l'ipotesi di SRG
hist(boot.SRG$t[,2], nclass = 50, xlim = c(2,16))
abline(v=boot.SRG$t0[2],col=2)
mean(boot.SRG$t[,2]>boot.SRG$t0[2]) # per quello che vale

# Commento: I dati smentiscono in modo netto l'ipotesi che il processo 
# generatore sia un SRG. I nodi mostrano molta più eterogeneità sia in inDegree
# che in outDegree.

## Modello ANOVA ###

# bisogna classificare come fattori indivuduali sia la socialità che l'attratività 
# pongo i valori sulla diagonale come NA
Y <- socio.matrix
diag(Y) <- NA


# creo i fattori latenti 
Ridx<-matrix((1:nrow(Y)),nrow(Y),nrow(Y))
Cidx<-t(Ridx) 

Ridx[1:4,1:4]
Cidx[1:4,1:4]

Y[1:4,1:4] 

# vettorizzo tutte le quantità
y <- c(Y)
ridx <- c(Ridx)
cidx <- c(Cidx)
ridx[1:20]

#stimo la regressione logistica
fit<-glm( y ~ factor(ridx) + factor(cidx), family=binomial) 
summary(fit)  
summary(glm(y~1)) # RCE ha troppo adattamento
save(fit,file="RCE_nocent")

# stimo il modello in cui i fattori individuali sono in relazione allo scostamento
# dalla media
# C() funzione che crea i contrasti
fit2<-glm( y ~  C(factor(ridx),sum) + C(factor(cidx),sum) , family=binomial)
summary(fit2)  
save(fit2,file="RCE_cent")


# Effetti individuali
mu.hat<-fit2$coef[1]
a.hat<- fit2$coef[1+1:(nrow(Y)-1)]   ; a.hat<-c(a.hat,-sum(a.hat) ) 
b.hat<- fit2$coef[nrow(Y)+1:(nrow(Y)-1)]   ; b.hat<-c(b.hat,-sum(b.hat) ) 
summary(a.hat)
summary(b.hat)
# Cacolo le probabilità previste per ogni cella
muij.mle<- mu.hat+ outer(a.hat,b.hat,"+")
p.mle<-exp(muij.mle)/(1+exp(muij.mle)) ; diag(p.mle)<-NA

# Ora analizziamo attraverso un bootstrap parametrico la qualità del 
# modello inferenziale
library(boot)
stat <- function(data)
{
  # std error outdegree e indegree
  out.deg <-  apply(data, 1,sum,na.rm=T)
  in.deg <- apply(data, 2,sum,na.rm=T)
  out.mu <- mean(out.deg)
  in.mu <- mean(in.deg)
  out.sq <- sqrt(sum((out.deg-out.mu)**2)/length(data[,1]))
  in.sq <- sqrt(sum((in.deg-in.mu)**2)/length(data[,1]))
  # diadi mutue. Utile per valutare l'indipendenza delle y 
  conc <- (data==t(data))
  mu.dy <- sum(conc[data==1], na.rm = T)/2
  c(in.sq,out.sq,mu.dy) 
}

stat(Y)
# controllo che rangen1 funzioni anche per questo modello
ran.gen1(Y,p.mle)
# tutto ok

# Faccio il bootstrap parametrico utilizzando la funzione boot
R <- 10**4
boot.RCE <- boot( data= socio.matrix, statistic = stat, R=R, sim = "parametric",ran.gen = ran.gen1,mle=p.mle )
save(boot.RCE, file='RCE_bootstrap')
# i warnigns erano previsti
# analizzo la distribuzione delle due statistiche 

# distribuzione dello stdr dell'inDegree sotto l'ipotesi di RCE
head(boot.RCE$t)  
hist(boot.RCE$t[,1], nclass = 50)
abline(v=boot.RCE$t0[1],col=2)
# p-value bilaterale
2*mean(boot.RCE$t[,1]< boot.RCE$t0[1])

# distribuzione dello stdr dell'outDegree sotto l'ipotesi di RCE
hist(boot.RCE$t[,2], nclass = 50)
abline(v=boot.RCE$t0[2],col=2)
# p-value bilaterale
2*mean(boot.RCE$t[,2]< boot.RCE$t0[2])

# distribuzione del numero di diadi mutue sotto l'ipotesi di RCE
hist(boot.RCE$t[,3], nclass = 50, xlim = c(100,600))
abline(v=boot.RCE$t0[3],col=2)
# Commento: sebbene l'outdegree e l'indegree siano modellati molto bene
# la distribuzione delle diadi comuni evidenzia come ci siano degli effetti
# di dipendenza di cui non stiamo tenendo conto.

### Modello Ergm più complesso ####
# carico le esplicative

X <- read.csv('C:/GitHub/smhdd_network/data/users.csv', header = T, sep=',')
head(X)
X$location

# mi creo l'esplicativa 'usa' che indica le università americane
usa <- c(1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1,
         1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1,
         1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
         0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
         1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0,
         0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0,
         0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1,
         0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
         0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0,
         0, 0, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1,
         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1) # 196 messo 1 e tolto uno zero in fondo dopo aver visto socio.matrix 
rank <- (c( rep('top15',15)  , rep('top50',35)  ,rep('top100',50)   , rep('top200',99)  ))      
cbind(rownames(socio.matrix), usa)         
#rankA <- c(rep(1,15),rep(0,184))
#rankB <- c(rep(0,15),rep(1,35),rep(0,199-50))
#rankC <- c( rep(0,50), rep(1,50), rep(0,99))
#rankD <- c( rep(0,100),rep(1,99))

# creo un oggetto network
library(ergm)
net <- as.network(socio.matrix)
diag(socio.matrix)<- NA
set.vertex.attribute(net,"state",usa)
set.vertex.attribute(net,"ranking",rank)
set.vertex.attribute(net,"followers",X$followers_count[1:199])
set.vertex.attribute(net,"friends",X$friends_count[1:199])
set.vertex.attribute(net,"verified",X$verified[1:199])


#set.vertex.attribute(net,"rankA",rankA)
#set.vertex.attribute(net,"rankB",rankB)
#set.vertex.attribute(net,"rankC",rankC)
#set.vertex.attribute(net,"rankD",rankD)


# modello equavalente ad una regressione logistica ( indipendenza y)
fit0 <- ergm(net~edges)
summary(fit0) #AIC: 19667
fit.ergm<-ergm( net ~ edges + nodeocov("state") + nodeicov("state") 
                +  nodeofactor("ranking",levels=c('top15','top50','top100')) 
                + nodeifactor("ranking",levels=c('top15','top50','top100')) + nodematch("state")+nodematch("ranking") )
summary(fit.ergm) # 17886  AIC ancora molto alto

# effetti RCE fatti con ergm
fit.ergm2<-ergm( net~edges + sender + receiver)
summary(fit.ergm2) # AIC 26755

# ho il dubbio che l'aic venga fatto su verosimiglianze con fattori moltiplicativi diversi
# controllo se i coefficienti sono uguali al modello ottenuto con glm
summary(fit)
cbind(fit$coefficients,fit.ergm2$coef) # si, i modelli sono gli stessi
# quindi va usato solo un pacchetto

# Provo a migliorare il modello
fit.ergm3<-ergm( net~edges + sender)
summary(fit.ergm3) # AIC 25338
fit.ergm4<-ergm( net~edges + receiver)
summary(fit.ergm4) # AIC 22259
# aggiungo outdegree e indegree
fit.ergm5<-ergm( net~edges+mutual)
summary(fit.ergm5) # AIC 18444
fit.ergm6<-ergm( net~edges + mutual+ nodeocov("state") + nodeicov("state") +
                   #nodeocov("ranking") + nodeicov("ranking") 
                   + nodematch("state")+
                   diff('ranking', pow=1, sign.action='identity'))#absdiff('ranking', pow=1)
summary(fit.ergm6) # AIC 16753
save(fit.ergm6, file='ergm_fit')
fit.ergm7<-ergm( net~ edges+mutual
                   + nodeocov("state") + nodeicov("state")+ nodematch("state")
                   + nodeocov("friends") + nodeicov("friends")
                   + nodeocov("followers") + nodeicov("followers")
                   + nodeocov("verified") + nodeicov("verified")+ nodematch("verified")
                   + nodeofactor("ranking",levels=c('top15','top50','top100')) 
                   + nodeifactor("ranking",levels=c('top15','top50','top100'))
                   +nodemix("ranking",levels=c('top15','top50','top100')),
                   control = control.ergm(MCMLE.maxit = 30)
                   )
                 
summary(fit.ergm7)
save(fit.ergm7, file='ergm_fit2')

outD <- apply(socio.matrix,1,sum,na.rm=T)
inD <- apply(socio.matrix,2,sum,na.rm=T)

# cerco di vedere se c'è una relazione tra posizione e outdeg e indeg
# perchè al momento non si riesce a spiegare la variabilità
plot(seq(1:199),outD) # non si vede molto forse è per questo che non riusciamo a spiegare
                      # la variabilità in uscita
plot(seq(1:199),inD)
plot(X$friends_count[1:199],inD)

plot(density(outD))
quantile(outD, probs =c(0.25,0.5,0.75,0.90) )
# stampo le università che hanno un outdegree che supera il 75% percentile
outlier <-(outD>29) 
outlier

# commento outdegree: è evidente come vi sia circa un 25% di ossservazioni anomale
# va indagato il perchè

fit.ergm8<-ergm( net~ edges+mutual+sender(nodes=outlier)
                 + nodeocov("state") + nodeicov("state")+ nodematch("state")
                 + nodeocov("friends") + nodeicov("friends")
                 + nodeocov("followers") + nodeicov("followers")
                 + nodeocov("verified") + nodeicov("verified")+ nodematch("verified")
                 + nodeofactor("ranking",levels=c('top15','top50','top100')) 
                 + nodeifactor("ranking",levels=c('top15','top50','top100'))
                 +nodemix("ranking",levels=c('top15','top50','top100')),
                 control = control.ergm(MCMLE.maxit = 60)
)
summary(fit.ergm8)

# Commento del modello
# Questo modello sembra il migliore e offre anche delle interpretazioni interessanti
# le università tendono a seguire di più se hanno un renking scarso mentre vengono seguite
# di più le università più famose. Stesso discorso tra americane e non
# scrivo la funzione per generare dal modello stimato 
# NB data=socio.matrix fit invece deve essere l'ergm che vogliomo testare
ran.gen2 <- function(data,fit)
{
  require(ergm)
  out <- as.matrix(simulate(fit)[1:length(data[,1]), 1:length(data[,1])])
  diag(out) <- NA
  out
}
apply(ran.gen2(socio.matrix,fit.ergm7),1,sum,na.rm=T)

# Faccio il bootstrap parametrico utilizzando la funzione boot
library(boot)
R <- 10
boot.ERGM <- boot( data= socio.matrix, statistic = stat, R=R, sim = "parametric",ran.gen = ran.gen2,mle=fit.ergm7 )
boot.ERGM$t
save(boot.ERGM, file='ERGM_bootstrap')
# i warnigns erano previsti
# analizzo la distribuzione delle due statistiche 

# distribuzione dello stdr dell'inDegree sotto l'ipotesi di RCE
head(boot.ERGM$t)  
hist(boot.ERGM$t[,1], nclass = 50)
abline(v=boot.ERGM$t0[1],col=2)
# p-value bilaterale
2*mean(boot.ERGM$t[,1]< boot.ERGM$t0[1])

# distribuzione dello stdr dell'outDegree sotto l'ipotesi di RCE
hist(boot.ERGM$t[,2], nclass = 50)
abline(v=boot.ERGM$t0[2],col=2)
# p-value bilaterale
2*mean(boot.ERGM$t[,2]< boot.ERGM$t0[2])

# distribuzione del numero di diadi mutue sotto l'ipotesi di RCE
hist(boot.ERGM$t[,3], nclass = 50, xlim = c(100,600))
abline(v=boot.ERGM$t0[3],col=2)
# Commento: 
# In realtà il risultato è ancora abbastanza insoddisfacente in outdegree e indegree