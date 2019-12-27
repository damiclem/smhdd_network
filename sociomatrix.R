#  carico i dati

users <- read.csv('C:/Users/pozza/Documents/GitHub/smhdd_network/data/users.csv', header = T)
friendship<- read.csv('C:/Users/pozza/Documents/GitHub/smhdd_network/data/friendship.csv', header = T)

# creo la matrice
uni <- users$screen_name 
str(uni)

# creo la matrice vuota
socio.matrix <- matrix(0,nrow = length(uni),ncol = length(uni))

# sulla colonne mettiamo le università seguite
for( i in 1:length(uni))
{
  for(j in 1:length(uni))
  {
    if(length(union(friendship$screen_name.to[friendship$screen_name.from==uni[i]],uni[j]))==
       length(friendship$screen_name.to[friendship$screen_name.from==uni[i]]))
    {
        socio.matrix[i,j] <- 1
    }
  }
}

colnames(socio.matrix)<- uni
rownames(socio.matrix)<- uni
save(socio.matrix,file='C:/Users/pozza/Documents/GitHub/smhdd_network/socioMatrix.RData')

