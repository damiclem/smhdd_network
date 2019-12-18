# Scaletta: 1) prendere i nomi da users 



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
save(socio.matrix,file='C:/Users/pozza/OneDrive/Documenti/Francesco/High dimentional data/Progetto/smhdd_network-master/data/socioMatrix.RData')

