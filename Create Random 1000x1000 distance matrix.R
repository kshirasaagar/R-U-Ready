
#Create Random 1000x1000 distance matrix

m1 <- matrix((runif(100000)*100),1000,1000)

for (i in 1:1000)
{
  for (j in 1:1000)
  {
    if(i != j) m1[i,j] = m1[j,i]
    else m1[i,j] = 0    
  }
}