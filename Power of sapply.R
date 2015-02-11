
a <- c(1,2,3,4)
b <- c(6,7,8,8)
c <- c(123,34,234,213)
d <- c(5,3,32,2)

df <- data.frame(a,b,c,d)

co <- function(i,j) (mean(df[,i],df[,j]))

row = 1:4
col = 1:4

sapply(row, function(row) co(row,col))

p1 <- matrix(nrow = 4, ncol = 4)
for(i in 1:4)
{
  for(j in 1:4)
  {
    p1[i,j] <- co(i,j)
  }
}