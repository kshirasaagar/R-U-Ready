#Power of Apply Functions

#Create an array of 3 rows, 4 columns and 2 dimensions
c = array(1:24,c(3,4,2))

#How do you access the first row?
c[1,,]
#How do you access the first column?
c[,1,]
#How do you access the first dimension?
c[,,1]

#Find sum of all columns overall
#Use colSums()

#Find sum of all columns BY each dimension
#Ask them to write it using for loops - it'll take atleast two/three nested for loops

#Now the same can be done using apply functions
col_sum = apply(c,c(3,2),sum)
#Applying the sum function across dimensions(3) and across columns(2)
#1- ROWS, 2 - COLUMNS, 3 - DIMENSION etc.