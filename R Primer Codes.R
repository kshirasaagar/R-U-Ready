##-----------------------------------------------------------
## Program: A Course in R.R
## Purpose: A basic introduction to R
## Author : Indrajit Sen Gupta
## Date   : January 2012 
##-----------------------------------------------------------

##-----------------------------------------------------------
## Load required libraries
##-----------------------------------------------------------
library(MASS)

##-----------------------------------------------------------
## Set working directory
##-----------------------------------------------------------
setwd("D:/Work/_Documents/Training/R Training (Dec 2011)")

##-----------------------------------------------------------
## A simple vector in R. The letter "c" stands for concatenate
##-----------------------------------------------------------
x <- c(1,2,4)

x

q <- c(x,x,8)

q

##-----------------------------------------------------------
## The standard assignment operator in R is "<-".
## "=" can also be used but is usually discouraged
##-----------------------------------------------------------

##-----------------------------------------------------------
## Let us access the third element in x via [ ]. 
##-----------------------------------------------------------

x[3]

##-----------------------------------------------------------
## Subsetting a vector
##-----------------------------------------------------------

q[3:7]

##-----------------------------------------------------------
## Calculate mean and std of q and assign it to a variable
##-----------------------------------------------------------

q.mean <- mean(q)
q.std <- sd(q)

##-----------------------------------------------------------
## Find the reminder when 38 is divided by 7
##-----------------------------------------------------------

38 %% 7

##-----------------------------------------------------------
## Find out what variables are there in R's memory
##-----------------------------------------------------------

ls()

##-----------------------------------------------------------
## Delete x from the memory
##-----------------------------------------------------------

rm(x)
ls()

##-----------------------------------------------------------
## Quitting R
##-----------------------------------------------------------

q()

##-----------------------------------------------------------
## Find out what internal datasets exist in R
##-----------------------------------------------------------

data()

data(package = .packages(all.available = TRUE))

##-----------------------------------------------------------
## Use the internal dataset Nile to plot a histogram
##-----------------------------------------------------------

Nile

hist(Nile, col = "red")

##-----------------------------------------------------------
## Help on the function hist
##-----------------------------------------------------------

help(hist)

##-----------------------------------------------------------
## Write a function in R to count the number of odd integers
##-----------------------------------------------------------

oddcount <- function(x) {
  k <- 0  ## Assign the value 0 to k
  for (n in x) {  ## Start a FOR loop for every element in x

    if (n %% 2 == 1) k <- k + 1  ## %% is a modulo operator
  }
  return(k)
}

oddcount(c(1,2,3,5,7,9,14))

##-----------------------------------------------------------
## A different way of writing the above function
##-----------------------------------------------------------

oddcount2 <- function(x) {
  k <- 0  ## Assign the value 0 to k
  for (i in 1:length(x)) {  ## The length function gives number of elements in x
    if (x[i] %% 2 == 1) k <- k + 1  ## %% is a modulo operator
  }
  return(k)
}
ls()

##-----------------------------------------------------------
## Some data structure examples in R
##-----------------------------------------------------------

y <- "Indrajit"  ## Character
mode(y)

mat <- matrix(data = c(1:9), nrow = 3, byrow = TRUE)  ## Matrix
mat

mat[1:3,1:2]  ## Get the first two columns and top three rows of the matrix
mat[1,]  ## Get the first column of the matrix

mat2 <- matrix(data = c(10:18), nrow = 3, byrow = FALSE)

mat %*% mat2  ## Matrix multiplication in R

mylist <- list(name = "Indrajit", age = 31)  ## Generate a list with two data types
mylist

mylist$name

hm <- hist(Nile, col = "red")  ## An object of class histogram
summary(hm)
hm

dm <- data.frame(list(kids = c("Jack", "Jill"), ages = c(12,10)))  ## A data frame
dm
dm$ages

##-----------------------------------------------------------
## Assignment: Write a function to calculate median absolute 
## deviation from median without using the median() function 
## and use mad() function to validate your results
##-----------------------------------------------------------

##-----------------------------------------------------------
## Some useful functions in R: all(), any()
##-----------------------------------------------------------

##-----------------------------------------------------------
## Vector In, Vector Out
##-----------------------------------------------------------

u <- c(5,2,8)
v <- c(1,3,9)
u > v
all(u > 4)
any(u > 7)

##-----------------------------------------------------------
## Apply error handling in functions wherever possible
##-----------------------------------------------------------

f1 <- function(x,c) return((x+c)^2)
f1(1:3,1:3)

f2 <- function(x,c) {
  if (length(c) != 1) {
      stop("vector c not allowed")
  }
  return((x+c)^2)
}
f2(1:4,2)

##-----------------------------------------------------------
## Simplify Apply function sapply(): Applies function to each 
## element and then converts the result to a matrix form
##-----------------------------------------------------------

f3 <- function(z) return(c(z,z^2))
t(sapply(1:8, f3))

##-----------------------------------------------------------
## NULL and NA values in R
##-----------------------------------------------------------
x <- NULL
x <- c(88, NA, 12, 168, 13)

x

mean(x)

mean(x, na.rm = TRUE)

x1 <- c(0, 2, 4)
1/x1
is.na(x) ## Check whether an object is NA

x <- c(88, NULL, 12, 168, 13)

x

##-----------------------------------------------------------
## Use of NULL in building up a vector
##-----------------------------------------------------------

rm(list = ls())  ## Delete all objects from the library

z <- NULL

for (i in 1:10) if (i %% 2 == 0) z <- c(z,i) ## A bad way to achieve this result however :-(

z

##-----------------------------------------------------------
## Subsetting vectors based on boolean logic
##-----------------------------------------------------------

z <- c(5, 2, -3, 8)
z[1:4]
z*z > 8

w <- z[z * z > 8]  ## What is our intent here?

w  ## Evaluating the expression z*z > 8 gives us a vector of Boolean values

z[z < 0] <- 0  ## Replace all negative values with 0

z

##-----------------------------------------------------------
## Subsetting vectors using the subset function 
## when NA is present
##-----------------------------------------------------------

x <- c(6, 1:3, NA, 12)
x

x[x > 5] 

subset(x,x > 5)

##-----------------------------------------------------------
## Identifying the indices which satisfy a condition using 
## the which() function
##-----------------------------------------------------------

which(x > 5)
which(is.na(x))

##-----------------------------------------------------------
## Applying functions to rows/columns of a matrix using apply()
## Usage: apply(x, MARGIN, FUN, ARG)
## x: data matrix on which to apply the function
## MARGIN: 1 indicates rows, 2 indicates columns
## FUN: function to be applied
## ARG: optional set of arguments
##-----------------------------------------------------------

z <- matrix(1:6, ncol = 2)

apply(z, 1, mean)

##-----------------------------------------------------------
## Differences between apply, lapply, sapply, tapply
##-----------------------------------------------------------

##-----------------------------------------------------------
## apply: Apply a function to the rows and columns of a
## matrix
##-----------------------------------------------------------

#Two dimensional matrix 
M <- matrix(runif(16),4,4)  

# apply min to rows 

apply(X = M, MARGIN = 1, FUN = min) 

# apply max to columns 

apply(M, 2, max)

##-----------------------------------------------------------
## lapply: Apply a function to each element of a list and
## in turn get a list back
##-----------------------------------------------------------

x <- list(a = 1, b = 1:3, c = 10:100, d = "abcd")

lapply(x, FUN = length)

##-----------------------------------------------------------
## sapply: Apply a function to each element of a list and
## get the output in form of a vector
##-----------------------------------------------------------

sapply(x, FUN = length)

f1 <- function(x) { rnorm(n = 3, mean = x)}

sapply(1:5, FUN = f1)


##-----------------------------------------------------------
## tapply: Apply a function to subsets of a vector where the
## subsets are defined by some other vector usually a factor
##-----------------------------------------------------------

x <- 1:20

y <- factor(rep(letters[1:5], each = 4))

tapply(x, INDEX = y, FUN = sum)

##-----------------------------------------------------------
## Appending datasets: rbind() and cbind()
##-----------------------------------------------------------

z

one <- c(1,2)

cbind(one, z)

two <- rep(2,2)

rbind(z, two)

##-----------------------------------------------------------
## Merging dataframes on common key
##-----------------------------------------------------------

d1 <- data.frame(kids = c("Jack", "Jill", "Jillian", "John"), 
  states = c("CA","MA","MA","HI"))

d2 <- data.frame(ages = c(10, 7, 12), 
                 kids = c("Jill","Lillian", "Jack"))

merge(d1, d2, all = TRUE) # Outer join
merge(d1, d2, all.x = TRUE) # Left join
merge(d1, d2, all.y = TRUE) # Right join
merge(d1, d2, all = FALSE) # Inner join

##-----------------------------------------------------------
## Merging dataframes on uncommon key
##-----------------------------------------------------------

d2 <- data.frame(ages = c(10, 7, 12), 
                 pals = c("Jill","Lillian", "Jack"))

merge(d1, d2, by.x = "kids", by.y = "pals")

##-----------------------------------------------------------
## Reading external files into R: read.table, read.csv
##-----------------------------------------------------------

rm(list = ls())

sales <- read.table("sales.csv", header = TRUE, sep= ",")

str(sales)

head(sales)

sink("myoutput.txt") # Direct all output to external file

sink() # Reset output

##-----------------------------------------------------------
## Writing to external files from R: write.table
##-----------------------------------------------------------

write.table(sales, file = "sales_out.csv", append = FALSE, 
sep = ",", col.names = TRUE, row.names = FALSE)

cat("Indrajit\n",file = "u.txt")
cat("Sengupta\n", file = "u.txt", append = TRUE)

##-----------------------------------------------------------
## Working with factors in R
##-----------------------------------------------------------

incomes <- read.table("income.csv", header= TRUE, sep = ",")

incomes

str(incomes)

attach(incomes)  ## Allow access to the variables inside the dataframe

tapply(Income, Gender, mean)

incomes$Over50 <- ifelse(Age > 50,1,0) ## Create a dummy variable

tapply(Income, list(incomes$Gender, incomes$Over50), mean)

str(incomes)

incomes$Over50 <- factor(incomes$Over50)

str(incomes)

summary(incomes$Over50)

##-----------------------------------------------------------
## SQL in R
##-----------------------------------------------------------

ls()
install.packages("sqldf")
library(sqldf)

sqldf("select * from incomes limit 3")

highinc <- sqldf("select * 
  from incomes
  where Income > 80000")

sqldf("select Gender, max(Income) from 
      incomes group by Gender")

##-----------------------------------------------------------
## Connecting to Databases in R
##-----------------------------------------------------------

library(RODBC)

ch <- odbcConnect("integration")  ## Create connection to ODBC

##-----------------------------------------------------------
## Get a list of tables in the database
##-----------------------------------------------------------

table.list <- sqlTables(ch)

table.list[,c(3,4)]  
## Get the table name and table type columns

##-----------------------------------------------------------
## Extract tables from ODBC into R
##-----------------------------------------------------------

customer <- sqlFetch(ch,"CustomerMaster", max= 20)

str(customer)
head(customer)

##-----------------------------------------------------------
## Query databases in R
##-----------------------------------------------------------

customer2 <- sqlQuery(ch, "select Customer_Name, City from CustomerMaster",
max = 50)

customer2

##-----------------------------------------------------------
## Reading SAS datasets in R
##-----------------------------------------------------------

library(sas7bdat)

trans <- read.sas7bdat("transaction.sas7bdat")

head(trans)

##-----------------------------------------------------------
## Graphs in R
##-----------------------------------------------------------

sales <- read.table("sales.csv", header = TRUE, sep= ",")

newsales=sales[1:500,c("quantity","sales","visits","ind","basket")]

newsales

attach(newsales)

plot(newsales$quantity,newsales$basket)

boxplot(newsales$sales)

plot(quantity, basket, xlab = "quantity",ylab= "basket")

plot(quantity, basket, xlab = "Quantity of Items Purchased",ylab="Value of Basket",
xlim = c(0,500), ylim=c(0,1200), pch="+", col = "blue", bty = "L", cex=0.5)

##-----------------------------------------------------------
## Output graphics to PDF
##-----------------------------------------------------------

pdf("fruitpie2.pdf")

fruits <- c(41,59,78,23,34)

names(fruits) <- c("grape", "chickoo", "mango", "papaya", "orange")

pie(fruits,col=c("black","brown","yellow","green","orange"),
main="Pie Chart for Fruit Juice Consumption")

dev.off()

##-----------------------------------------------------------
## Two plots in the same graph
##-----------------------------------------------------------

theta = seq(10,20,0.1)

thetapower = 1 - pnorm(15,theta, 3)

plot(theta, thetapower, type = "l", col="red",xlab = "theta", ylab = "power")

thetapower = 1 - pnorm(13, theta, 3)

points(theta, thetapower, type = "l", lty = "dashed", col="blue")

legend(10,0.8, legend = c("x=15", "x=13"), lty = c("solid", "dashed"))

title("Power Function for Z Test for Normal Mean theta sd=3")

rm(theta)
rm(thetapower)

##----------------------------------------------------------------------------------------------
## 
##----------------------------------------------------------------------------------------------

##----------------------------------------------------------------------------------------------
## 
##----------------------------------------------------------------------------------------------
