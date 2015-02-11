##-----------------------------------------------------------
## Program: A Course in R Part III.R
## Purpose: A basic introduction to R
## Date   : June 2012 
##-----------------------------------------------------------

##-----------------------------------------------------------
## Set working directory
##-----------------------------------------------------------
setwd("D:/Work/_Documents/Training/R Training (Dec 2011)/WIP")

##-----------------------------------------------------------
## USING ifelse
##-----------------------------------------------------------

income <- read.table("income.csv", 
                     header = TRUE, 
                     sep = ",",
                     colClasses = c("factor", "integer", "integer"))

income$category <- ifelse(income$Age < 35, "Young", "Old")

s <- 3
switch(s, mean(income$Income), sum(income$Income))

ss <- "mean"
switch(ss, mean = mean(income$Income), sum = sum(income$Income))

for ( i in 1:5) {
  print(i)
}

i <- 0
while (i < 10) {
  print(i)
  i <- i + 1
}

##-----------------------------------------------------------
.,l## Write a function in R to count the number of odd integers
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

