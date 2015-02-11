##-----------------------------------------------------------
## Program: A Course in R Part II.R
## Purpose: A basic introduction to R
## Date   : June 2012 
##-----------------------------------------------------------

##-----------------------------------------------------------
## Set working directory
##-----------------------------------------------------------
setwd("D:/Work/_Documents/Training/R Training (Dec 2011)/WIP")

##-----------------------------------------------------------
## Data frame object
##-----------------------------------------------------------

v1 <- c(1:3)

m1 <- matrix(c(2:10), nrow = 3)

dfName <- data.frame(vec = v1, mat = m1)

dfName

mode(dfName)

class(dfName)

##-----------------------------------------------------------
## Reading an external file into R
##-----------------------------------------------------------

income <- read.table("income.csv", 
                     header = TRUE, 
                     sep = ",",
    colClasses = c("factor", "integer", "integer"))

head(income, n =7)

str(income)

colnames(income)

##-----------------------------------------------------------
## Accessing Rows and Columns
##-----------------------------------------------------------

income[3,]

income[,2]

income[,c("Age", "Gender")]

income$Age

##-----------------------------------------------------------
## Sorting on the Age column
##-----------------------------------------------------------

order(income$Age, income$Income)

income[order(income$Age),]

sortedInc <- income[order(income$Age, income$Income),]

##-----------------------------------------------------------
## Search (which function did we use earlier to search?)
##-----------------------------------------------------------

which(income$Gender == "M")

# This index vector may be used to obtain a subset of the dataset directly.
income[which(income$Gender == "M" &
  income$Age >= 50),]

# First subset by Males, then
# sort by Age (in one step)
income[which(income$Gender == "M" &
  order(income$Age)),]

# all and any

all(income$Gender == "M")  # returns FALSE
any(income$Gender == "M")  # returns TRUE

##-----------------------------------------------------------
## Aggregate - break down the data into subsets, compute 
## summary statistics for each, and returns the result in a 
## convenient form
##-----------------------------------------------------------

# Find the mean of Age and Income grouped by Gender.

aggregate(x = income[,c("Age","Income")], 
          by = income["Gender"], 
          FUN = max)


##-----------------------------------------------------------
## Merge - Merge two data frames by common columns or row names, 
## or do other versions of database join operations.
##-----------------------------------------------------------

##-----------------------------------------------------------
## Merging dataframes on common key
##-----------------------------------------------------------

d1 <- data.frame(
  kids = c("Jack", "Jill", "Jillian", "John"), 
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
## Merge the two dataframes (Left as an exercise)
##-----------------------------------------------------------

cust <- read.csv("CustomerMaster.csv", header = TRUE)
trans <- read.csv("TransactionMaster.csv", header = TRUE)

# Check the datasets by typing head(dataFrameName)
