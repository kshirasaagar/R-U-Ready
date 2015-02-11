##-----------------------------------------------------------
## Program: A Course in R.R
## Purpose: A basic introduction to R
## Date   : June 2012 
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
## Basic Calculation
##-----------------------------------------------------------

1:50

##-----------------------------------------------------------
## The standard assignment operator in R is "<-".
## "=" can also be used but is usually discouraged
##-----------------------------------------------------------

x<-2
y<-3
x+y


##-----------------------------------------------------------
## A simple vector in R. The letter "c" stands for concatenate
##-----------------------------------------------------------

workshop <- c(1,2,1,2,1,2,1,2)

##-----------------------------------------------------------
## Calculate mean and variance of workshop and assign it to a variable
##-----------------------------------------------------------

mn.workshop <-mean(workshop)
var.workshop<-var(workshop)

mn.workshop
var.workshop

##-----------------------------------------------------------
## Check if workshop is a vector
##-----------------------------------------------------------

is.vector(workshop)

##-----------------------------------------------------------
## Generaing Sequence by using function seq() 
##-----------------------------------------------------------

## simple sequence

1:12
10:1

## specify increments

seq(4, 6, .25)

## use rep() to repeat certain (set) values(s)

rep("MSU", 5) # to repeat 5 times

## combining seq() and rep()

rp_seq = rep(seq(4, 6, 0.25), 2)
rp_seq

##-----------------------------------------------------------
## Functions on Vectors 
##-----------------------------------------------------------

# Finding Position of a value in a Vector

which(rp_seq==4)


# Finding existence of a value in a Vector

4 %in% rp_seq

##-----------------------------------------------------------
## Let us access the 10th element in rp_seq via [ ]. 
##-----------------------------------------------------------

rp_seq[10]


## return more that one value
rp_seq[c(1,10)]
rp_seq[c(1:10)]


## return values EXCLUDING certain values
rp_seq[-4] # return all values in rp_seq excluding one on 4th position

rp_seq[-c(4:12)] # return all values in rp_seq excluding values on 4th to 12th position


##-----------------------------------------------------------
## Factors
##-----------------------------------------------------------

## let's use vector "workshop" created earlier

## Making the workshop vector into a factor variable

factor.workshop <- factor(workshop,levels=c(1,2,3,4),labels=c("BA","SBA","AM","EM"))
factor.workshop
class(workshop)

##-----------------------------------------------------------
## Matrices
##-----------------------------------------------------------

mymatrix <- matrix(c( 1, 1, 5, 1,2, 1, 4, 1,2, 2, 4, 3,3, 1,NA, 
                      3,4, 5, 2, 4,5, 4, 5, 5,5, 3, 4, 4,4, 5, 5, 5),
                   nrow=8, ncol=4, byrow=TRUE)
mymatrix

##-----------------------------------------------------------
## Accessing elements of a matrics
##-----------------------------------------------------------

mymatrix[4,3] # returns element at 4th row and 3rd column

mymatrix[4,]  # returns the fourth row

mymatrix[,3] # returns the third column

mymatrix[c(1,2),] # returns 1st and 2nd row and all columns

# what if we put negative numbers to retrieve elements?

mymatrix[-4,-3] 
mymatrix[-c(3,4),]
mymatrix[,-c(1,2)]

# Check if mymatrix is a metric (just like is,vector)
is.matrix(mymatrix)

##-----------------------------------------------------------
## Adding Columns & Rows to a matrix
##-----------------------------------------------------------

# returns dimensions of metric

dim(mymatrix) 

# adding a column to metrix

mymatrix <- cbind(mymatrix, 1:5) 
mymatrix

# adding a row to a matrix

mymatrix <- rbind(mymatrix, 1:6)
mymatrix

##-----------------------------------------------------------
## Lists Data Type
##-----------------------------------------------------------

mylist <- list(workshop = workshop, 
               rp = rp_seq, 
               mat = mymatrix)

mylist

##-----------------------------------------------------------
## Accessing elements of a list
## use [[]] to access each "parent" elements
## use [] in combination with [[]] to access "child" elements
##-----------------------------------------------------------

mylist[[2]]              # accessing rp_seq
mylist[1:2]
mylist[[2]][1]          # accessing first element of rp_seq
mylist[c(2,3)]
##-----------------------------------------------------------
## Some Important functions
##-----------------------------------------------------------

##-----------------------------------------------------------
## Mode - classification of an object according to its basic 
## structure
## Class - Property assigned to an object which determines how 
## generic functions are going to operate on it
##-----------------------------------------------------------

mode(rp_seq)

mode(mylist)

mode(mymatrix)

class(rp_seq)

class(mylist)

class(mymatrix)

##-----------------------------------------------------------
## str function returns the structure of the object, giving
## information like its class and its components
##-----------------------------------------------------------

str(mylist)
