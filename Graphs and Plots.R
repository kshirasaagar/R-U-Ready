##-----------------------------------------------------------
## Program: A Course in R Part V.R
## Purpose: A basic introduction to R
## Date   : June 2012 
##-----------------------------------------------------------

##-----------------------------------------------------------
## Set working directory
##-----------------------------------------------------------

setwd("D:/Work/_Documents/Training/R Training (Dec 2011)/WIP")

##-----------------------------------------------------------
## Demo
##-----------------------------------------------------------

demo(graphics)

##-----------------------------------------------------------
## Read in sales.csv
##-----------------------------------------------------------

custsales <- read.table("sales.csv", header = T, sep = ",")

head(sales, n = 10)

attach(custsales)

##-----------------------------------------------------------
## A scatter plot
##-----------------------------------------------------------
windows(width = 5, height = 3, xpos = -5, ypos = 5)

plot(quantity[1:100], type = "l", ylab = "Quantity",
main = "Line Plot")

plot(quantity[1:1000], sales[1:1000], col = "blue",
	main = "A Scatter Plot",
	xlab = "Quantity", ylab = "Sales")

plot(sort(basket))

##-----------------------------------------------------------
## Box Plots
##-----------------------------------------------------------

x <- rnorm(1000, mean = 15, sd = 4)
y <- rnorm(1000, mean = 24, sd = 6)
z <- c(rep("Category A", 1000), rep("Category B", 1000))
w <- c(x,y)

boxplot(w, range = 3, col = "red", main = "Box Plots")

boxplot(w ~ z, range = 0, col = c("red", "blue"),
        main = "Box Plots by Group")

boxplot(w ~ z, range = 1.5, col = c("red", "blue"),
        main = "Box Plots with Outliers")

##-----------------------------------------------------------
## Histograms
##-----------------------------------------------------------

hist(sort(sales)[1:120000], col = "#800000",
     xlab = "Sales", 
     main = "Sales Distribution")

y <- density(sort(sales)[1:120000])

plot.density(y, col = "#006666", main = "Kernel Density Plots")

polygon(y, col = "#006666", border = "#006666")

##-----------------------------------------------------------
## Bar Plots
##-----------------------------------------------------------

data(iris)

iris[1:5,]

irisMeans <- aggregate(iris$Sepal.Length,by= list(iris$Species), FUN = "mean")

barplot(irisMeans$x, names.arg = irisMeans[,1], col = "#008080",
      main = "Mean Sepal Lengths", ylab = "Centimeters",
      xlab = "Species")

##-----------------------------------------------------------
## Stacked Bar Plots
##-----------------------------------------------------------

head(custsales)
summary(quantity)

qty.Segment <- ifelse(quantity < 17, yes="3. Low", 
                no = ifelse(quantity  < 34, "2. Medium", "1. High"))

summary(factor(qty.Segment))

quantile(visits)

visit.Segment <- ifelse(visits < 2, yes = "Occassional", "Frequent")

crosstab <- table(qty.Segment, visit.Segment)

barplot(crosstab, main = "Distribution by Visits & Quantity",
        xlab = "Visit", ylab = "No. of Visits", beside = TRUE,
        col = c("blue", "red", "green"))

demo(graphics)

##-----------------------------------------------------------
## Pie Charts
##-----------------------------------------------------------

# Pie Chart with Percentages
slices <- c(10, 12, 4, 16, 8) 

lbls <- c("US", "UK", "Australia", "Germany", "France")

pct <- round(slices/sum(slices)*100)

lbls <- paste(lbls, pct) # add percents to labels 

lbls <- paste(lbls,"%",sep="") # ad % to labels 

pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries") 

##-----------------------------------------------------------
## 3D Pie Chart
##-----------------------------------------------------------
library(plotrix)

slices <- c(10, 12, 4, 16, 8) 

lbls <- c("US", "UK", "Australia", "Germany", "France")

pie1 <- pie3D(slices, radius = 1.6, explode=0.1, main = "Pie Chart of Countries ")

pie3D.labels(pie1, radius = 2, labels = lbls)

##-----------------------------------------------------------
## Scatter Plot Matrix
##-----------------------------------------------------------

summary(iris$Species)
pairs(iris[,c(1:4)], col = c(rep("red", 50), rep("green", 50), rep("blue", 50)),
      main="Edgar Anderson's Iris Data")

library(psych)

pairs.panels(iris[,c(1:4)],scale = F, ellipses = FALSE)

##-----------------------------------------------------------
## QQ Norm Plots to check if distribution is normal
##-----------------------------------------------------------

hist(iris$Sepal.Width)
qqnorm(iris$Sepal.Width, col = "#800000")

qqline(iris$Sepal.Width, col = "#008080")

mean(iris$Sepal.Width)

sd(iris$Sepal.Width)

rndNorm <- rnorm(1000, mean = 3.05, sd = 0.4358)

qqplot(rndNorm, iris$Sepal.Width, main = "QQ Plot With Normal")
abline(0,1)

##-----------------------------------------------------------
##  Plotting two data series
##-----------------------------------------------------------

x <- seq(0, 2*pi, by = 0.1)

y <- sin(x)

y1 <- cos(x)

plot(x, y, col = "red", type = "l", lwd = 3)

lines(x, y1, col = "green", type = "l", lwd = 3)

mtext("Sine & Cosine Plot", side = 3, line = 2)
# side: 1 = bottom, 2 = left, 3 = top, 4 = right
# line: Distance from the margin


f <- function(x) { x * (x + 1) / 2 }

x <- 1:20

y <- f(x)

plot(x, y, xlab = "", ylab = "", col = "#800000")

mtext("Plotting the expression", side = 3, line = 2.5)
mtext(expression(y == sum(i,1,x,i)), side = 3, line = 0)
mtext("The first variable X", side = 1, line = 3)
mtext("The second variable Y", side = 2, line = 3)

##-----------------------------------------------------------
## Multiple Plots
##-----------------------------------------------------------

par(mfrow = c(2, 2))# arranged by row; 

value <- seq(0,50, 0.1)

plot(value, dnorm(value, 50,15), type = "l", xlab = "Value",
     ylab = "Density at value", main = "Normal Density")

count <- 0:20

plot(count, dbinom(count, 20, 0.35), type = "h", xlab = "Count",
     ylab = "Probability", main = "Binomial Probabilities")

plot(count, pbinom(count, 20, 0.35), type = "s", xlab = "Count",
     ylab = "CDF at Count", main = "Binomial CDF")

x <- 1:12

y <- log(x)+2

plot(x, y, xlab = "X", ylab = "log(X) + 2", bty = "n",
     pch = 20, type = "b",col = "forestgreen", 
     main = "Y = log(X)+2" )

##-----------------------------------------------------------
## 
##-----------------------------------------------------------

par(mfcol = c(3,1))

hist(iris$Sepal.Length, main = "Sepal Length", col = "#008080")

hist(iris$Sepal.Width, main = "Sepal Width", col = "#008080")

hist(iris$Petal.Length, main = "Petal Length", col = "#008080")

##-----------------------------------------------------------
## Identifying points dynamically
##-----------------------------------------------------------

plot(iris$Sepal.Length[1:50], iris$Sepal.Width[1:50], 
     col = "#800000", pch = 16)

identify(iris$Sepal.Length[1:50], iris$Sepal.Width[1:50], 
         labels = row.names(iris[1:50,]))
# Click on some points in the graph and press Esc after you are done

locator()
# Click on some points in the graph and press Esc after you are done
