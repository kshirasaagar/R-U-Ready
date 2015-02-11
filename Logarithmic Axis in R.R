
#Logarithmic Axis in R
setwd("E:\\MSU\\R\\Self Preparation\\Session 2")
world <- read.csv('World Stats.txt',stringsAsFactors = FALSE)

plot(world$gdp,world$income)

#Both x and y on log axis
plot(world$gdp,world$income,log = "xy")

#Only X on log axis
plot(world$gdp,world$income,log = "x")

#Only Y on log axis
plot(world$gdp,world$income,log = "y")