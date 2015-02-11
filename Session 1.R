
#Session 1

df <- read.csv('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data\\Traffic Flow - First Example.csv')
world <- read.csv('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data\\World Stats.txt')
bangalore <- read.delim('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data\\Bangalore Temperature.tab', sep = '\t')

setwd('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data')
getwd()

bangalore <- read.delim('Bangalore Temperature.tab', sep = '\t')

movies <- read.delim('Movies',sep='|')

install.packages('sas7bdat')
library(sas7bdat)
roll <- read.sas7bdat('rollup.sas7bdat')


