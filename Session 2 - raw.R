#Session 2 - raw

c <- c(1,2,3)
d <- c('a','b','c')
f <- factor(d)
m <- matrix(1:12,4,3)
l <- list(c,d,f,m)
fix(d)
fix(l)
l
l[[1]]
c
d
df <- data.frame(c,d)
View(df)
sum(world$gdp)
world <- read.csv('World Stats.txt')
setwd('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data')
world <- read.csv('World Stats.txt')
sum(world$gdp)
mode(world$gdp)
world[,5]
d1 <- world[5]
d2 <- world[,5]
d3 <- world$military
world[1,2]
world[1,2] = 5600
View(world)
which(literacy == 98.7)
which(world$literacy == 98.7)
world[1]
which(world$literacy == 98.7)
world[which(world$literacy == 98.7),]
ib <- world[which(world$income>10000)]
ib <- world[which(world$income>10000),]
View(ib)
world[which(world$income>30000),]
world[which(world$literacy == 98.7),]
world[which(world$literacy == 98.7),1]
d <- world[which(world$literacy == 98.7),1]
movies <- read.delim('Movies', sep= '|')
View(movies)
head(movies[1])
head(movies[c(1,2)])
head(movies[1,2])
head(movies[c(1,2)])
head(movies[c(1,3)])
head(movies[c(2,4)]
;
head(movies[c(2,4)]
head(movies[c(2,4)])
head(movies[c(2,4)]))
head(movies[c(2,4)])
movies[which(movies$rank <= 10),c(2,4)]
View(world)
sum(world$gdp[which(world$literacy==98.7),
sum(world$gdp[which(world$literacy==98.7),)
sum(world$gdp[which(world$literacy==98.7])
sum(world$gdp[which(world$literacy==98.7)]
;
sum(world[which(world$literacy == 98.7),2])
count(world[which(world$literacy == 98.7),1])
length(world[which(world$literacy == 98.7),1])
;
