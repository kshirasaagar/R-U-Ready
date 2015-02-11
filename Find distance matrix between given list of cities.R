
#Find distance matrix between given list of cities

library(ggmap)

dist <- data.frame()

city1 <- c('Melbourne',  'Sydney',	'Adelaide',	'Brisbane',	'Perth',	'Hobart',	'Sydney',	'Canberra',	'Auckland',	'Christchurch',	'Dunedin',	'Hamilton',	'Napier',	'Queenstown',	'Wellington')
city2 <- c('Melbourne',  'Sydney',  'Adelaide',	'Brisbane',	'Perth',	'Hobart',	'Sydney',	'Canberra',	'Auckland',	'Christchurch',	'Dunedin',	'Hamilton',	'Napier',	'Queenstown',	'Wellington')

for (i in 11:15)
{
  for(j in 1:15)
  {
    point1 <- geocode(city1[i])
    point2 <- geocode(city2[j])
    dist[i,j] = as.numeric(geodetic.distance(point1,point2))
  }
}
    
geodetic.distance <- function(point1, point2)
{
  R <- 6371
  p1rad <- point1 * pi/180
  p2rad <- point2 * pi/180
  d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))  
  d <- acos(d)
  R*d + 200
}