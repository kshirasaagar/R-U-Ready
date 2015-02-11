
#Heat-Map attempts 1

center = c(22.5,82.5)
zoom = 5
india_map <- GetMap(center = center,zoom = zoom, destfile = "India Map.png",sensor = "true")
tmp <- india_map + PlotOnStaticMap(india_map, lat = c(22,65), 
                                  lon = c(24,76), 
                                  destfile = "IndiaMap.png", 
                                  cex=1.5,pch=20,
                                  col=c('blue', 'green'), add=FALSE);

tmp1 <- india_map + geom_point(x = lon, y = lat, size = 10);

as.ggplot(india_map)

india_map <- qmap('India',zoom = 5, color = 'color')