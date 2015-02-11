
#Draw concentric circles and plot points within each circle

#Create an empty plot
plot(1:20,axes = F,type="n",xlab="",ylab="",pch = 20)

#Draw concentric circles
draw.circle(10,10,3,col="green",lty=1,lwd=1)
draw.circle(10,10,2,col="yellow",lty=1,lwd=1)
draw.circle(10,10,1,col="red",lty=1,lwd=1)

#Plot away to glory
points(10,9,pch = 20)
points(11,7,pch = 20)
points(11,6,pch = 20)
points(7.5,9,pch = 20)
points(7.5,10,pch = 20,cex = 0.75)
