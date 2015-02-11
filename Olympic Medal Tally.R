
#Olympic Medal Tally

library(XML) 
library(RCurl) 
theurl <- "http://www.bbc.co.uk/sport/olympics/2012/medals/countries"
page <- getURL(theurl)

Final_Results <- (readHTMLTable(page,header=FALSE)) [[1]]
colnames(Final_Results) <- c("Rank","Country","Gold","Silver","Bronze","Total")

India_Results <- Final_Results[which(Final_Results$Country == 'India'),]

if(nrow(India_Results) == 0) {print('No Medals as of now for India!')} else print(India_Results)

head(Final_Results)
