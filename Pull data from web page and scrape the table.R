
#Pull data from web page and scrape the table

library(XML)
library(RCurl)
url = "http://en.wikipedia.org/wiki/List_of_shortest_people"
r1 <- getURL(url)
t1 <- readHTMLTable(r1,header = FALSE)
t1 <- t1[[1]]
t3 <- t1[[9]] [c(3,2)]
write.csv(t3,"t3.csv")

