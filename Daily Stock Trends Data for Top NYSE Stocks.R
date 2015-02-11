
#Daily Stock Trends Data for Top NYSE Stocks

#Pull tickers of all NYSE contributing stocks
library(XML)
library(RCurl)
url = "http://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"
r1 <- getURL(url)
t1 <- readHTMLTable(r1,header = FALSE)
t2 <- t1[[2]]
ticker <- data.frame((t1[[2]]) [-1,])
ticker <- subset(ticker, V2 == "NYSE",select = V3)

#Get Quotes from Yahoo Finance for Daily Stock Trends
getquotes = function(ticker)
{
  URL <- paste("http://ichart.finance.yahoo.com/table.csv?s=",ticker)
  temp <- read.csv(URL)
  write.csv(temp,paste(ticker,"csv",sep = '.'))
}

for(i in 1 : nrow(ticker)) getquotes(as.character(ticker[i,1]))

#nyse <- read.csv("http://www.nyse.com/indexes/nyahist.csv",header = TRUE)
