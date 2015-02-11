
#Scraping Economic Times for latest trends

library(RCurl)
library(wordcloud)

# Set SSL certs globally

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

url <- 'http://economictimes.indiatimes.com/'
page <- getURL(url)
list <- readHTMLList(url)

url_news <- 'http://economictimes.indiatimes.com/news'
page_news <- getURL(url)
list_news <- readHTMLList(url)

url_markets <- 'http://economictimes.indiatimes.com/markets'
page_markets <- getURL(url)
list_markets <- readHTMLList(url)

url_opinion <- 'http://economictimes.indiatimes.com/opinion'
page_opinion <- getURL(url)
list_opinion <- readHTMLList(url)

tic <- as.data.frame(c(unlist(list),unlist(list_news),unlist(list_markets),unlist(list_opinion)))

write.csv(tic,'et.csv')
                       
                       