
#Extracting Physician-Payor Disclosures from website

library(XML) 
library(RCurl)

table <- list()
final <- data.frame()

for (i in 1:20)
{
  url1 <- paste("http://projects.propublica.org/docdollars/search?page=",i,sep='')
  url2 <- "&period[]=&services[]=&showall=true&state[id]=&term=&utf8=%E2%9C%93"
  url <- paste(url1,url2,sep='')
  page <- getURL(url)
  table <- (readHTMLTable(page,header=FALSE)) [[1]]
  final <- rbind(final,table)
}

colnames(final) <- c("Name/Payee", "City", "State", "Company", "Year", "Category", "Amount")

write.csv(final,"Final Table.csv")