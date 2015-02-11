
#Scraping Clinical Trials based on NCT IDs

library(RCurl)
library(XML)

#Enter the NCT ID here
nct = "NCT01836523"

url1 <- paste('http://clinicaltrials.gov/archive/',nct,sep='')
page1 <- getURL(url1)
table1 <- readHTMLTable(page1)

dates <- data.frame(table1[2])[1]

for(i in 2:nrow(dates))
{
  url <- paste('http://clinicaltrials.gov/archive/',nct,'/',as.character(dates[i,1]),'/changes',sep='')
  page <- getURL(url)
  table <- readHTMLTable(page)
  if (as.character(data.frame(table[2])[2,3]) == "Recruiting") trial_start_date = as.character(dates[i,1])
  if (as.character(data.frame(table[2])[2,3]) == "Active, Not recruiting") trial_end_date = as.character(dates[i,1])
}
