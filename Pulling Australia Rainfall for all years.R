
#Pulling Australia Rainfall for all years

library(XML) 
library(RCurl) 
theurl <- "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=086232"
page <- getURL(theurl)

Final_Results <- (readHTMLTable(page,header=FALSE)) [[1]]
Table <- Final_Results[-c(1,27,28),-14]


colnames(Table) <- c('Year','Jan','Feb','Mar',
                     'Apr','May','Jun',
                     'Jul','Aug','Sep',
                     'Oct','Nov','Dec')

write.csv(Table,'')