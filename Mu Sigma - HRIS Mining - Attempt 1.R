
#Mu Sigma - HRIS Mining - Attempt 1

setwd("D:\\Photos\\Info")
library(XML)
library(RCurl)

for (i in 1500:1520)
{
  url <- paste("https://hris.mu-sigma.com/HRIS/empdirinfo.do?emplid=",i,sep='')
  page <- getURL(url)

  #Get Image
  photo <- substr(page,regexpr('src=\"photos',page)+12,regexpr('.jpg',page)+3)
  photo_file <- paste('D:\\Photos\\',photo,sep= '')
  download.file(paste('https://hris.mu-sigma.com/HRIS/photos/',photo,sep=''),photo_file,mode='wb')
    
  #Get Personnel Information
  table <- as.data.frame(readHTMLTable(page)[2])
  colnames(table) <- c("Information","Value")
  assign(paste("Employee_",photo,sep=''),table)
}



