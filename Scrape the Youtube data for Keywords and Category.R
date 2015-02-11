
#Scrape the Youtube data for Keywords and Category

Input_path = "E:\\Links.csv"     #Mention the Input file here
Output_path = "E:\\Output.csv"   #Mention the Output file here

url <- read.csv(Input_path, stringsAsFactors = FALSE)
colnames(url) <- 'links'

youtubeit <- function(url)
{
  con <- socketConnection(host = "192.168.16.19", 8080)
  #Reading the URL as lines
  lines <- readLines(url)
  #Creating a line item of URL, Keywords, Category
  return(paste(url,
               gsub(".*?content\\=(.*?)>.*", "\\1", lines[grep('keywords',lines)] [1]),
               substr(lines[grep('eow-category',lines)],46,(max(gregexpr('\\"',lines[grep('eow-category',lines)])[[1]]))-1),
               sep = ";"))
}

links <- sapply(url$links,function(url) youtubeit(url))
links = unlist(links)
links = data.frame(links)
colnames(links) <- paste('URL','Keywords','Category',sep = ";")
row.names(links) <- NULL
write.csv(links,Output_path,row.names = FALSE)