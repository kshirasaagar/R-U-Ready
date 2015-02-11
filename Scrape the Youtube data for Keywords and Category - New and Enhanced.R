
#Scrape the Youtube data for Keywords and Category - New and Enhanced
#Kshira Saagar - Cisco

Input_path = "E:\\Links.csv"     #Mention the Input file here - where the first and only column should be links
Output_path = "E:\\Output.csv"   #Mention the Output file here
con <- socketConnection(host = "192.168.16.19", 8080) #Optional Proxy - Enable if needed

################ Don't modify beneath these lines ###############################

url <- read.csv(Input_path, stringsAsFactors = FALSE)
colnames(url) <- 'links'

out <- data.frame('','','')
colnames(out) <- c('URL','Keywords','Category')
lines <- list(2)
  
  for (i in 1:length(url$links))
  {
    lines <- readLines(url$links[i])               
    one <- data.frame(url$links[i],
               gsub(".*?content\\=(.*?)>.*", "\\1", lines[grep('keywords',lines)] [1]),
               substr(lines[grep('category',lines)],46,(max(gregexpr('\\"',lines[grep('category',lines)])[[1]]))-1))
    colnames(one) <- c('URL','Keywords','Category')
    out <- merge(out,one,all.y = TRUE,all.x = TRUE)                 
    Sys.sleep(2)
  }
  
out <- out[-1,]
write.csv(out,Output_path)

#################################################################################