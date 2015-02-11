
#Copying Data directly from Excel

read.excel <- function(header=TRUE,...) 
              {
              read.table("clipboard",sep="\t",header=header,...)
              } 

file <- read.excel()

dada <- read.table("clipboard",sep="\t",header=TRUE)