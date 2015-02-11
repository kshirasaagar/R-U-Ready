
#Converting PDF to readable tables

setwd("D:\\")

final <- data.frame("",0)
colnames(final) <- c("Text","Page")

for (i in 1:10)
{
  file1 <- read.csv(paste("publication_",i,".txt",sep=''), header = FALSE, strip.white = TRUE)
  file1 <- cbind(file1,rep(i,nrow(file1)))
  colnames(file1) <- c("Text","Page")

  remove <- c(grep("\\$",file1$Text))

  file2 <- file1[-remove,]
  file3 <- file2[which(nchar(as.character(file2$Text)) > 10),]

  keep <- c(grep("g$",file3$Text),
          grep("Kg",file3$Text),
          grep("mL",file3$Text),
          grep("Litre",file3$Text),
          grep("Pack",file3$Text),
          grep("Australian",file3$Text),
          grep("Coles",file3$Text),
          grep("D'Orsogna",file3$Text),
          grep("Superior Gold",file3$Text),
          grep("From the",file3$Text))

  file4 <- unique(file3[sort(unique(keep)),])
  
  final <- rbind(final,file4)
}

final <- final[-1,]