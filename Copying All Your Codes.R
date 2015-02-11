
#Copying All Your Codes

srcdir <- file.path("//192.168.16.26/Sanofi-Aventis/04. Knowledge Management/02. Training Material/muCELS/Introduction to R")
list.files(srcdir)
destdir <- setwd("E:\\") # modify as desired
chapters <- sprintf("Chapter%02d.R", 1:14)
for (i in chapters)
{
  file = paste(srcdir, i, sep = "")
  file.copy(file, destdir,copy.mode = TRUE)
}