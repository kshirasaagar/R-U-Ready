
#Read individual sheets from an excel file

setwd('C:\\Users\\kshira.saagar\\Desktop\\Session 2 -Data')

library(gdata)

for(j in 1:2)
{
for(i in 1:2)
  {
  input_file = paste('Input',j,sep='_')
  input_file = paste(input_file,'xls', sep = '.')
  assign(paste('f',i,j,sep='_'),read.xls(input_file, perl = 'C:\\myperl\\portableshell.bat', sheet = i))
}
}