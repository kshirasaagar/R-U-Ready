
#Offshore-Onsite Utilization - Sanofi

library('googleVis')

data <- read.csv("E:\\numbers.csv",stringsAsFactors = FALSE)

colnames(data) <- c("Quarter","Resource","Current Folks","No. Onboarded","No. Offboarded","Move outs","Move ins","Billed","Buffers","Utilization")

data$Utilization <- as.numeric(sub('\\%','',data$Utilization))

data$Quarter <- as.Date(data$Quarter,"%m/%d/%Y")


M <- gvisMotionChart(data, idvar="Resource", timevar="Quarter",
                     options=list(width=700, height=600))

out <- M$html$chart

(M$html$chart, file="tmp.html")

plot(M)