
#UGARCH trials

library(rugarch)
setwd("E:\\SEARCH\\HILTON\\DATA")
raw <- read.csv("Hilton_Impressions.csv",stringsAsFactors = FALSE)
variate <- as.numeric(raw$Impressions)

spec = ugarchspec(mean.model = list(armaOrder = c(1,2),include.mean = TRUE,arfima = TRUE))
fit = ugarchfit(spec,variate, solver = "solnp",fit.control = list(stationarity = 1))
forecast = ugarchforecast(fit,variate,n.ahead = 365)
plot(forecast,which = "all")


