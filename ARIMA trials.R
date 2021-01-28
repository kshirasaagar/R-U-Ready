
#More Comments
#ARIMA trials

setwd("E:\\SEARCH\\HILTON\\DATA")
raw <- read.csv("Hilton_Impressions.csv",stringsAsFactors = FALSE)

get.best.arima <- function(x.ts, maxord = c(1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
  {
    fit <- arima(x.ts, order = c(p,d,q))
    fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
    if (fit.aic < best.aic)
    {
      best.aic <- fit.aic
      best.fit <- fit
      best.model <- c(p,d,q)
    }}
  list(best.aic, best.fit, best.model)
}

best.arima <- get.best.arima(variate, maxord=c(2,2,2))

variate <- as.numeric(raw$Impressions)
ts.plot(variate)
variate_new <- diff(log(raw$Impressions))
ts.plot(variate_new)

order = best.arima[[3]]

fore_raw <- arima(variate,order = c(1,2,2))

predict_raw <- predict(fore_raw, n.ahead = 100)

ts.plot(variate,xlim=c(1,566))
lines(predict_raw$pred, col = "red")
