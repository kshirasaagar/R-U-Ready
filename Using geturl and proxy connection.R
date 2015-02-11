
#Using geturl and proxy connection

opts <- list(
  proxy         = "192.168.16.19", 
  proxyusername = "MU-SIGMA\\Kshira.Saagar", 
  proxypassword = "*getalife123", 
  proxyport     = 8080
)

theurl <- "http://www.bbc.co.uk/sport/olympics/2012/medals/countries"
page <- getURL(theurl,.opts = opts)