
#Historical tweets for any given user ID

user_handle <- 'anilanto77' #Mention User handle here

##############################################################

library(ROAuth)
library(twitteR)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "http://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "8plm1E0GjkWSZE0UbyzgQ"
consumerSecret <- "7GyBUlTPhHAw4i1W9L65RVamOwaqU3L8VTCAuPi8p8"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,
consumerSecret=consumerSecret,
requestURL=reqURL,
accessURL=accessURL,
authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

data <- userTimeline(user_handle)
data <- unlist(data)
tweets <- data.frame(data)
