
################### Enter the twitter handle/file with handles here ####################

input_file <- 'E:\\d1.csv' # Enter the input file name here

########################################################################################

############################# Do not modify beneath this ###############################

library('twitteR')
library('plyr')

tweeter <- read.csv(input_file, stringsAsFactors = FALSE)
colnames(tweeter) <- 'handle'
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

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

twitCred$handshake(cainfo="cacert.pem")
save(twitCred)
registerTwitterOAuth(twitCred)

for (i in 1:length(tweeter$handle))
{
user <- ldply(userTimeline(tweeter$handle[i],n = 3200, includeRts=TRUE,cainfo="cacert.pem"),statusText)
colnames(user) <- 'Tweets'
assign(paste(tweeter$handle[i],'tweets',sep='_'), user) write.csv(user,eval(paste(tweeter$handle[i],"csv",sep='.')),row.names = FALSE) }

#### End ####
