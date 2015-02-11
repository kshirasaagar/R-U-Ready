
#Twittercise - New Code

setwd('E:\\MSU\\R\\TWITTERCISE');

#Enter the Twitter user name
user_name <- "esurance"

python <- paste('c:/Python27/python.exe E:/MSU/R/TWITTERCISE/Tweetfinder.txt',user_name)
file_name <- paste(user_name,'.txt',sep='')

system(python)

tweets <- read.delim(file_name,sep = "\t", quote = "",
                     header = FALSE, stringsAsFactors = FALSE,
                     colClasses = c("character","character","numeric",
                                    "character","character","character",
                                    "numeric","numeric","numeric","character"),
                     col.names = c("Tweet_Time","Tweet_ID","Retweet_Count",
                                   "Favorited","In Reply to User ID", "User_ID",
                                   "Friends_Count","Followers_Count","Tweets_Count",
                                   "User_Created_Date"))

tweets <- subset(tweets, !duplicated(tweets$Tweet_ID))

assign(paste(user_name,"_tweets",sep=''), tweets[c(1,2,3)])
assign(paste(user_name,"_user details",sep=''), data.frame(tweets[1,c(7,8,9,10)]))
