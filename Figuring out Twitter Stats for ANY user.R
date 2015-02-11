
#Figuring out Twitter Stats for ANY user

python <- paste('c:/python27/python.exe c:/python27/tweetdigger.py')
file_name <- paste(user_name,'.txt',sep='')

system(python)

tweets <- read.delim(file_name,sep = "\t", quote = "",header = FALSE)

colnames(tweets) <- c("Timestamp","Tweet")

for(i in 1:(length(tweets$Timestamp)-1))
  if(tweets[i,1] == tweets[i+1,1]) tweets[i,3] = 1 else tweets[i,3] = 0

tweets <- tweets[-which(tweets[,3] == 1),-3]
tweets$Timestamp <- strptime(sub('\\+0000','',tweets$Timestamp),"%c")
tweets = tweets[which(is.na(tweets$Timestamp) == FALSE),]

weekday <- data.frame(table(weekdays(tweets$Timestamp)))
month <- data.frame(table(strftime(tweets$Timestamp,"%b %Y")))
hour <- data.frame(table(format(tweets$Timestamp,"%H")))

max_weekday <- as.character(weekday[which.max(weekday$Freq),1])
max_hour <- as.character(hour[which.max(hour$Freq),1])
max_month <- as.character(month[which.max(month$Freq),1])
max_tweets_month <- as.character(month[which.max(month$Freq),2])

print(paste('User Name is :',user_name))
print(paste('Tweet Count :', length(which(is.na(tweets$Timestamp) == FALSE))))
print(paste('Maximum Tweets on ',max_weekday,'s',sep = ''))
print(paste('Maximum Tweets at ',max_hour,':00 hours',sep =''))
print(paste('Most Active month was ',max_month,' with ',max_tweets_month,' tweets',sep = ''))
plot(hour,type = 'h',xlab = 'Hour of the day', ylab = 'No of Tweets', main = 'Tweet Count by Hour of the Day')

#Enter the Twitter user name
user_name <- "anilanto77"
