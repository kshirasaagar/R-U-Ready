# ======================================================================================#
#-- Code:           Mustang
#-- Project Name:   --                                                      
#-- Task :          Extract information from a tweet JSON & store it into a CSV
#-- version :       1.0.1
#-- date :          07/15/2013
#-- author :        Eeshan Chatterjee
#-- SVN Directory:  \xxxx
# ======================================================================================#

# ======================================================================================#
#input:  A Tweet JSON 	
#output: Tweet appended to CSV file 'Tweets.csv'
# ======================================================================================#

# ======================================================================================#
  require(rjson)
# ======================================================================================#


convertJSON2CSV<-function(Input){  

messageData = fromJSON(Input)
  
  TweetText = gsub(","," ",messageData$text)
  timeoftweet = "NA"
  tweetsource = "NA"
  truncated = "NA"
  coordinates = "NA"
  place = "NA"
  tweethashtags = "NA"
  usersmentioned = "NA"
  userid = "NA"
  username = "NA"
  origauthorID = "NA"
  origauthorName = "NA"
  origTweet = "NA"
  origauthorfollowers = "NA"
  thisauthorfollowers = "NA"

  # Code to extract hashtags and usermentions
  hashtags = vector()
  usermentions = vector()
  
  for(i in seq_along(messageData$entities$hashtags)){
    hashtags = paste(c(hashtags,messageData$entities$hashtags[[i]]$text),collapse='; ') # to be completed
  }
  
  for(i in seq_along(messageData$entities$user_mentions)){
    usermentions = paste(c(usermentions,messageData$entities$user_mentions[[i]]$screen_name),collapse='; ') 
  }

if(length(messageData$retweeted_status)>1){
  origauthorID = messageData$retweeted_status$user$id
  origauthorName = messageData$retweeted_status$user$screen_name
  origTweet = messageData$retweeted_status$text
  origTweet = messageData$retweeted_status$text
  origauthorfollowers = messageData$retweeted_status$user$followers_count
}
  tryCatch({
    if(!is.null(messageData$created_at)){
      timeoftweet <- messageData$created_at
    }
  }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$created_at)){
      timeoftweet <- messageData$created_at
    }
  }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$source)){
      tweetsource <- messageData$source
    }
    }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$coordinates)){
      coordinates <- messageData$coordinates 
      }
    }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$place)){
      place <- messageData$place 
      }
    }, error = function(e){})
  tryCatch({
    if(length(hashtags)>0){
      tweethashtags <- hashtags 
      }
    }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(length(usermentions)>0){
      usersmentioned <- usermentions 
      }
    }, error = function(e){})
  
  tryCatch({
    if(!is.null(messageData$user$id_str)){
      userid <- messageData$user$id_str
      }
    }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$user$screen_name)){
      username <- messageData$user$screen_name
      }
    }, error = function(e){print("error in trycatch")})
  tryCatch({
    if(!is.null(messageData$user$followers_count)){
      thisauthorfollowers <- messageData$user$followers_count
    }
  }, error = function(e){print("error in trycatch")})
  


  rawtweetdata = c(timeoftweet,TweetText,truncated,tweetsource,coordinates,place,tweethashtags,usersmentioned,userid,username,origauthorID,origauthorName,origTweet,origauthorfollowers,thisauthorfollowers)

  rawmatrix = matrix(rawtweetdata,nrow=1)  
  rawdf = as.data.frame(rawmatrix)
  write.table(rawdf,file = "A_tweets7132013_0134_200_5.csv",append = TRUE,col.names=FALSE,row.names=FALSE,sep=',')

      return()   
}

