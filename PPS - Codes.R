
data_views <- read.csv("~/Desktop/PPS download 07052013.csv", stringsAsFactors = FALSE)

which(data_views$Book1 == '' & data_views$Book2 == '' & data_views$Book3 == '')

#Number of words in a sentence
summary(sapply(gregexpr("\\W+", data_views$Book1.reason), length) + 1) #People use 18-20 words on an average. The 75% is at 24 words and 25% is at 14 words
summary(sapply(gregexpr("\\W+", data_views$Book2.reason), length) + 1) #Decrease of 3 words over the board
summary(sapply(gregexpr("\\W+", data_views$Book3.reason), length) + 1) #Decrease of 5 words over the board

library(tm)
library(wordcloud)
#Understanding Book Reasons
Book1.reason_2 <- c(tolower(data_views$Book1.reason),tolower(data_views$Book2.reason),tolower(data_views$Book3.reason))
Book1.reason_2 <- removeWords(Book1.reason_2, c(stopwords(kind = "en"), "also","lot","read","reading","one","books","book"))
Book1.reason_2 <- removePunctuation(Book1.reason_2, preserve_intra_word_dashes = TRUE)
wordcloud(Book1.reason_2, min.freq = 15, random.order = FALSE, random.color = FALSE)

#Understanding Books
Book <- c(tolower(data_views$Book1),tolower(data_views$Book2),tolower(data_views$Book3))
Book <- removeWords(Book, c(stopwords(kind = "en"), "also","lot","read","reading","one","books","book"))
Book <- removePunctuation(Book, preserve_intra_word_dashes = TRUE)
wordcloud(Book, min.freq = 15, random.order = FALSE, random.color = TRUE)

#Understanding Movies
Movie <- c(tolower(data_views$Movie1),tolower(data_views$Movie2),tolower(data_views$Movie3))
Movie <- removeWords(Movie, c(stopwords(kind = "en"), "also","lot","one","movie","watch"))
Movie <- removePunctuation(Movie, preserve_intra_word_dashes = TRUE)
wordcloud(Movie, min.freq = 15, random.order = FALSE, random.color = TRUE)

#Understanding Movie Reasons
Movie_reason <- c(tolower(data_views$Movie1.reason),tolower(data_views$Movie2.reason),tolower(data_views$Movie3.reason))
Movie_reason <- removeWords(Movie_reason, c(stopwords(kind = "en"), "also","lot","one","movie","watch","film","like"))
Movie_reason <- removePunctuation(Movie_reason, preserve_intra_word_dashes = TRUE)
wordcloud(Movie_reason, min.freq = 15, random.order = FALSE, random.color = FALSE)


#Understanding Misconception
Misconception <- tolower(data_views$Misconception)
Misconception <- removeWords(Misconception, c(stopwords(kind = "en"), "also","people","think","others","person","actually","true",
                                              "friends","will","get","thinks","everyone","true","one",
                                              "know","many","always","thinking",
                                              "just","good","like","much","take",
                                              "things","time","work","can","well"))
Misconception <- removePunctuation(Misconception, preserve_intra_word_dashes = TRUE)
wordcloud(Misconception, min.freq = 15, random.order = FALSE, random.color = FALSE)

#Understanding Failure
Failure <- tolower(data_views$Failure)
Failure <- removeWords(Failure, c(stopwords(kind = "en"), "also","people","think",
                                              "others","person","actually","true",
                                              "friends","will","get","failure","failures",
                                              "know","many","always","life",
                                              "just","much","take",
                                              "can","well"))
Failure <- removePunctuation(Failure, preserve_intra_word_dashes = TRUE)
wordcloud(Failure, min.freq = 15, random.order = FALSE, random.color = FALSE)




# tdm <- as.TermDocumentMatrix(Book1.reason_2,control = list(removePunctuation = TRUE,stopwords = TRUE), weighting = weightTf)
# 
# findFreqTerms(tdm)
