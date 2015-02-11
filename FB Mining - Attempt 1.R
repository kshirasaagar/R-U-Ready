
#FB Mining - Attempt 1

require(RCurl)
require(rjson)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

access_token <- "CAACEdEose0cBAJAd43l3rkhB6keNpeMaJ29HI5ZCC32ZCNZCEfXzhWKJsSjS8BFDiFcnL3sLHjujUkiiDCFCE0whwFG8enEp53YKhc2MuUKo9FCqZAFb5rsloVCi616wdGvsYZC3yomeZAvXZCmg5BWDwwf1uaaLkseQUZB9gtjhFcF255zNs3ZBMNROoSQUjOMAZD"

facebook <-  function( path = "me", access_token = token, options){
  if( !missing(options) ){
    options <- sprintf( "?%s", paste( names(options), "=", unlist(options), collapse = "&", sep = "" ) )
  } else {
    options <- ""
  }
  data <- getURL( sprintf( "https://graph.facebook.com/%s%s&access_token=%s", path, options, access_token ) )
  fromJSON( data )
}

dir.create("photos")

photos <- facebook( "me/photos", access_token)
sapply( photos$data, function(x){
  url <- x$source
  download.file( url, file.path( "photos", basename(url) ) )
})


friends <- facebook( path="me/friends" , access_token=access_token)