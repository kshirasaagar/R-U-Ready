

#Find the Page Title or Header of an URL
library(RCurl)
url = "http://blogs.hbr.org/taylor/2012/08/its_more_important_to_be_kind.html"
r1 <- getURL(url)
title = substr(r1,regexpr("<title>",r1)+7,regexpr("</title>",r1)-1)
