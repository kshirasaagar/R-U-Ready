
#Google Scraping

term <- 'Kshira Saagar'
pages <- 2

jfgit = function(term,page = 0)
{
url = paste('http://www.google.com/search?q=',term,sep='')
if(page > 0) url = paste('http://www.google.com/search?q=',term,'&start=',page*10,sep='')
read = readLines(url)
hrefpat = 'href *= *"([^"]*)"'
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(hrefpat,read[grep('href *=',read)])
res = mapply(getexpr,read[grep('href *=',read)],gg)
links = sub(hrefpat,'\\1',res)
refs = links[grep('q=http:',links)]
clean = function(text) substr(text,regexpr("http:/",text),regexpr("&amp",text)-1)
refs2 = lapply(refs,clean)
}

links = sapply(0:pages,function(page) jfgit(term,page))
links = unlist(links)[-1]
paste('The top links are','')
links
