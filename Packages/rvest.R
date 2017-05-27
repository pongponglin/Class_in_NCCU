library(rvest)
library(dplyr)
library(xml2)
## rvest
page.source <- read_html("https://en.wikipedia.org/wiki/R_(programming_language)")
version.block <- html_nodes(page.source, ".wikitable td , .wikitable th")
head(version.block)
content <- html_text(version.block)
head(content)
content1 <- as.data.frame(content, sep=",")
content2 <- t(content1)
#接下來還不知道怎麼轉

##fail

url = 'https://tw.answers.yahoo.com/dir/index?sid=396545372'
content_css = read_html(url) %>% html_nodes("section p") %>% html_text()
temp <- iconv(content_css,'utf8')

## news
url_news <- "http://news.ltn.com.tw/list/BreakingNews"
news <- read_html(url_news) %>% html_nodes(".picword") %>% html_text() %>% as.data.frame()
# news 網址
#爬取同一個網頁
news_location <- read_html(url_news) %>% html_nodes(".picword") %>% html_attr("href") #%>% as.data.frame()

## 爬多頁標題

#http://news.ltn.com.tw/list/BreakingNews?page=1
#http://news.ltn.com.tw/list/BreakingNews?page=2

title <- NULL
for (i in 1:5) {
  pathfile <- paste("http://news.ltn.com.tw/list/BreakingNews?page=",i,sep = "")
  mtitle <- read_html(pathfile) %>% html_nodes(".picword") %>% html_text() 
  title <- c(title,mtitle)
}
title <- as.data.frame(title)


#爬網址內文 多頁未成功 長度不符 可以用paste連成一篇文章
n=length(news_location)
content <- matrix(ncol = 1, nrow = n)
row.names(content) <- news[1:24,]
for (i in 1:5) {
  path <- news_location[i]
  content[i] <- read_html(path) %>% html_nodes("#newstext p") %>% html_text()
  Sys.sleep(runif(1,2,5))
}

path <- news_location[1]
content <- read_html(path) %>% html_nodes("#newstext p") %>% html_text()

#baseball
url_sport <- "http://sports.ltn.com.tw/"
sport <- read_html(url_sport) %>% html_nodes(".content .list_title") %>% html_text() %>% as.data.frame() 
sport_location <- read_html(url_sport) %>% html_nodes(".content .listA a") %>% html_attr("href") %>% as.data.frame()
#這裡要注意，使用Selector選區塊時不要按在標題上，要點在旁邊，框框會比點在標題上面大喔，
#所以所選出來的程式碼和剛剛的不一樣。



## 不能爬的時候。未解決-------------------------------
try <- read_html("/Users/apple/nicole/smartphone.html") 
html_nodes("topic_gen") %>% html_text() %>% iconv("UTF-8")

download.file("http://www.mobile01.com/topiclist.php?f=61","/Users/apple/nicole/smartphone.html")
#------------------------------------------------------

##apple_news
try <- read_html("http://www.appledaily.com.tw/realtimenews/section/new/") %>%
   html_nodes(".rtddt a") %>% html_attr("href") %>% as.data.frame()
try <- read_html("http://www.appledaily.com.tw/realtimenews/section/new/") %>%
  html_nodes("font")%>% html_text() %>% as.data.frame()

## yahoo
yahoo <- read_html("https://tw.news.yahoo.com/") %>%
  html_nodes(".yom-primary .yog-8u .yom-list a") %>%  html_text() %>% as.data.frame() 


##ptt title
#tmp <- paste(i, '.html', sep='')
ptturl <- 'https://www.ptt.cc/bbs/marvel/index1657.html'
ptt <- read_html(ptturl) 
ptt_css <- html_nodes(ptt, ".title a")
ptt_text <- html_text(ptt_css)
head(ptt_text)
ptt_h <- html_attr(ptt_css ,'href')

# ptt多頁
ptt_data = c()
for(i in 1:length(links_data_ptt)){
  url = paste0('https://www.ptt.cc',links_data_ptt[i])
  content_css = read_html(url) %>% html_nodes("#main-content") %>% html_text()
  utf8_text_content <- iconv(content_css,'utf8')
  ptt_data = c(ptt_data,utf8_text_content)
  Sys.sleep(runif(1,2,5))
}



#?html_nodes
ateam <- read_html("http://www.boxofficemojo.com/movies/?id=ateam.htm")
html_nodes(ateam, "center")
html_nodes(ateam, "center font")
html_nodes(ateam, "center font b")
#?html_text
movie <- read_html("http://www.imdb.com/title/tt1490017/")
cast <- html_nodes(movie, "#titleCast span.itemprop")
html_text(cast)
html_name(cast)
html_attrs(cast)
html_attr(cast, "class")


