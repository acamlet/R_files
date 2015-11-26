# install.packages
install.packages("twitteR", 
                 "RCurl", 
                 "RJSONIO", 
                 "stringr", 
                 "base64enc",
                 "ROAuth")

library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(base64enc)

# Declare Twitter API Credentials
api_key <- "4tZCZIPJBqK2Y1C8QdIzrgWB1" # From dev.twitter.com
api_secret <- "7YkEhlSCrdbo76ICpoVMgQqdgy7uuIYBwRVSEZGI2EgpVJsl5g" # From dev.twitter.com
token <- "3148844640-NqstpGnst2JwCWEmuQiw8y26XSgkcCWjo2FnbVL" # From dev.twitter.com
token_secret <- "CYOMAtlQI4jPvOkclwN8nBu5xuqPIFDwAIFrAwqoMAPAm" # From dev.twitter.com

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

# search keyword
keyword = enc2utf8("국정교과서")
history <- searchTwitter(keyword, n=300,lang="ko")
head(history)

# 트위터 본문만 빼냄
history <- twListToDF(history)
history.text <- history$text

# 문장에서 단어분리
library(KoNLP)
history_word<-sapply(history.text, extractNoun, USE.NAMES=F)
history_word<-unlist(history_word)


# 불필요한 단어삭제 
history_word <- history_word [nchar(history_word)>=2]
history_word <- gsub('\n', '', history_word)
history_word <- gsub('\r', '', history_word)
history_word <- gsub('RT', '', history_word)
history_word <- gsub('http', '', history_word)
history_word <- gsub('[[:punct:]]', '', history_word)


# 공백 없애기 위해 저장하였다가 다시 불러옴
write(history_word,"history.txt")
history<-read.table("history.txt")


# 상위 20개 단어 확인
wordcount<-table(history)
head(sort(wordcount,decreasing = T),20)


# 워드클라우드 출력
library(RColorBrewer)
palete<-brewer.pal(9,"Set1")
wordcloud(names(wordcount), 
          freq=wordcount, 
          scale=c(5,0.5), 
          rot.per=0.25,
          min.freq=3, 
          random.order=F, 
          random.color=T, 
          colors=palete)


