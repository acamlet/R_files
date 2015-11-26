#3) 텍스트마이닝
#트위터 가입 및 모바일 연동
#http://app.twitter.com 접속 App생성
##1) API설정 변경
##2) API key 복사해두기
##3) twitterR 패키지설치

#install.packages("twitteR")
#install.packages("RCurl")                 
#install.packages("RJSONIO")
#install.packages("stringr")
#install.packages("bit64")
#install.packages("base64enc")

library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(KoNLP)


#인증키 벡터 생성
api_key <- "4tZCZIPJBqK2Y1C8QdIzrgWB1" 
api_secret <- "7YkEhlSCrdbo76ICpoVMgQqdgy7uuIYBwRVSEZGI2EgpVJsl5g" 
token <- "3148844640-NqstpGnst2JwCWEmuQiw8y26XSgkcCWjo2FnbVL" 
token_secret <- "CYOMAtlQI4jPvOkclwN8nBu5xuqPIFDwAIFrAwqoMAPAm"


#API 인증
setup_twitter_oauth(api_key, api_secret, token, token_secret)


#트위터에서 자료가져오기
keyword<-enc2utf8("박근혜")
president <- searchTwitter(keyword, n=500,lang="ko")

apple_tweets<-searchTwitter("@apple", n=500)
head(apple_tweets)
length(apple_tweets)


#감정사전 불러오기
setwd("D:/git/github/R_files/R_files") 
pos.word=scan("positive-words.txt", what="character", comment.char = ";")
neg.word=scan("negative-words.txt", what="character", comment.char = ";")


#score.sentiment 함수생성
score.sentiment=function(sentences, pos.words, neg.words, .progress="none")
{
  require(plyr)
  require(stringr)
  score=laply(sentences, function(sentence,pos.words,neg.words) {
    #문장에서 불필요한 요소 삭제
    sentence=gsub('[[:punct:]]','',sentence)
    sentence=gsub('[[:cntrl:]]','',sentence)
    sentence=gsub('\\d+','',sentence)
    #영어 모두 소문자로 
    sentence=tolower(sentence)
    #문장을 단어로 분리
    word.list=str_split(sentence,'\\s+')
    #분리된 단어들을 리스트에서 벡터로 변경
    words=unlist(word.list)
    #감정사전과 단어 비교
    pos.matches=match(words, pos.words)
    neg.matches=match(words, neg.words)
    #결측값 제거
    pos.matches=!is.na(pos.matches)
    neg.matches=!is.na(neg.matches)
    #긍정빈도-부정빈도=점수
    score=sum(pos.matches)-sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  score.df=data.frame(scores=score, text=sentences)
  return(score.df)
}


#트위터 문자 array로 변환후 UTF8로 인코딩변환
library(plyr) #laply : list를 분리하여 array로 반환
apple_text<-laply(apple_tweets, function(t)t$getText()) #getText트위터글에 본문만추출
str(apple_text)
head(apple_text)


#인코딩 변환 
Encoding(apple_text)[1:10]
apple_text<-apple_text[!Encoding(apple_text)=="UTF-8"]
head(apple_text,4) 
apple_text[[10]]   


#score.sentiment 함수로 감정 점수산출
apple_score<-score.sentiment(apple_text, pos.word, neg.word, .progress='text')
hist(apple_score$scores)



##한글트위터 분석


#갤럭시, 아이폰 트위터 가져오기
galaxy <- enc2utf8("갤럭시")
galaxy <- searchTwitter(galaxy, n=1000,lang="ko")
head(galaxy)

iphone <- enc2utf8("아이폰")
iphone <- searchTwitter(iphone, n=1000,lang="ko")
head(iphone)

#getText()함수 : 트위터 내용중 본문만 추출
g_text<-laply(galaxy, function(t)t$getText())
g_text<-g_text[Encoding(g_text)=='UTF-8']

i_text<-laply(iphone, function(t)t$getText())
i_text<-i_text[Encoding(i_text)=='UTF-8']

#불필요한 문자제거
g_text<-gsub('\\x','',g_text) #특수문자제거
g_df<-as.data.frame(g_text)
View(g_df)

i_text<-gsub('\\x','',i_text) #특수문자제거
i_df<-as.data.frame(i_text)
View(i_df)


#감정사전 불러오기
pos.word=scan("positive-words-ko-v2.txt", what="character", comment.char = ";")
neg.word=scan("negative-words-ko-v2.txt", what="character", comment.char = ";")


#인코딩변환
i_text<-i_text[Encoding(i_text)=="UTF-8"]
g_text<-g_text[Encoding(g_text)=="UTF-8"]


# 갤럭시 점수산출
par(mfrow=2:1)
g_score<-score.sentiment(g_text, pos.word, neg.word, .progress="none")
hist(g_score$score)

# 아이폰 점수산출
i_score<-score.sentiment(i_text, pos.word, neg.word, .progress="none")
hist(i_score$score)

# plot그리기
iphone<-i_score$score
galaxy<-g_score$score

plot(density(iphone), col="red")
par(new=T)
plot(density(galaxy), col="blue")


