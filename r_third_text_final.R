#패키지와 함수 정리##################################################

#자연어 처리
library(KoNLP)
SimplePos09('이 영화 정말 재미있다')
extractNoun('구내 식당은 맛이 별로 없다')
a<-SimplePos09('김성근 완전 멋있어요')


#명사, 형용사, 동사 추출
library(stringr)
str_match(a, '([가-힣]+)/[NP]')


#url에서 html소스 가져오기
library(httr)
GET("http://www.google.com")


#html 일부 내용만 추출하기
library(rvest)
htxt<-html_nodes(htxt,".comment") #해당노드만 추출
htxt<-html_nodes(htxt, "a") 
htxt<-html_text(htxt)  # 텍스트만 추출
length(htxt)


#크롤링#########################################################
setwd("g:/kickR")

library("KoNLP") 
library("httr")  
library("rvest")
library("stringr")

movie_text_sum<-c()
movie_text_sum

for(i in 1:200){
  url<-c("http://movie.daum.net/moviedetail/moviedetailNetizenPoint.do?movieId=73750&searchType=all&type=after&page=")
  urls<-GET(paste(url,i,sep=""))
  htxt<-html(urls)  
  htxt<-html_nodes(htxt,".comment") #Html 중에서 코멘트부분 추출
  htxt<-html_nodes(htxt, "a") #a노드만 추출
  htxt<-html_text(htxt) #텍스트만 추출(엔터 그림제외)
  if(length(htxt)==0) break
  movie_text_sum<-c(movie_text_sum, htxt)
}
movie_text_sum


#명사, 형용사, 동사뽑아내기###########################################

keyword<-sapply(movie_text_sum, SimplePos09, USE.NAMES = F)
keyword2<- str_match(keyword, '([가-힣]+)/[NP]')
keyword3<- keyword2[,2]
keyword4<-Filter(function(x) {nchar(x)>=2},keyword3)


#불필요한 단어 삭제
keyword4<-gsub("\\-","",keyword4)  #숫자제거
keyword4<-gsub(" ","",keyword4)    #빈칸제거


#빈칸제거
write(unlist(wordcount),"aven.txt")
wordcount<-read.table("aven.txt")


#단어빈도파악
wordcount<-table(keyword4)
head(sort(wordcount, decreasing = T), 30)


#워드클라우드 출력하기
library(wordcloud)
library(RColorBrewer)
palete<-brewer.pal(9,"Set1")
wordcloud(names(wordcount), 
          freq=wordcount, 
          scale=c(5,0.5), 
          rot.per=0.25, 
          min.freq=1,
          random.order = F, 
          random.color = T, 
          colors = palete)
