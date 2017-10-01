#load data
#---------
setwd("C:\\Users\\suman\\Desktop\\datasciencecoursera\\capstone")
#check if downloaded
if(!file.exists("./Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./Coursera-SwiftKey.zip",mode = "wb")
}
#check if unzipped
if(!file.exists("./final")){
  unzip(zipfile="./Coursera-SwiftKey.zip",exdir=".")
}
#read the file with readLines
#-----------------------------
#path<-file.path("./final","en_US")
#files<-list.files(path,recursive = T)
con<-file("./final/en_US/en_US.twitter.txt", "r")
lineTwitter<-readLines(con,skipNul = T)
close(con)
con <- file("./final/en_US/en_US.blogs.txt", "r") 
lineBlogs<-readLines(con, skipNul = T)
close(con)
con <- file("./final/en_US/en_US.news.txt", "r") 
lineNews<-readLines(con, skipNul = T)
close(con)

#summary of files
#-----------------
library(stringi)
fileSummary <- data.frame(
      fileName = c("Twitter","Blogs","News"),
     fileSize = c(round(file.info("./final/en_US/en_US.twitter.txt")$size, digits = 2), 
                                      round(file.info("./final/en_US/en_US.blogs.txt")$size,digits = 2), 
                                      round(file.info("./final/en_US/en_US.news.txt")$size, digits = 2)),
     lineCount = c(length(lineTwitter)[1], length(lineBlogs)[1], length(lineNews)[1]),
     wordCount = c(stri_stats_general(lineTwitter)[1], stri_stats_general(lineBlogs)[1], stri_stats_general(lineNews)[1])                  
  )
fileSummary



#creating sample of 10000
#------------------------
sampleTwitter<-lineTwitter[sample(1:length(lineTwitter),10000)]
sampleBlogs<-lineTwitter[sample(1:length(lineBlogs),10000)]
sampleNews<-lineTwitter[sample(1:length(lineNews),10000)]
testsample<-c(sampleTwitter,sampleBlogs,sampleNews)
save(testsample, file="savedtestsample")

#plot
#-----
#library(ggplot2)


#creating corpus and clean using tm pkg
#--------------------------------------
library(tm)
cleanSample <- VCorpus(VectorSource(testsample))
#view one content of VCorpus
as.character(cleanSample[[1]])
#rm(testsample)
#remove number,punctuation,lowercase transform,url,twitter,hashtag
cleanSample<-tm_map(cleanSample,content_transformer(tolower))
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
urlremove<-function(x)      gsub("[[:alnum:]]*", "", x) 
specialcharremove<-function(x)      gsub("[[:punct:]]*", "", x) 
hashtagremove<- function(x) gsub("#\\S+", "", x)
twitterremove<- function(x) gsub("@\\S+", "", x)
#twitterwordremove<- function(x) gsub("twitter\\S+", "", x)
#special words remove
cleanSample <- tm_map(cleanSample, removeWords, c("twitter"))

#testing
#---------------------------------------
#a<-"hi this is ### --  $$  ^^  *** suman https://google.com suman@gmail.com "
#b<-"li jo"
#a1 <- VCorpus(VectorSource(b))
#email and sitename remove
#emailremove <- function(x) {str_replace_all(x,"[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+", "")}
#urlremove <- function(x) gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", x)
#library(stringr)
#specialcharremove<-function(x)      gsub("[[:punct:]]*", "", x) 
#a1<-tm_map(a1,content_transformer(specialcharremove))
#a1<-tm_map(a1,content_transformer(emailremove))
#a1<-tm_map(a1,content_transformer(urlremove))
#trigramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
#a3 <- TermDocumentMatrix(a1, control = list(tokenize = trigramTokenizer))

#adf<-data.frame(word = a3$dimnames$Terms, frequency = a3$v)
#atdm<-plyr::arrange(tdmdf, -frequency)




#not working below
#cleanSample<-tm_map(cleanSample,content_transformer(urlremove))
#cleanSample<-tm_map(cleanSample,content_transformer(hashtagremove))
#cleanSample<-tm_map(cleanSample,content_transformer(twitterremove))
cleanSample<-tm_map(cleanSample,content_transformer(specialcharremove))
#cleanSample<-tm_map(cleanSample,content_transformer(twitterwordremove))

#cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, stripWhitespace)


#create function for ngram
--------------------------
ngramTokenizer <- function(x,n) NGramTokenizer(x,Weka_control(min = n, max = n))



#1 gram word making
#-----------------------------------
#convert to term document matrix
tdm1 <- TermDocumentMatrix(cleanSample)
#find the words that occurs more than 1000 times
findFreqTerms(tdm1,1000)
#sort data on frequency descending
tdm1freq<- data.frame(word = c(tdm1$dimnames$Terms,rep(NA,length(tdm1$v)-length(tdm1$dimnames$Terms) )), frequency = tdm1$v)
#sort on frequency descending
tdd1freq <- plyr::arrange(tdm1freq, -frequency)
tdm1freq<-tdm1freq[complete.cases(tdm1freq),]
#show the wordcloud for words freq>1000
wordcloud::wordcloud(cleanSample,min.freq=1000)





#create 2 gram pharess
#----------------------------
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(cleanSample, control = list(tokenize = BigramTokenizer))
save(tdm2, file="savedtdm2")
#see most occuring words
#findFreqTerms(tdm2,120)
#make a dataframe with missing term as NA
tdm2freq<- data.frame(word = c(tdm2$dimnames$Terms,rep(NA,length(tdm2$v)-length(tdm2$dimnames$Terms) )), frequency = tdm2$v)
#sort on frequency descending
tdm2freq <- plyr::arrange(tdm2freq, -frequency)
#remove add na values
tdm2freq<-tdm2freq[complete.cases(tdm2freq),]




#create 3 gram pharess
#----------------------------
library(RWeka)
trigramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm3 <- TermDocumentMatrix(cleanSample, control = list(tokenize = trigramTokenizer))
save(tdm3, file="savedtdm3")
#see most occuring words
findFreqTerms(tdm3,60)
#make a dataframe with missing term as NA
tdm3freq<- data.frame(word = c(tdm3$dimnames$Terms,rep(NA,length(tdm3$v)-length(tdm3$dimnames$Terms) )), frequency = tdm3$v)
#sort on frequency descending
tdm3freq <- plyr::arrange(tdm3freq, -frequency)
#remove add na values
tdm3freq<-tdm3freq[complete.cases(tdm3freq),]


