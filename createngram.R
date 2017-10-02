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



#creating sample of 150000
#------------------------
sampleTwitter<-lineTwitter[sample(1:length(lineTwitter),50000)]
sampleBlogs<-lineTwitter[sample(1:length(lineBlogs),50000)]
sampleNews<-lineTwitter[sample(1:length(lineNews),50000)]
testsample<-c(sampleTwitter,sampleBlogs,sampleNews)
#saveRDS(testsample, file="savedtestsample.rds")

#creating corpus and clean using tm pkg
#--------------------------------------
library(tm)
#careful consumes huge memory
cleanSample <- VCorpus(VectorSource(testsample))
#view one content of VCorpus
as.character(cleanSample[[1]])
#rm(testsample)
#remove number,punctuation,lowercase transform,url,twitter,hashtag
cleanSample<-tm_map(cleanSample,content_transformer(tolower))
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
urlremove <- function(x) gsub("http[^[:space:]]*", "", x)

#urlremove<-function(x)      gsub("[[:alnum:]]*", "", x) 
specialcharremove<-function(x)      gsub("[[:punct:]]*", "", x) 
hashtagremove<- function(x) gsub("#\\S+", "", x)
twitterremove<- function(x) gsub("@\\S+", "", x)
#twitterwordremove<- function(x) gsub("twitter\\S+", "", x)
#special words remove
cleanSample <- tm_map(cleanSample, removeWords, c("twitter"))



#not working below
cleanSample<-tm_map(cleanSample,content_transformer(urlremove))
#cleanSample<-tm_map(cleanSample,content_transformer(hashtagremove))
#cleanSample<-tm_map(cleanSample,content_transformer(twitterremove))
cleanSample<-tm_map(cleanSample,content_transformer(specialcharremove))
#cleanSample<-tm_map(cleanSample,content_transformer(twitterwordremove))
#first I didnot remove stopwords so that pharases are predictable. Now I am removing
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
cleanSample <- tm_map(cleanSample, stripWhitespace)


#create function for ngram
#--------------------------
ngramTokenizer <- function(x,n) NGramTokenizer(x,Weka_control(min = n, max = n))



#1 gram word making
#-----------------------------------
#convert to term document matrix with removing word <5000 times
tdm1 <- TermDocumentMatrix(cleanSample,control=list(wordLengths=c(1, 10), 
                           bounds = list(global = c(5000,Inf))))

#find the words that occurs more than 1000 times
findFreqTerms(tdm1,1000)
#sort data on frequency descending for huge TDM
library(slam)
freq <- sort(rowSums(as.matrix(rollup(tdm1, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm1freq<-data.frame(word = names(freq), freq = freq)



#tdm1freq <- rollup(tdm1, 2, na.rm=TRUE, FUN = sum)



#a<-data.frame(a=c(slam::row_sums(tdm1, na.rm = T)))

#a <- colapply_simple_triplet_matrix(tdm1,FUN=sum)
#b <- sort(a, decreasing=T)


#tdm1freqt<-as.matrix(tdm1)
#tdm1freq<-data.frame(word=rownames(tdm1freqt),freq=rowSums(tdm1freqt))
#a<-sort(tdm1freq,)

#tdm1freq<- data.frame(word = c(tdm1$dimnames$Terms,rep(NA,length(tdm1$v)-length(tdm1$dimnames$Terms) )), frequency = tdm1$v)
#remove added na values
#tdm1freq<-tdm1freq[complete.cases(tdm1freq),]

#sort on frequency descending
#tdm1freq <- plyr::arrange(tdm1freq, -frequency)
#take only top 20 words
#tdm1freq<-tdm1freq[1:20,]
saveRDS(tdm1freq, file="savedtdm1.rds")
#show the wordcloud for words freq>1000
#wordcloud::wordcloud(cleanSample,min.freq=1000)





#create 2 gram pharess
#----------------------------
library(RWeka)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(cleanSample, control = list(tokenize = BigramTokenizer))
#see most occuring words
#findFreqTerms(tdm2,120)
#make a dataframe with missing term as NA
freq <- sort(rowSums(as.matrix(rollup(tdm2, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm2freq<-data.frame(word = names(freq), freq = freq)


#tdm2freq<- data.frame(word = c(tdm2$dimnames$Terms,rep(NA,length(tdm2$v)-length(tdm2$dimnames$Terms) )), frequency = tdm2$v)
#sort on frequency descending
#tdm2freq <- plyr::arrange(tdm2freq, -frequency)
#remove add na values
#tdm2freqtemp<-tdm2freq[complete.cases(tdm2freq),]
#remove the phrases occuring less then 3 times
tdm2freq<-tdm2freq[(tdm2freq$freq>5),]
saveRDS(tdm2freq, file="savedtdm2.rds")




#create 3 gram pharess
#----------------------------
library(RWeka)
trigramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
tdm3 <- TermDocumentMatrix(cleanSample, control = list(tokenize = trigramTokenizer))

freq <- sort(rowSums(as.matrix(rollup(tdm3, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm3freq<-data.frame(word = names(freq), freq = freq)

#see most occuring words
#findFreqTerms(tdm3,60)
#make a dataframe with missing term as NA
#freq <- sort(rowSums(as.matrix(rollup(tdm3, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
#tdm3freq<-data.frame(word = names(freq), freq = freq)

#tdm3freq<- data.frame(word = c(tdm3$dimnames$Terms,rep(NA,length(tdm3$v)-length(tdm3$dimnames$Terms) )), frequency = tdm3$v)
#sort on frequency descending
#tdm3freq <- plyr::arrange(tdm3freq, -frequency)
#remove add na values
#tdm3freqtemp<-tdm3freq[complete.cases(tdm3freq),]
#remove the phrases occuring less then 2 times as I am getting 800 row which is good numer
tdm3freq<-tdm3freq[(tdm3freq$freq>5),]
saveRDS(tdm3freq, file="savedtdm3.rds")



#create 4 gram pharess
#----------------------------
library(RWeka)
fourgramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4))
tdm4 <- TermDocumentMatrix(cleanSample, control = list(tokenize = fourgramTokenizer))
#see most occuring words
findFreqTerms(tdm4,60)
#make a dataframe with missing term as NA
freq <- sort(rowSums(as.matrix(rollup(tdm4, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm4freq<-data.frame(word = names(freq), freq = freq)

#tdm4freq<- data.frame(word = c(tdm4$dimnames$Terms,rep(NA,length(tdm4$v)-length(tdm4$dimnames$Terms) )), frequency = tdm4$v)
#sort on frequency descending
#tdm4freq <- plyr::arrange(tdm4freq, -frequency)
#remove add na values
#tdm4freqtemp<-tdm4freq[complete.cases(tdm4freq),]
#remove the phrases occuring less then 2 times getting 300 rows
tdm4freq<-tdm4freq[(tdm4freq$freq>1),]
saveRDS(tdm4freq, file="savedtdm4.rds")


#create 5 gram pharess
#----------------------------
library(RWeka)
fivegramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 5, max = 5))
tdm5 <- TermDocumentMatrix(cleanSample, control = list(tokenize = fivegramTokenizer))
#see most occuring words
findFreqTerms(tdm5,60)
#make a dataframe with missing term as NA
freq <- sort(rowSums(as.matrix(rollup(tdm5, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm5freq<-data.frame(word = names(freq), freq = freq)

#tdm5freq<- data.frame(word = c(tdm5$dimnames$Terms,rep(NA,length(tdm5$v)-length(tdm5$dimnames$Terms) )), frequency = tdm5$v)
#sort on frequency descending
#tdm5freq <- plyr::arrange(tdm5freq, -frequency)
#remove add na values
#tdm5freqtemp<-tdm5freq[complete.cases(tdm5freq),]
#remove the phrases occuring less then 2 times getting 130 rows
tdm5freq<-tdm5freq[(tdm5freq$freq>1),]
saveRDS(tdm5freq, file="savedtdm5.rds")


