#load data and library
#---------------------
library(tm)
library(stringi)
library(slam)
library(RWeka)

setwd("C:\\Users\\suman\\Desktop\\datasciencecoursera\\capstone")
if(!file.exists("./Coursera-SwiftKey.zip")){
  download.file(Url,destfile="./Coursera-SwiftKey.zip",mode = "wb")
}
if(!file.exists("./final")){
  unzip(zipfile="./Coursera-SwiftKey.zip",exdir=".")
}

#read the file with readLines
#-----------------------------
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
#**change for more sample
#------------------------
sampleTwitter<-lineTwitter[sample(1:length(lineTwitter),50000)]
sampleBlogs<-lineBlogs[sample(1:length(lineBlogs),50000)]
sampleNews<-lineNews[sample(1:length(lineNews),50000)]
testsample<-c(sampleTwitter,sampleBlogs,sampleNews)

#creating corpus and clean using tm pkg
#--------------------------------------
cleanSample <- VCorpus(VectorSource(testsample))
rm(sampleBlogs)
rm(sampleNews)
rm(sampleTwitter)
rm(testsample)
#view one content of VCorpus
#as.character(cleanSample[[1]])
#remove number,punctuation,lowercase transform,url,twitter,hashtag
cleanSample<-tm_map(cleanSample,content_transformer(tolower))
cleanSample <- tm_map(cleanSample, content_transformer(removePunctuation))
cleanSample <- tm_map(cleanSample, content_transformer(removeNumbers))
urlremove <- function(x) gsub("http[^[:space:]]*", "", x)
specialcharremove<-function(x)      gsub("[[:punct:]]*", "", x) 
hashtagremove<- function(x) gsub("#\\S+", "", x)
twitterremove<- function(x) gsub("@\\S+", "", x)
mystopword<-c("a","an","the")
#special words remove
cleanSample <- tm_map(cleanSample, removeWords, c("twitter"))
cleanSample<-tm_map(cleanSample,content_transformer(urlremove))
cleanSample<-tm_map(cleanSample,content_transformer(specialcharremove))

#**if stopwords engligh or mystopword need to remove change here
cleanSample <- tm_map(cleanSample, removeWords, stopwords("english"))
#cleanSample <- tm_map(cleanSample, removeWords,mystopword)

#**if steming required stem the document to original form
#cleanSamplecopy<-cleanSample
#cleanSampletemp<-tm_map(cleanSample,stemDocument,language="english")
#cleanSample<-tm_map(cleanSampletemp,stemCompletion,dictionary=[cleanSamplecopy)

cleanSample <- tm_map(cleanSample, stripWhitespace)
saveRDS(cleanSample,"savedcleansample.rds")

#create function for ngram
#--------------------------
ngramTokenizer <- function(x,n) NGramTokenizer(x,Weka_control(min = n, max = n))

#1 gram word making
#-----------------------------------
#create tdm for occurance > 5000
tdm1 <- TermDocumentMatrix(cleanSample,control=list(wordLengths=c(1, 10), 
                           bounds = list(global = c(5000,Inf))))
#find the words that occurs more than 1000 times
findFreqTerms(tdm1,1000)
#sort data on frequency descending for huge TDM
freq <- sort(rowSums(as.matrix(rollup(tdm1, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm1freq<-data.frame(word = names(freq), freq = freq)
tdm1freq<-tdm1freq[1:10,]
saveRDS(tdm1freq, file="savedtdm1.rds")

#create 2 gram pharess
#----------------------------
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm2 <- TermDocumentMatrix(cleanSample, control = list(tokenize = BigramTokenizer))
#sort on fequency desc
freq <- sort(rowSums(as.matrix(rollup(tdm2, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm2freq<-data.frame(word = names(freq), freq = freq)

#** can be change to take more or less records
tdm2freq<-tdm2freq[(tdm2freq$freq>10),]
saveRDS(tdm2freq, file="savedtdm2.rds")

#create 3 gram pharess
#----------------------------
trigramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 3, max = 3))
tdm3 <- TermDocumentMatrix(cleanSample, control = list(tokenize = trigramTokenizer))
freq <- sort(rowSums(as.matrix(rollup(tdm3, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm3freq<-data.frame(word = names(freq), freq = freq)

#** can be change to take more or less records
tdm3freq<-tdm3freq[(tdm3freq$freq>9),]
saveRDS(tdm3freq, file="savedtdm3.rds")

#create 4 gram pharess
#----------------------------
fourgramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 4, max = 4))
tdm4 <- TermDocumentMatrix(cleanSample, control = list(tokenize = fourgramTokenizer))
#see most occuring words
findFreqTerms(tdm4,60)
freq <- sort(rowSums(as.matrix(rollup(tdm4, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm4freq<-data.frame(word = names(freq), freq = freq)

#** can be change to take more or less records
tdm4freq<-tdm4freq[(tdm4freq$freq>3),]
saveRDS(tdm4freq, file="savedtdm4.rds")


#create 5 gram pharess
#----------------------------
fivegramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 5, max = 5))
tdm5 <- TermDocumentMatrix(cleanSample, control = list(tokenize = fivegramTokenizer))
freq <- sort(rowSums(as.matrix(rollup(tdm5, 2, FUN = sum)), na.rm = T), decreasing = TRUE)
tdm5freq<-data.frame(word = names(freq), freq = freq)

#** can be change to take more or less records
tdm5freq<-tdm5freq[(tdm5freq$freq>3),]
saveRDS(tdm5freq, file="savedtdm5.rds")


