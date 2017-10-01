#load ngrams saved earlier
#-------------------------
setwd("C:\\Users\\suman\\Desktop\\datasciencecoursera\\capstone")
savedtdm1 <- readRDS("savedtdm1.rds")
savedtdm2 <- readRDS("savedtdm2.rds")
savedtdm3 <- readRDS("savedtdm3.rds")
savedtdm4 <- readRDS("savedtdm4.rds")
library(stringr)

xx="of assistantopinionretreat via % $ #"

#clean input proc
#------------------------------
cleaninput<-function(xx){
  # remove non-word charactters from user input
  charsToClean <- c("[:cntrl:]", "[:punct:]")
  for (i in 1:length(charsToClean)){
    xx <- gsub(paste0("[", charsToClean[i], "]"),  " ", xx, fixed=FALSE)
  }
  xx
}

xx<-cleaninput(xx)



# break user input of n words to 1,2,3,4 words and create list xgram maybe not required
#-------------------------------------------------------------------
splitinput<-function(xx,n=1:4){
  xgramRange <- (n - 1)
  xgramRange <- xgramRange[xgramRange > 0]
  xgrams <- lapply(xgramRange, function(n) {
    if (n > 0) {
      # xgram regex matches 1 .. n-1 words
      xgram <- regmatches(xx, regexec(paste0("^.*\\b(", paste(rep("\\w+ +", n-1), collapse=""), "\\w+) *$"), xx))
      # drop first element i.e inputText, keep only the words part
      xgram[[1]][-1]
    }
  else character(0)
  })
  xgrams
}  
  
xx<-splitinput(xx) 



#logic followed
#if >=3 words check in 4gram with last 3 words
#if not found in 4gram or user input 2 check in 3 gram with last 2 word
#if not found in 3gram or user input 1 check in 2 gram with last 1 word
#if not found in 2 gram or user no input check in 1 gram and show



#predict next word 
#--------------------
predictnextword<-function(xx,n=1:4){
  xx<-cleaninput(xx)
  xx<-trimws(xx,which = "both")
  len<-length(strsplit(xx," ")[[1]])
  
  
}  

for(i in 1:nrow(savedtdm1)){
  print(as.vector(savedtdm1[i,1]))
  if(as.vector(savedtdm1=="supports")) print("yesss")
}  

#check in 2 gram with user input 1)text 2)no of pred
----------------------------------------------------
twogram<-function(word,noofpred){
  matches<-c()
  #take last 1 word from user
  x<-strsplit(word," ")
  len<-length(x[[1]])
  word<-x[[1]][len]
  for(i in 1:nrow(savedtdm2)){
    #print(as.vector(savedtdm2[i,1]))
    if (grepl(paste0('\\<',word," "), as.vector(savedtdm2[i,1]))) {
      aa<-strsplit(as.vector(savedtdm2[i,1]), " ")
      #print("yes")
      #break
      matches<-c(matches,aa[[1]][2])
      #print(aa)
      #print(class(aa))
      #print(matches)
      if(length(matches)==noofpred) break
    }  
  }
  matches
}

#check in 3 gram with user input 1)text 2)no of pred
----------------------------------------------------
threegram<-function(word,noofpred){
    matches<-c()
    #take last 2 words from user
    x<-strsplit(word," ")
    len<-length(x[[1]])
    word<-paste(x[[1]][len-1],x[[1]][len],sep=" ")
    for(i in 1:nrow(savedtdm3)){
      if (grepl(paste0('\\<',word," "), as.vector(savedtdm3[i,1]))) {
        aa<-strsplit(as.vector(savedtdm3[i,1]), " ")
        matches<-c(matches,aa[[1]][3])
        if(length(matches)==noofpred) break
      }  
    }
    matches
}

#check in 4 gram with user input 1)text 2)no of pred
----------------------------------------------------
fourgram<-function(word,noofpred){
    matches<-c()
    #take last 3 words from user
    x<-strsplit(word," ")
    len<-length(x[[1]])
    word<-paste(x[[1]][len-2],x[[1]][len-1],x[[1]][len],sep=" ")
    for(i in 1:nrow(savedtdm4)){
      if (grepl(paste0('\\<',word," "), as.vector(savedtdm4[i,1]))) {
        aa<-strsplit(as.vector(savedtdm4[i,1]), " ")
        matches<-c(matches,aa[[1]][4])
        if(length(matches)==noofpred) break
      }  
    }
    matches
}


xx="of assistantopinionretreat via% I am drinking"
predictnextword(xx)

y="hi there"
makeXgrams(y)



word<-strsplit(x," ")
length(word[[1]])

