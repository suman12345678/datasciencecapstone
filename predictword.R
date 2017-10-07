#load ngrams saved earlier
#-------------------------
#setwd("C:\\Users\\suman\\Desktop\\datasciencecoursera\\capstone\\shiny")
if(!exists("savedtdm1")){
  savedtdm1 <- readRDS("savedtdm1.rds")
}
if(!exists("savedtdm2")){
  savedtdm2 <- readRDS("savedtdm2.rds")
}  
if(!exists("savedtdm3")){
  savedtdm3 <- readRDS("savedtdm3.rds")
}  
if(!exists("savedtdm4")){
    savedtdm4 <- readRDS("savedtdm4.rds")
}    
if(!exists("savedtdm5")){
  savedtdm5 <- readRDS("savedtdm5.rds")
}  
#library(stringr)


#clean input proc
#------------------------------
cleaninput<-function(xx){
  #trim and make to lower case
  xx<-trimws(tolower(xx))
  library(tm)
  mystopwords<-c("a","an","the")
  nostopword<-c()
  x<-unlist(strsplit(xx," "))
  
  #**for english stopword removal
  xx<-x[!x %in% stopwords(kind="en")]
  
  #for mystopeord removal
  #xx<-x[!x %in% mystopwords]
  
  #for nostopword
  #xx<-x[!x %in% nostopword]
  
  #convert to string
  xx<-paste(xx,collapse = " ")
  if (length(xx)==0) xx<-""
  
  #print(xx)
  # remove non-word charactters from user input
  charsToClean <- c("[:cntrl:]", "[:punct:]")
  for (i in 1:length(charsToClean)){
    xx <- gsub(paste0("[", charsToClean[i], "]"),  "", xx, fixed=FALSE)
  }
  #print(xx)
  xx
}



#ngram common module pass user input and vector of words formed
#-------------------------------------------------------------
ngram<-function(word,noofpred,gram){
  matches<-c()
  file<-get(paste0("savedtdm",gram))
  #take last gram-1 words from user
  x<-strsplit(word," ")
  len<-length(x[[1]])
  temp=""
  #print("check in ngram ")
  #print(gram)
  #when gram=1
  if (gram==1){
    for(i in 1:nrow(savedtdm1)){
        matches<-c(matches,as.vector(file[i,1]))
        #print(matches)
        if(length(matches)==noofpred) break
    }
    return(matches)
  }
  
  #take the last gram-1 word from the user input for gram > 1 and reduce size of word 
  for (j in 1:(gram-1)){
    #print("debug")
    #print(j)
    #print(len-gram+j+1)
    if(temp=="")  { temp<-x[[1]][len-gram+j+1] }
    else { temp<-paste(temp,x[[1]][len-gram+j+1],sep=" ") }
  }
  word<-temp
  #word<-paste(x[[1]][len-2],x[[1]][len-1],x[[1]][len],sep=" ")
  
  #search in ngram this taking time
  #for(i in 1:nrow(file)){
    #print(word)
  #  if (grepl(paste0('\\<',word," "), as.vector(file[i,1]))) {
  #    aa<-strsplit(as.vector(file[i,1]), " ")
  #    matches<-c(matches,aa[[1]][gram])
  #    if(length(matches)==noofpred) break
  #  }  
  #}
  
  #search in ngram with lapply
  fun<-function(x,word){
    #print("search in 2")
    #print(word)
    #print(x)
    if(grepl(paste0('\\<',word," "),x)){
      aa<-strsplit(as.vector(x)," ")
      x<-c(aa[[1]][length(aa[[1]])])
      #print("match found so pass")
      #print(aa)
    }
    else x<-c(NA)
    x
  }
  t<-as.vector(file[,1])
  t1<-lapply(t,fun,word=word)
  t2<-as.vector(t1)
  t3<-t2[!is.na(t2)]
  #print("match obj")
  #print(gram)
  #print(t3)
  if (length(t3)>noofpred) matches<-t3[1:noofpred]
  else matches<-t3
  
  
  
  matches
}






#logic followed
#if >=3 words check in 4gram with last 3 words
#if not found in 4gram or user input 2 check in 3 gram with last 2 word
#if not found in 3gram or user input 1 check in 2 gram with last 1 word
#if not found in 2 gram or user no input check in 1 gram and show



#predict next word with input as sentence or word and no of pred
#----------------------------------------------------------------
predictnextword<-function(xx,noofpred=5){
  xx<-cleaninput(xx)
  len<-length(strsplit(xx," ")[[1]])
  #logic followed
  #if >=3 words check in 4gram with last 3 words
  #if not found in 4gram or user input 2 check in 3 gram with last 2 word
  #if not found in 3gram or user input 1 check in 2 gram with last 1 word
  #if not found in 2 gram or user no input check in 1 gram and show
  vec<-c()
  #print(len)
  if (len>=4){
    #print("checking in 5")
    vec<-ngram(xx,noofpred,5)
    #print(vec)
  }
  #if(len>=3 & length(vec)<noofpred){
  if(len>=3 & (length(vec)<noofpred)){
    #print("checking in 4")
    tmp<-ngram(xx,noofpred,4)
    vec<-c(vec,tmp)
    #print("from 4")
    #print(vec)
  }  
  #if(len>=2 & length(vec)<noofpred){
  if(len>=2 & (length(vec)<noofpred)){
    tmp<-ngram(xx,noofpred,3)
    #print("checking in 3")
    vec<-c(vec,tmp) 
    #print("from 3")
    #print(vec)
  }  
  #if(len>=1 & length(vec)<noofpred){
  if(len>=1 & (length(vec)<noofpred)){
    #print("checking in 2")
    tmp<-ngram(xx,noofpred,2)
    vec<-c(vec,tmp) 
    #print("from 2")
    #print(vec)
  }
  if(length(vec)<noofpred){
    #print("checking in 1")
    tmp<-ngram(xx,noofpred,1) 
    vec<-c(vec,tmp)
    #print(vec)
  }
  if (length(vec)>noofpred) vec<-vec[1:noofpred]
  #get unique
  vec<-unique(vec)
  
  
  vec
}  
