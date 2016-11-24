library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

#get parameter files
logs<-list.files("C:/MotionMeerkat/cuts",recursive = T,pattern="log",full.names=T)

parseLog<-function(log){
  
  x<-readLines(log)
  
  #parse :
  pars2<-sapply(x,function(y){
    str_match(y,": (\\w+)")[[2]]
  })
  
  pars1<-sapply(x,function(y){
    str_match(y,"(^.+):")[[2]]
  })
  
  parframe<-data.frame(Par=pars1,value=pars2)
  
  #remove empty lines
  parframe<-parframe %>% filter(!is.na(value))
  parframe<-parframe[-c(2:3),]
  
  return(parframe)
  }

#run on all logs
parsed<-lapply(logs,parseLog)

#name them
names(parsed)<-str_match(logs,"/(\\w+)/Parameters_Results")[,2]

dat<-melt(parsed)

colnames(dat)[3]<-"Video"

dat<-spread(dat,Par,value)
ggplot(dat,aes(x=Video,y=as.numeric(Hitrate))) + geom_point() + labs(y="Frames Returned (%)") + theme_bw() + coord_flip()
