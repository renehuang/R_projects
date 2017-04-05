count<-function(input_text,output_text){
  
}
setwd("C:\\UCLA fall\\stat102A\\HW3")
x<-scan("HW3Supplement_const.txt",character(0))
list<-NULL
for(i in 1:length(x)){
  count=0
  for(j in i:length(x)){
    if(x[j]==x[i]){
      count<-count+1
    }
  }
  factor<-c(x[i],count)
  list<-rbind(list,factor)
}

for(i in 1:dim(list)[1]){
  del_num=NULL
  for(j in i:dim(list)[1]){
    if(list[j,1]==list[i,1]){
      del_num<-c(del_num,j)
    }
  }
  list<-list[-del_num,]
}