################# USEFULL LIBS
library(anytime)
library(jsonlite)

#### FILE CONNECTION
dirs=list.dirs("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/")
interesting_dirs=dirs[3:12]
tweeters=c("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/twitter/2015","C:/Users/Celian/Desktop/BienEtreSubjectif/Data/twitter/2016","C:/Users/Celian/Desktop/BienEtreSubjectif/Data/twitter/2017")

fileConn<-file("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/tweet_corpus.txt","w")



#### FOR EACH DIR
for(twt_dir in tweeters){
  
  
  files=list.files(twt_dir)

#### FOR EACH FILES
  for(f in files){
    
#### GET INFO IN FILENAME
    split <- strsplit(f, "_")
    year=split[[1]][1]
    country_id=split[[1]][2]
    polarite=split[[1]][3]
    city=split[[1]][4]
    city=gsub(" ","_",city)
    keyword=split[[1]][5]
    if(length(split[[1]])>5){
      keyword=paste(keyword,split[[1]][6])
    }
    keyword=gsub(".json","",keyword)
    keyword=gsub(" ","_",keyword)
    print(paste(year,country_id,polarite,city,keyword,sep=" -- "))
    json_file=paste(twt_dir,f,sep="/")
    print(json_file)
    content<- paste(readLines(json_file), collapse="")

#### READ FILE
    json_data <- fromJSON(content)
    json_data_frame <- as.data.frame(json_data)
    
#### EXPORT CONTENT
    for (row in 1:nrow(json_data_frame)) {
      content <- json_data_frame[row,"text"]
      vars<-paste(c("****",year,country_id,city,keyword,polarite), collapse=" *")
      write(vars, file=fileConn, append=T)
      write(content, file=fileConn, append=T)
    }
  }
  
}
close(fileConn)
