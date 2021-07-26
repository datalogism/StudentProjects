library(RODBC)
library(anytime)
library(jsonlite)
library(textcat)
library(stringr)

## CONNECTION BDD
dirs=list.dirs("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/")
interesting_dirs=dirs[3:12]
cse=c("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/CSE/2015","C:/Users/Celian/Desktop/BienEtreSubjectif/Data/CSE/2016","C:/Users/Celian/Desktop/BienEtreSubjectif/Data/CSE/2017")

db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase.accdb")
con<-odbcConnectAccess2007(db)

##### GET PERIOD REF

periods=sqlQuery(con,"SELECT * FROM Period")

findperiod <- function(period) {
  found=FALSE
  if(period=="2005"){
    return (2)
  }
  for(row in 1:nrow(periods)){
    if(period == periods[row,"year"]){
      found=TRUE
      return(periods[row,"period_id"])
    }
  }
  if(found==FALSE){
    return (0)
  }
}


close(con) 
# GET PERIOD
WHR<-read.csv("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/WHR.csv", header = TRUE,
           sep = ";",encoding="UTF-8")
idx=1
for (row in 1:nrow(WHR)) {
  if(idx!=1){
    if( nchar(WHR[row,"period_id"])==4){
      if(findperiod(WHR[row,"period_id"])!=0){
        WHR[row,"period_id"]=findperiod(WHR[row,"period_id"])
      }
      
      print(WHR[row,"period_id"])
    }
  }
  print(WHR[row,"period_id"])
  idx=idx+1
}


write.csv(WHR, file = "C:/Users/Celian/Desktop/BienEtreSubjectif/Data/WHR_OK.csv")