library(RODBC)
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase.accdb")
con<-odbcConnectAccess2007(db)


periods=sqlQuery(con,"SELECT Keywords.polarity,COUNT(Keywords.polarity) FROM Twitter_data JOIN Keywords ON Twitter_data.keywords_id = Keywords.keyword_id GROUP BY polarity;
")