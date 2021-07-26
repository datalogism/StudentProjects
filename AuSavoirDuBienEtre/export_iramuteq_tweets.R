library(RODBC)
library(anytime)
library(jsonlite)
library(textcat)
library(stringr)

dirs=list.dirs("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/")
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase_Final.accdb")


### EXPORT HASTAGS
con<-odbcConnectAccess2007(db)
fileConn<-file("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/iramuteq/201504_periods.txt","w")
rq=paste("SELECT Period.Year, Keywords.keyword_label, Keywords.polarity, Cities.[city name], Cities.country_id, Twitter_data.lang_id,tweet_content_orig,
         Twitter_data.hastag_contened FROM Period INNER JOIN (Keywords 
         INNER JOIN ((Countries INNER JOIN Cities ON Countries.country_id = Cities.country_id) 
         INNER JOIN Twitter_data ON Cities.city_id = Twitter_data.city_id) 
         ON Keywords.keyword_id = Twitter_data.keyword_id) ON Period.period_id = Twitter_data.period_id
        WHERE (((Twitter_data.date_tweet)>#4/1/2015# And (Twitter_data.date_tweet)<#5/1/2015#));")
res=sqlQuery(con,rq)

close(con) 

for(row in 1:nrow(res)){
  year=as.character(res[row,"Year"])
  country=as.character(res[row,"country_id"])
  city=as.character(res[row,"city name"])
  keyword=as.character(res[row,"keyword_label"])
  lang=as.character(res[row,"lang_id"])
  hastags=as.character(res[row,"tweet_content_orig"])
  
  polarite=as.character(res[row,"polarity"])
  hastags=trimws(gsub(";"," ",hastags))
  content <- json_data_frame[row,"tweet_content"]
  vars<-paste(c("****",year,country,polarite), collapse=" *")
  write(vars, file=fileConn, append=T)
  write(hastags, file=fileConn, append=T)
 
}
close(fileConn)
#### EXPORT TWEETS CONTENT
con<-odbcConnectAccess2007(db)
fileConn<-file("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/iramuteq/tweets_corpus.txt","w")
rq=paste("SELECT Period.Year, Keywords.keyword_label, Keywords.polarity, Cities.[city name], Cities.country_id, Twitter_data.lang_id, Twitter_data.tweet_content_clean FROM Period INNER JOIN (Keywords INNER JOIN ((Countries INNER JOIN Cities ON Countries.country_id = Cities.country_id) INNER JOIN Twitter_data ON Cities.city_id = Twitter_data.city_id) ON Keywords.keyword_id = Twitter_data.keyword_id) ON Period.period_id = Twitter_data.period_id;")
res=sqlQuery(con,rq)
close(con) 
Twitter_data.tweet_content_clean
for(row in 1:nrow(res)){
  year=as.character(res[row,"Year"])
  country=as.character(res[row,"country_id"])
  city=as.character(res[row,"city name"])
  keyword=as.character(res[row,"keyword_label"])
  lang=as.character(res[row,"lang_id"])
  content_clean=as.character(res[row,"tweet_content_clean"])
  
  polarite=as.character(res[row,"polarity"])
  content_clean=trimws(content_clean)
  vars<-paste(c("****",year,country,city,keyword,polarite,lang), collapse=" *")
  write(vars, file=fileConn, append=T)
  write(content_clean, file=fileConn, append=T)
  
}
close(fileConn)


#### EXPORT WEBSITE CONTENT
con<-odbcConnectAccess2007(db)
fileConn<-file("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/iramuteq/website_corpus.txt","w")
rq=paste("SELECT GoogleCSE_data_results.content, GoogleCSE_data_results.lang_website, GoogleCSE_data_queries.country_id, Keywords.polarity, Keywords.keyword_label, Period.year
FROM Period INNER JOIN (Keywords INNER JOIN (GoogleCSE_data_queries INNER JOIN GoogleCSE_data_results ON GoogleCSE_data_queries.cse_query_id = GoogleCSE_data_results.cse_query_id) ON Keywords.keyword_id = GoogleCSE_data_queries.keyword_id) ON Period.period_id = GoogleCSE_data_queries.period_id WHERE GoogleCSE_data_results.content<>'';")
res=sqlQuery(con,rq)
close(con) 
Twitter_data.tweet_content_clean
for(row in 1:nrow(res)){
  year=as.character(res[row,"Year"])
  country=as.character(res[row,"country_id"])
  keyword=as.character(res[row,"keyword_label"])
  lang=as.character(res[row,"lang_website"])
  content_clean=as.character(res[row,"content"])
  
  polarite=as.character(res[row,"polarity"])
  content_clean=trimws(content_clean)
  vars<-paste(c("****",year,country,keyword,polarite,lang), collapse=" *")
  write(vars, file=fileConn, append=T)
  write(content_clean, file=fileConn, append=T)
  
}
close(fileConn)


