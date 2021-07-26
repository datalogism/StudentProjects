##### USEFULL LIBS
library(RODBC)
library(anytime)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

#### DB CONNECTION
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase_Final.accdb")
con<-odbcConnectAccess2007(db)


############ EXPORT DATA BY DATE  FOR TWEETS FR
rq="SELECT Cities.country_id, DateValue([date_tweet]) AS Expr1, Sum(Twitter_data.retweets) AS SumOfretweets, Sum(Twitter_data.likes) AS SumOflikes, Cities.city_id, Cities.[city name], Cities.city_lat, Cities.city_long, Keywords.polarity, Count(Twitter_data.tweet_id) AS CountOftweet_id, Keywords.keyword_label
FROM Keywords INNER JOIN (Cities INNER JOIN Twitter_data ON Cities.city_id = Twitter_data.city_id) ON Keywords.keyword_id = Twitter_data.keyword_id
GROUP BY Cities.country_id, DateValue([date_tweet]), Cities.city_id, Cities.[city name], Cities.city_lat, Cities.city_long, Keywords.polarity, Keywords.keyword_label
HAVING (((Cities.country_id)='FR'));"
res=sqlQuery(con,rq)
close(con) 

library(jsonlite)

write_json(res,"C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Geo_data.json", pretty = FALSE, auto_unbox = FALSE)


############ EXPORT DATA BY DATE  FOR TWEETS BJ
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase_Final.accdb")

con<-odbcConnectAccess2007(db)
rq="SELECT Cities.country_id, DateValue([date_tweet]) AS Expr1, Sum(Twitter_data.retweets) AS SumOfretweets, Sum(Twitter_data.likes) AS SumOflikes, Cities.city_id, Cities.[city name], Cities.city_lat, Cities.city_long, Keywords.polarity, Count(Twitter_data.tweet_id) AS CountOftweet_id, Keywords.keyword_label
FROM Keywords INNER JOIN (Cities INNER JOIN Twitter_data ON Cities.city_id = Twitter_data.city_id) ON Keywords.keyword_id = Twitter_data.keyword_id
GROUP BY Cities.country_id, DateValue([date_tweet]), Cities.city_id, Cities.[city name], Cities.city_lat, Cities.city_long, Keywords.polarity, Keywords.keyword_label
HAVING (((Cities.country_id)='BJ'));"
res=sqlQuery(con,rq)
close(con) 

library(jsonlite)

write_json(res,"C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Geo_data_BJ.json", pretty = FALSE, auto_unbox = FALSE)