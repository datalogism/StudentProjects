library(RODBC)
library(anytime)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)

library(lubridate)

db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase_Final.accdb")

con<-odbcConnectAccess2007(db)
rq="SELECT DateValue(Twitter_data.date_tweet) as tweet_day, count(Twitter_data.date_tweet) as nb
FROM  (Countries INNER JOIN Cities ON Countries.country_id = Cities.country_id) INNER JOIN Twitter_data ON Cities.city_id = Twitter_data.city_id 
WHERE Countries.country_id='FR' GROUP BY DateValue(Twitter_data.date_tweet) ;"
res=sqlQuery(con,rq)
close(con) 

r <- decompose(ts(res, start = c(2015, 01,01), end=c(2018, 2,18), frequency=12 ))
r$seasonal
plot(r$seasonal)
result_acf=acf(res)

result_pacf=pacf(res)

plot(res, main = "original")


con<-odbcConnectAccess2007(db)
rq="SELECT DateValue(Twitter_data.date_tweet) as day,Month(DateValue(Twitter_data.date_tweet)) as month,Year(DateValue(Twitter_data.date_tweet)) as year, Keywords.keyword_label, Keywords.polarity , COUNT(Twitter_data.date_tweet) as nb_twt , sum(retweets) as nb_rtwt, sum(likes) as nb_likes, sum(replies)  as nb_replies FROM Keywords INNER JOIN Twitter_data ON Keywords.keyword_id = Twitter_data.keyword_id GROUP BY DateValue(Twitter_data.date_tweet), Keywords.keyword_label, Keywords.polarity;"
res_pos_by_day_by_polarity=sqlQuery(con,rq)
close(con) 

r <- decompose(res_pos_by_day_by_polarity)
result_acf=acf(res_pos_by_day_by_polarity)

result_pacf=pacf(res_pos_by_day_by_polarity)

result_acf=acf(res_pos_by_day_by_polarity)
cormat <- round(cor(res_pos_by_day_by_polarity[,4:7]),2)
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
# Obtenir le triangle supérieur
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
upper_tri
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)
library(ggplot2)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()


con<-odbcConnectAccess2007(db)
rq="SELECT  Keywords.keyword_label, Keywords.polarity, COUNT(Twitter_data.date_tweet) as nb_twt , sum(retweets) as nb_rtwt, sum(likes) as nb_likes, sum(replies)  as nb_replies FROM Keywords INNER JOIN Twitter_data ON Keywords.keyword_id = Twitter_data.keyword_id GROUP BY DateValue(Twitter_data.date_tweet), Keywords.keyword_label, Keywords.polarity;"
res_pos_by_day_by_polarity=sqlQuery(con,rq)
close(con) 


  ggplot(res_pos_by_day_by_polarity, aes(x=log(nb_twt), y = log(nb_likes), color=polarity, shape=polarity)) +
    geom_point() + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  
  ggplot(res_pos_by_day_by_polarity, aes(x=log(nb_twt), y = log(nb_replies), color=polarity, shape=polarity)) +
    geom_point() + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  
  
  ggplot(res_pos_by_day_by_polarity, aes(x=log(nb_twt), y = log(nb_likes), color=keyword_label, shape=keyword_label)) +
    geom_point() + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  
  ggplot(res_pos_by_day_by_polarity, aes(x=log(nb_twt), y = log(nb_replies), color=keyword_label, shape=keyword_label)) +
    geom_point() + 
    geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
  
  
  con<-odbcConnectAccess2007(db)
  rq="SELECT  Keywords.keyword_label, COUNT(Twitter_data.date_tweet) , Keywords.polarity, COUNT(Twitter_data.date_tweet) as nb_twt , sum(retweets) as nb_rtwt, sum(likes) as nb_likes, sum(replies)  as nb_replies FROM Keywords INNER JOIN Twitter_data ON Keywords.keyword_id = Twitter_data.keyword_id GROUP BY DateValue(Twitter_data.date_tweet), Keywords.keyword_label, Keywords.polarity;"
  res=sqlQuery(con,rq)
  close(con) 
  
  p <- ggplot(res, aes(keyword_label, log(nb_twt),color=polarity))
  p + geom_boxplot() + coord_flip()
  