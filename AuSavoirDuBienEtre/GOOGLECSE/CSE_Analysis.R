########### LOAD LIBRARIES
library(RODBC)
library(anytime)
library(stringr)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)

### GET FILE AND BDD CONNECTION
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase_08_04.accdb")
con<-odbcConnectAccess2007(db)

################################################## ANALYSE SETS DE SITES WEBS
rq="SELECT GoogleCSE_data_queries.country_id, GoogleCSE_data_queries.period_id, GoogleCSE_data_results.url
FROM (Period INNER JOIN (Keywords INNER JOIN GoogleCSE_data_queries ON Keywords.keyword_id = GoogleCSE_data_queries.keyword_id) ON Period.period_id = GoogleCSE_data_queries.period_id) INNER JOIN GoogleCSE_data_results ON GoogleCSE_data_queries.cse_query_id = GoogleCSE_data_results.cse_query_id;"

# GET QUERY RESULT
res=sqlQuery(con,rq)
close(con) 
res=as.data.frame(res)


uniques_global=levels(res$url)

#### GET UNIQUE LINK FOR EACH COUNTRY
BJ_data= unique(as.vector(res[which(res["country_id"]=='BJ'),"url"]))
FR_data= unique(as.vector(res[which(res["country_id"]=='FR'),"url"]))


#### GET UNIQUE LINK FOR EACH YEAR
data2015= unique(as.vector(res[which(res["period_id"]=='12'),"url"]))
data2016= unique(as.vector(res[which(res["period_id"]=='13'),"url"]))
data2017= unique(as.vector(res[which(res["period_id"]=='14'),"url"]))

### GET ONLY BJ/FR LINKS
only_BJ=setdiff(uniques_global,BJ_data )
only_FR=setdiff(uniques_global,FR_data )


### GET ONLY FOR YEARS LINKS
only_2015=intersect(uniques_global,data2015 )
only_2016=setdiff(uniques_global,data2016 )
only_2017=setdiff(uniques_global,data2017 )


########################################### ANALYSE PAR MOTS CLEFS
con<-odbcConnectAccess2007(db)
rq="SELECT Period.year, Countries.country_id, Keywords.polarity, Sum(GoogleCSE_data_queries.nb_total_results) AS SumOfnb_total_results
FROM Period INNER JOIN (Keywords INNER JOIN (Countries INNER JOIN GoogleCSE_data_queries ON Countries.country_id = GoogleCSE_data_queries.country_id) ON Keywords.keyword_id = GoogleCSE_data_queries.keyword_id) ON Period.period_id = GoogleCSE_data_queries.period_id
GROUP BY Period.year, Countries.country_id, Keywords.polarity;"
res=sqlQuery(con,rq)
close(con) 

################################### BY POLARITY FOR FRANCE RESULTS
res=as.data.frame(res)
A40<-filter(res, country_id=="FR")
A41<-group_by_at(A40, vars(year, polarity))
A42 <- summarize(A41, Frequency = sum(SumOfnb_total_results))
AAA <- mutate(A42,freq = Frequency /sum(Frequency))

ggplot() + geom_bar(aes(y = freq, x = year, fill = polarity), data = AAA,
                    stat="identity")



################################### BY INDEX FOR FRANCE RESULTS
con<-odbcConnectAccess2007(db)
rq="SELECT GoogleCSE_data_results.source, GoogleCSE_data_queries.country_id, Avg(GoogleCSE_data_results.index_result) AS AvgOfindex_result
FROM Keywords INNER JOIN (GoogleCSE_data_queries INNER JOIN GoogleCSE_data_results ON GoogleCSE_data_queries.cse_query_id = GoogleCSE_data_results.cse_query_id) ON Keywords.keyword_id = GoogleCSE_data_queries.keyword_id
GROUP BY GoogleCSE_data_results.source, GoogleCSE_data_queries.country_id ;"
res=sqlQuery(con,rq)
close(con) 

A40<-filter(res, country_id=="FR")

top_50=top_n(A40,50,AvgOfindex_result)
A83 <- arrange(top_50, desc(AvgOfindex_result))
A83$source <- factor( A93$source, levels =  A93$source)

ggplot(data=A83, aes(x= source, y=AvgOfindex_result)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE) +
  coord_flip()


################################### BY POLARITY FOR BJ
con<-odbcConnectAccess2007(db)
rq="SELECT Keywords.keyword_label, Sum(GoogleCSE_data_queries.nb_total_results) AS SumOfnb_total_results
FROM Keywords INNER JOIN GoogleCSE_data_queries ON Keywords.keyword_id = GoogleCSE_data_queries.keyword_id
GROUP BY Keywords.keyword_label;"
res=sqlQuery(con,rq)
close(con) 



A90<-filter(res,country_id=="BJ")
A91<-group_by(res, keyword_label)
A93 <- arrange(A91, desc(SumOfnb_total_results))
A93$keyword_label <- factor( A93$keyword_label, levels =  A93$keyword_label)
ggplot(data=A93, aes(x=keyword_label , y=SumOfnb_total_results, fill=keyword_label, label = SumOfnb_total_results)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE) +
  coord_flip()



