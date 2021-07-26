install.packages('rvest')
library(rvest)
library(XML)

url="http://www.univ-lyon2.fr/formation/"

page<-htmlParse(url) 
page<-html(url)
page%>%  html_nodes("div.sansstyle a") %>% html_text()
page%>%  html_nodes("div.sansstyle a") %>% html_attr("href")


