library(RODBC)
#2016
#https://www.microsoft.com/en-us/download/details.aspx?id=54920
#DB connection
db<-file.path("C:/Users/Celian/Desktop/BienEtreSubjectif/Data/Bdd Access/HapynessDataBase.accdb")
con<-odbcConnectAccess2007(db)

#GET ALL TABLES NAMES
tables<-sqlTables(con,tableType = "TABLE")
tables[3]

#Get TABLE
Countries <- sqlFetch(con,"Countries")
#GET COLUMNS NAMES
columns<-sqlColumns(con, "Countries", errors = FALSE, as.is = TRUE,special = FALSE, catalog = NULL, schema = NULL,literal = FALSE)
columns[4]

sqlQuery(con,"")
### TEST INSERT

odbcSetAutoCommit(con, autoCommit = TRUE)


close(con) 
library("readxl")
# xls files
my_data <- read_excel("my_file.xls")
# xlsx files