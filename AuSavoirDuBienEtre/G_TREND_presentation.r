##### Importation des donnees et selection des elements ----

setwd("F:/Texte/Documents textes/ETUDES/M1 HUMANITES NUMERIQUES/COURS/recherches dossiers transversal/GOOGLE TRENDS")
d<- read.csv2("DataGoogleTrendsBJFROK.csv",header=TRUE,dec=";")
head(d)
str(d)
names(d)

#Representation de l'effectif d'une variable qualitative
barplot(table(d$polarity), col=c.asc, main = "Polarité des tweets au Benin et en France", xlab = "", ylab = "")

#Representation de l'eff d'une variable qualitative que pour le Benin (par exemple)
#---> Faire une sous-population
d.Benin <- d[d$country_id == 'BJ',]
edit(d.Benin)
barplot(table(d.Benin$polarity), col=c.asc, main = "Polarité des tweets au Benin", xlab = "", ylab = "")
