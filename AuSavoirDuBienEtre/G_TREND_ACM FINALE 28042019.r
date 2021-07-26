#Chargement des packages
library("questionr")
library("FactoMineR")
library ("factoextra")
library("corrplot")


##### Importation des donnees et selection des elements ----
setwd("F:/Texte/Documents textes/ETUDES/M1 HUMANITES NUMERIQUES/COURS/recherches dossiers transversal/GOOGLE TRENDS")
d<- read.csv2("DataGoogleTrendsBJFROK.csv",header=TRUE,dec=";")
head(d)
str(d)

#ACM----
names(d)
d.analyse<-d[, colnames(d) %in% c("country_id","keyword_label","polarity")]

res.mca <- MCA (d.analyse, graph = FALSE)  #calcul de l'ACM 
print(res.mca) 

# Visualider les valeurs propres
eig.val <- get_eigenvalue(res.mca) 
eig.val #On choisit donc deux axes

fviz_eig(res.mca, addlabels = TRUE, ylim = c(0, 10))

#Visualisation des individus 
windows()
fviz_mca_ind (res.mca, select.ind = list(cos2 = 75), col.ind = "contrib",#Visualiser les 75 variables les mieux représentées en les coloriant selon leur contribution
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

#Modalites 
var <- get_mca_var(res.mca)
var
#Axe 1
coord<-var$coord[,1]
contrib<-var$contrib[,1]
cos2<-var$cos2[,1]
display<-cbind(coord,contrib,cos2)

#Axe2
coord<-var$coord[,2]
contrib<-var$contrib[,2]
cos2<-var$cos2[,2]
display<-cbind(coord,contrib,cos2)

###### COS2

corrplot(var$cos2, is.corr=FALSE)# graphique du Cos2 des points colonnes sur tous les axes
fviz_cos2(res.mca, choice = "var")# graphique du Cos2 des points colonnes sur le 1er axe
fviz_cos2(res.mca, choice = "var", axes = 2)# graphique du Cos2 des points colonnes cumulé sur les 2 premiers axes
corrplot(var$contrib, is.corr=FALSE)# graphique de la contributions des points colonnes sur tous les axes
fviz_contrib(res.mca, choice = "var")# graphique de la contribution des points colonnes sur le 1er axe
fviz_contrib(res.mca, choice = "var", axes = 2)# graphique de la contribution des points colonnes cumulées sur les 2 premiers axes

##### Visualisation des modalites contribuants aux axes
fviz_cos2(res.mca, choice = "var", top = 10)
fviz_cos2(res.mca, choice = "var", axes = 2, top=10)

fviz_contrib(res.mca, choice = "var", axes = 1, top = 10)# Visualisation des 10 modalites contribuants le plus sur l'axe 1. La ligne en pointillé rouge indique la contribution moyenne
fviz_contrib(res.mca, choice = "var", axes = 2, top = 10)# Visualisation des 10 modalites contribuants le plus sur l'axe 2. La ligne en pointillé rouge indique la contribution moyenne 


##### Graphique des profils colonnes
fviz_mca_var(res.mca, col.var = "contrib", #graphique des profils colonnes en les coloriant selon leur contribution
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE)
fviz_mca_var(res.mca, alpha.var = "contrib",repel=TRUE)# graphique des profils colonnes en changeant la transparence en fonction de la contribution

fviz_mca_var(res.mca, col.var = "cos2", #Points colorés selon leur cosinus²
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

res.mca$quanti # Variables quantitatives supplémentaires
fviz_mca_var(res.mca, choice = "quanti.sup") #Graphique des vars quantis supplémentaires
