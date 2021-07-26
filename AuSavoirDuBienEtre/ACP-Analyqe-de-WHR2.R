library("FactoMineR")
library("factoextra")
library("missMDA")
library("ggplot2")
library("corrplot")
d<-read.csv2("C:/Users/Celian/Desktop/BienEtreSubjectif/WebIndicators_WHR2.csv",header=TRUE, dec="," )

# DEFITION NOM INDIVIDUS
rownames(d) <- d[,1]

########################## CORRECTION DONNEes POSITIVEs
d[,2]=1-d[,2]
d[,3]=1-d[,3]
d[,4]=1-d[,4]
########################## DESCRIPTION DES INDICATEURS BONHEUR
split_fct <- function(X){
  return(strsplit(as.character(x))[[1]])
}
library(stringr)

d2=cbind(d,str_split_fixed(d[,1], "_", 2))
colnames(d2)[26]<-"PAYS"
colnames(d2)[27]<-"ANNEE"
d2[5,26] <- "BJ"
# Grouped effet positif WHR
ggplot(d2, aes(fill=ANNEE, y=effet_positif, x=PAYS)) + 
  geom_bar(position="dodge", stat="identity")
# Grouped effet negatif WHR
ggplot(d2, aes(fill=ANNEE, y=effet_negatif, x=PAYS)) + 
  geom_bar(position="dodge", stat="identity")
#GROUPED Google resultats
ggplot(d2, aes(fill=ANNEE, y=Part.de.tweets.positifs, x=PAYS)) + 
  geom_bar(position="dodge", stat="identity")
#Grouped Google recherche
ggplot(d2, aes(fill=ANNEE, y=Part.de.rÃ.sultats.Google.positif, x=PAYS)) + 
  geom_bar(position="dodge", stat="identity")
#Grouped Twitter
ggplot(d2, aes(fill=ANNEE, y=Part.de.recherche.Google.positif, x=PAYS)) + 
  geom_bar(position="dodge", stat="identity")



###################### ON NE PRENDS FINALEMENT QUE LES DONNEES SANS NA
d=d[,2:14]
colnames(d)
str(d)
names(d)


#######################" GRAPHIQUE DE CORRELATION
cormat <- round(cor(d),2)
library(reshape2)
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)
upper_tri

melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
# Obtenir le triangle supérieur

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#################### DEBUT ACP
res.pca = PCA(d,quanti.sup = c(1,2,3),  ncp = 5, graph = TRUE)

eig.val <- get_eigenvalue(res.pca)
eig.val
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #graphique des valeurs propres avec la fonction fviz_eig() ou fviz_screeplot() du package factoextra

fviz_cos2(res.pca, choice = "var")# graphique du Cos2 des variables sur le 1 premier axe
fviz_cos2(res.pca, choice = "var", axes =2)# graphique du Cos2 des variables cumul??? sur les 2 premiers axes

corrplot(var$cos2, is.corr=FALSE)#Graphique du cos2 des variables sur toutes les dimensions avec le package corrplot

fviz_contrib(res.pca, choice = "var", axes = 1)# Visualisation des variables contribuants le plus sur l'axe 1. La ligne en pointill??? rouge indique la contribution moyenne
fviz_contrib(res.pca, choice = "var", axes = 2)# Visualisation des variables contribuants le plus sur l'axe 2. La ligne en pointill??? rouge indique la contribution moyenne 

var <- get_pca_var(res.pca) #Cr???ation d'une variable "var" avec tous les r???sultats concernants les variables
var
var$coord# Coordonn???es
var$cos2# Cos2: qualit??? de r???presentation
var$contrib# Contributions aux composantes principales

library("corrplot")
corrplot(var$contrib, is.corr=FALSE) #Pour chaque axe, graphique des contributions des variables avec le package corrplot
fviz_pca_var(res.pca)#graphique des variables quantis actives et suppl???mentaires selon leurs coordonn???es
fviz_pca_var(res.pca, col.var = "contrib", #graphique des variables en les coloriant selon leur cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) # ???vite le chevauchement de texte
fviz_pca_var(res.pca, invisible = "var")# Cacher les variables actives sur le graphique, ne montrer que des variables suppl???mentaires
fviz_pca_var(res.pca, invisible = "quali.sup")# Cacher les variables suppl???mentaires
ind <- get_pca_ind(res.pca) #Cr???ation d'une variable "var" avec tous les r???sultats concernants les variables
ind #la fonction get_pca_ind() retourne une liste de matrices contenant tous les r???sultats pour les individus 
head(ind$coord) #Coordonn???es des individus
head(ind$cos2) #Qualit??? des individus
head(ind$contrib) #Contributions des individus

coord<-ind$coord[,1]
contrib<-ind$contrib[,1]
cos2<-ind$cos2[,1]
display<-cbind(coord,contrib,cos2)

fviz_cos2(res.pca, choice = "ind",top=20)# graphique du Cos2 des individus sur le 1 premier axe
fviz_cos2(res.pca, choice = "ind", axes = 1:2,top=20)# graphique du Cos2 des individus cumul??? sur les 2 premiers axes
fviz_contrib(res.pca, choice = "ind", axes = 1, top=20)# Contributions des 10 individus contribuants le plus sur l'axe 1. La ligne en pointill??? rouge indique la contribution moyenne 
fviz_contrib(res.pca, choice = "ind", axes = 2, top =20)# Visualisation des 10 individus contribuants le plus sur l'axe 2. La ligne en pointill??? rouge indique la contribution moyenne

#Description des dimensions et mise en ???vidence des variables qui contribuent le plus aux composantes principales
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)#fonction dimdesc() [dans FactoMineR]
res.desc$Dim.1# Description de la dimension 1
res.desc$Dim.2# Description de la dimension 2

fviz_pca_ind(res.pca, col.ind = "red")#graphique des individus selon leurs coordonn???es
#Filtrer les r???sultats. S'il y a un nombre ???lev??? d'individus (variables), il est possible de visualiser seulement certains d'entre eux en utilisant les arguments select.ind et select.var.
#es valeurs autoris???es sont NULL ou une list contenant le nom des arguments, cos2 ou contrib
fviz_pca_ind (res.pca, select.ind = list(cos2 = 0.75)) # Visualiser les individus dont cos2> = 0.75

fviz_pca_ind (res.pca, select.ind = list(cos2 = 0.75), col.ind = "contrib",#Visualiser les individus dont cos2> = 0.75 et en les coloriant selon leur cos2
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              title="Pays (les mieux repr???sent???s sur l'axe 1) colori???s selon leur contribution",
              repel = TRUE) # ???vite le chevauchement de texte

fviz_pca_ind (res.pca, select.ind = list (contrib = 75), pointsize = "cos2",#Top 50 des individus les plus contibutifs avec une taille des points selon leur cos2
              pointshape = 21, fill = "#E7B800",
              title="75 pays (contribuant le plus sur l'axe 1)",
              repel = TRUE)
fviz_pca_ind (res.pca, select.ind = list (cos2 = 0.75), pointsize = "contrib",
              pointshape = 21, fill = "#E7B800",title="Les pays selon les variables du niveau de d???veloppement",
              repel = TRUE)

#Exporter les graphiques en PDF / PNG : le package factoextra produit des graphiques de type ggplot2
scree.plot <- fviz_eig (res.pca) #Cr???ation des graphiques que l'on veut enregistrer. Celui des valeurs propres
ind.plot <- fviz_pca_ind (res.pca)#Celui des individus
var.plot <- fviz_pca_var (res.pca)#Celui des variables

setwd("D:/Master 1 HN Lyon 2/Projet Transversal")#Pour sp???cifier le dossier dans lequel le fichier des graphiques va ???tre cr??????. Si rien de sp???cifier alors le fichier est cr?????? dans le r???pertoire de travail que l'on veut voir avec getwd()
pdf("PCA.pdf") # Exportation des graphiques ggplot2 dans un seul fichier pdf. Cr???er un nouveau p???riph???rique pdf
print (scree.plot)
print (ind.plot)
print (var.plot)
dev.off () # Fermer le p???riph???rique pdf

setwd("D:/Master 1 HN Lyon 2/Projet Transversal")
png ("pca-scree-plot.png")
print(scree.plot)
dev.off ()
png ("pca-variables.png")
print(var.plot)
dev.off ()
png ("pca-individuals.png")
print(ind.plot)
dev.off ()
library (ggpubr)
library(magrittr)

ggexport (plotlist = list(scree.plot, ind.plot, var.plot), #Exportez plusieurs graphiques dans un seul fichier pdf (un graphique par page)
          filename = "Graph.pdf")
ggexport (plotlist = list(scree.plot, ind.plot, var.plot), #Exporter plusieurs graphiques dans un seul fichier mais en mettant plusieurs graphiques par page
          nrow = 2, ncol = 2, filename = "Graph2.pdf")

write.infile(res.pca, "pca.csv", sep = ";")  # Exporter vers un fichier CSV
library(FactoMineR)
write.infile(res.pca, "pca.csv", sep = ";")  # Exporter vers un fichier CSV
write.infile(res.pca, "pca.txt", sep = "\t") # Exporter vers un fichier TXT
