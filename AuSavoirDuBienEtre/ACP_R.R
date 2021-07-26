library("FactoMineR")
library("factoextra")
library("missMDA")
library("ggplot2")
d<-read.csv2("C:/Users/Celian/Desktop/BienEtreSubjectif/WebIndicators_WHR2.csv",header=TRUE, dec="," )
rownames(d) <- d[,1]

d[,2]=1-d[,2]
d[,3]=1-d[,3]
d[,4]=1-d[,4]


d=d[,2:16]
colnames(d)

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

head(d)
str(d)
names(d)
d2<-d[,2:length(d)]
nb<-estim_ncpPCA(d2,ncp.max=5)
d.analyse.complete<-imputePCA(d2,ncp=nb$ncp)
res.pca = PCA(d.analyse.complete, quali.sup = c(20,21,22,23,24),  ncp = 5, graph = FALSE)

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
res.pca$quali.sup #r???sultats pour la variable quanti sup avec ses coordonn???es, corr???lations et cos???

library("corrplot")
corrplot(var$contrib, is.corr=FALSE) #Pour chaque axe, graphique des contributions des variables avec le package corrplot

...