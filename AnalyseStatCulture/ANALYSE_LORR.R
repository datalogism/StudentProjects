Les données
library(ade4)


data_lorr=data.frame(DATA.LORRAINE)
index=c(1:nrow(data_lorr))
Operateur=data_lorr[,1]
labels(data_lorr)
data_lorr=data_lorr[,-34]
png("Lorr_nuageDePointIntro.png", bg="transparent", width=500, height=500)
pairs(data_lorr[,1:8])
dev.off()

#Mise en colonne des variables domaines, sous domaines et vocation
v1=array()
v2=array()
v3=array()
v4=array()
v5=array()
data_lorr2=data_lorr
data_lorr2=cbind(data_lorr2,v1,v2,v3,v4,v5)
colnames(data_lorr2)[c(34,35,36,37,38)]=c("Domaine","SousDomaine","Vocation","VA","Groupe")
labels(data_lorr2)
#Modif labels
ssmod=labels(data_lorr2)[[2]][18:27]
ssmod
for(i in 1:length(ssmod)){
  ssmod[i]=substr(ssmod[i],7,nchar(ssmod[i]))}
colnames(data_lorr2)[18:27]=ssmod
voc=labels(data_lorr2)[[2]][28:33]
voc
for(i in 1:length(voc)){
  voc[i]=substr(voc[i],5,nchar(voc[i]))}
colnames(data_lorr2)[28:33]=voc

#Reshape Dom
for(i in 1 :nrow(data_lorr2)){
  for(j in 1: 9){
    if(data_lorr2[i,j+8]==1){data_lorr2[i,34]=labels(data_lorr2)[[2]][j+8]}
  }
}
data_lorr2
#Reshape ssDom
for(i in 1 :nrow(data_lorr2)){
  for(j in 1: 10){
    if(data_lorr2[i,j+17]==1){data_lorr2[i,35]=labels(data_lorr2)[[2]][j+17]}
  }
}
data_lorr2

#Reshape voc
for(i in 1 :nrow(data_lorr2)){
  for(j in 1: 6){
    if(data_lorr2[i,j+27]==1){data_lorr2[i,36]=labels(data_lorr2)[[2]][j+27]}
  }
}
data_lorr2
data_lorr2[,36]
data_lorr2=data_lorr2[,-c(9:33)]

#Suppression NA
for(i in 1 :nrow(data_lorr2)){
  for(j in 10 :11) {
    if(is.na(data_lorr2[i,j])){data_lorr2[i,j]="non def"}
  }
}

#Trouver les valeurs atypiques
t=NULL
png("Lorr_NuageDePoints_TOUT.png", bg="transparent", width=500, height=500)
plot(data_lorr[,2], main=paste("Nuage de points des montants versés à chaque opérateur"), ylab="Montant en euro")
dev.off()
png("Lorr_BoxPlot_TOUT.png", bg="transparent", width=500, height=500)
boxplot(data_lorr[,2], main=paste("Boîte à moustaches de la répartition des financements"), horizontal=TRUE,xlab="Montant en euro")
dev.off()
summary(data_lorr[,2])
Q1=as.numeric(quantile(data_lorr[,2],0.25))
Q3=as.numeric(quantile(data_lorr[,2],0.75))

Val.At.min=Q1-1.5*(Q3-Q1)
Val.At.max=Q3+1.5*(Q3-Q1)
indice=list(which(data_lorr[,2]>Val.At.max | data_lorr[,2]<Val.At.min))


#Récupérer Outliers

t=NULL
v=array()
t=data_lorr
t=cbind(t,v)
t[,34]=rep(0,nrow(t))
for(i in 1:length(indice[[1]])){
  t[indice[[1]][i],34]=1
}
t[,34]
colnames(t)[34]="VA"
sum(t[,34])

data_lorr_VA=t
write.csv2(data_lorr_VA,file = "Lorr_VA.csv")
data_lorr2[,12]=data_lorr_VA[,34]
u1=table(data_lorr2[,12],data_lorr2[,9])
write.csv2(u1,file = "Lorr_VA_DOM.csv")
u2=table(data_lorr2[,12],data_lorr2[,10])
write.csv2(u2,file = "Lorr_VA_SSDOM.csv")
u3=table(data_lorr2[,12],data_lorr2[,11])
write.csv2(u3,file = "Lorr_VA_VOC.csv")
data_lorr2[,11]
labels(data_lorr2)
write.csv2(data_lorr2,file = "Lorr_MOD_SSMOD_VOC.csv")
row.names(data_lorr2)<-NULL
data_lorr2=data_lorr2[,-1]
labels(data_lorr2)
#STAT DOM/SS_DOM

stat1=table(data_lorr2[,10],data_lorr2[,9])
stat1
write.csv2(stat1,file = "Lorr_DOMbyVOC.csv")
chisq.test(stat1,simulate.p.value = TRUE)# ssdom depend voc
stat2=table(data_lorr2[,11],data_lorr2[,10])
stat2
write.csv2(stat2,file = "Lorr_VAbyVoc.csv")
chisq.test(stat2,simulate.p.value = TRUE)# va depend voc
stat3=table(data_lorr2[,9],data_lorr2[,11])#va depend ssdom
stat3
write.csv2(stat3,file = "Lorr_VAbySsDom.csv")
chisq.test(stat3,simulate.p.value = TRUE)
stat4=table(data_lorr2[,8],data_lorr2[,11])#va depend dom
stat4
write.csv2(stat4,file = "Lorr_VAbyDom.csv")
chisq.test(stat4,simulate.p.value = TRUE)




#Jeux de données sur les rangs (minimise erreur dues val atypiques)
DATATEST=NULL
DATATEST=cbind(rank(data_lorr[2]),rank(data_lorr[3]),data_lorr[,c(4:33)])
v1=array()
v2=array()
v1=DATATEST[,3]+DATATEST[,4]+DATATEST[,5]+DATATEST[,6]+DATATEST[,7]
v2=data_lorr_VA[,34]
DATATEST=cbind(DATATEST,v1,v2)
colnames(DATATEST)[33]="Nb fin."
colnames(DATATEST)[34]="VA"
for(i in 1:nrow(DATATEST)){
  if(DATATEST[i,33]==1){
    DATATEST[i,33]="1f."
  }
  if(DATATEST[i,33]==2){
    DATATEST[i,33]="2f."
  }
  if(DATATEST[i,33]==3){
    DATATEST[i,33]="3f."
  }
  if(DATATEST[i,33]==4){
    DATATEST[i,33]="4f."
  }
  if(DATATEST[i,33]==5){
    DATATEST[i,33]="5f."
  }
}
colnames(DATATEST)[1]="Rang Montant"
colnames(DATATEST)[2]="Rang Population"
DATATEST=data.frame(DATATEST)
DATATEST


#ACP
library(FactoMineR)
res.acpT=NULL
res.acpT=PCA(DATATEST[,-c(17:32,34)],scale.unit=T,quanti.sup=1,quali.sup=17,
             ind.sup=which(DATATEST[,34]==1), ncp=8, graph=T)
labels(DATATEST)
eig=res.acpT$eig
eig
write.csv2(eig,file = "Lorr_VP.csv")

#Nb Axes analyse
png("Lorr_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(res.acpT$eig[,2],type="l",main="Lorr_Eboulli des valeurs propres")
dev.off()

NBAxe=0
for(i in 1:nrow(res.acpT$eig)){
  if(res.acpT$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
res.acpT$ind$cos2


#Projections
png("Lorr_Proj_Ind_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(1, 2),choix="ind")
dev.off()
png("Lorr_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 2), choix="var",habillage="var")
dev.off()
png("Lorr_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(1, 3), choix="ind")
dev.off()
png("Lorr_Proj_Var_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 3), choix="var")
dev.off()
png("Lorr_Proj_Ind_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(2, 3), choix="ind")
dev.off()
png("Lorr_Proj_Var_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 3), choix="var")
dev.off()
png("Lorr_Proj_Ind_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(1, 4), choix="ind")
dev.off()
png("Lorr_Proj_Var_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 4), choix="var")
dev.off()
png("Lorr_Proj_Ind_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(2, 4), choix="ind")
dev.off()
png("Lorr_Proj_Var_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 4), choix="var")
dev.off()
png("Lorr_Proj_Ind_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, ,label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(3, 4), choix="ind")
dev.off()
png("Lorr_Proj_Var_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(3, 4), choix="var")
dev.off()


t=res.acpT$var$cor
write.csv2(t,file = "Lorr_ACP_corr.csv")

library(ggplot2)
library(reshape2)
#Heatmap : correlations
png("Lorr_Corr_VariablesAC.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(res.acpT$var$cor), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

#Contributions à l'inertie 
t=NULL
t=matrix(data = NA, nrow = nrow(res.acpT$var$contrib), ncol = ncol(res.acpT$var$contrib), byrow = FALSE,       dimnames = labels(res.acpT$var$contrib))
for(j in 1:ncol(res.acpT$var$contrib)){
  for(i in 1:nrow(res.acpT$var$contrib)){
    if(res.acpT$var$contrib[i,j]>mean(res.acpT$var$contrib[,j])){
      if(res.acpT$var$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "Lorr_ACP_contrib_var.csv")
t=NULL
t=matrix(data = NA, nrow = nrow(res.acpT$ind$contrib), ncol = ncol(res.acpT$ind$contrib), byrow = FALSE,
         dimnames = labels(res.acpT$ind$contrib))
for(j in 1:ncol(res.acpT$ind$contrib)){
  for(i in 1:nrow(res.acpT$ind$contrib)){
    if(res.acpT$ind$contrib[i,j]>mean(res.acpT$ind$contrib[,j])){
      if(res.acpT$ind$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "Lorr_ACP_contrib_ind.csv")

TtestKhi=data.frame(t) 
stat5=table(TtestKhi[,1],data_lorr2[which(data_lorr2[,11]==0),8])
stat5
write.csv2(stat5,file = "Lorr_ContribByDomAxe1.csv")

chisq.test(stat5,simulate.p.value = TRUE) 

RepartDom=c(174,57,49,59,34,89,10,400,349)
RepartDom=RepartDom/1221

stat5Bis=colSums(stat5)
chisq.test(stat5Bis,p=RepartDom,simulate.p.value = TRUE)

stat6=table(TtestKhi[,2],data_lorr2[which(data_lorr2[,11]==0),8])
stat6
write.csv2(stat6,file = "Lorr_ContribByDomAxe2.csv")
chisq.test(stat6)

stat6Bis=colSums(stat6)
chisq.test(stat6Bis,p=RepartDom,simulate.p.value = TRUE)

stat7=table(TtestKhi[,3],data_lorr2[which(data_lorr2[,11]==0),8])
stat7
write.csv2(stat7,file = "Lorr_ContribByDomAxe3.csv")
chisq.test(stat7,simulate.p.value = TRUE)

stat7Bis=colSums(stat7)
chisq.test(stat7Bis,p=RepartDom,simulate.p.value = TRUE)


stat8=table(TtestKhi[,4],data_lorr2[which(data_lorr2[,11]==0),8])
stat8
write.csv2(stat8,file = "Lorr_ContribByDomAxe4.csv")
chisq.test(stat8,simulate.p.value = TRUE)
stat8Bis=colSums(stat8)
chisq.test(stat8Bis,p=RepartDom,simulate.p.value = TRUE)

#Qualité
Quali=cbind(res.acpT$var$cos2[,1]+res.acpT$var$cos2[,2],
            res.acpT$var$cos2[,1]+res.acpT$var$cos2[,3],
            res.acpT$var$cos2[,3]+res.acpT$var$cos2[,2],
            res.acpT$var$cos2[,1]+res.acpT$var$cos2[,4],
            res.acpT$var$cos2[,2]+res.acpT$var$cos2[,4],
            res.acpT$var$cos2[,3]+res.acpT$var$cos2[,4]
)
colnames(Quali)=c("1+2","1+3","2+3","1+4","2+4","3+4")

t=NULL
t=matrix(data = NA, nrow = nrow(Quali), ncol = ncol(Quali), byrow = FALSE,
         dimnames = labels(Quali))
for(j in 1:ncol(Quali)){
  for(i in 1:nrow(Quali)){
    if(Quali[i,j]>0.7){
      t[i,j]=Quali[i,j]      
    }
  }
}
write.csv2(t,file = "Lorr_ACP_qualite_var.csv")

Quali=cbind(res.acpT$ind$cos2[,1]+res.acpT$ind$cos2[,2],
            res.acpT$ind$cos2[,1]+res.acpT$ind$cos2[,3],
            res.acpT$ind$cos2[,3]+res.acpT$ind$cos2[,2],
            res.acpT$ind$cos2[,1]+res.acpT$ind$cos2[,4],
            res.acpT$ind$cos2[,2]+res.acpT$ind$cos2[,4],
            res.acpT$ind$cos2[,3]+res.acpT$ind$cos2[,4]
)
colnames(Quali)=c("1+2","1+3","2+3","1+4","2+4","3+4")
t=NULL
t=matrix(data = NA, nrow = nrow(Quali), ncol = ncol(Quali), byrow = FALSE,
         dimnames = labels(Quali))
for(j in 1:ncol(Quali)){
  for(i in 1:nrow(Quali)){
    if(Quali[i,j]>0.7){
      t[i,j]=Quali[i,j]      
    }
  }
}
write.csv2(t,file = "Lorr_ACP_qualite_ind.csv")


#la classification
#CAH


for(i in 1:nrow(DATATEST)){
  if(DATATEST[i,33]=="1f."){
    DATATEST[i,33]=1
  }
  if(DATATEST[i,33]=="2f."){
    DATATEST[i,33]=2
  }
  if(DATATEST[i,33]=="3f."){
    DATATEST[i,33]=3
  }
  if(DATATEST[i,33]=="4f."){
    DATATEST[i,33]=4
  }
  if(DATATEST[i,33]=="5f."){
    DATATEST[i,33]=5
  }
}
DATATEST[,33]=as.numeric(DATATEST[,33])
res.acpT2=PCA(DATATEST[,-34],scale.unit=T,quanti.sup=1,
              ind.sup=which(DATATEST[,34]==1), ncp=8, graph=T)
#res.acp2=PCA(data_lorr4,scale.unit=T, ncp=6, graph=T,ind.sup=which(data_lorr4[,33]==1))
#plot(res.acp2$eig[,2])
res.hcpc=NULL
res.hcpc = HCPC(res.acpT2, metric="euclidean", method="ward",graph=F)
cluster=res.hcpc$data.clust
png("Lorr_CAH_Arbre.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="tree")
dev.off()
png("Lorr_CAH_barplot.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="bar")
dev.off()
png("Lorr_CAH_map_1_2.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,2))
dev.off()
png("Lorr_CAH_map_2_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,3))
dev.off()
png("Lorr_CAH_map_1_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,3))
dev.off()
png("Lorr_CAH_map_1_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,4))
dev.off()
png("Lorr_CAH_map_2_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,4))
dev.off()
png("Lorr_CAH_map_3_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(3,4))
dev.off()

#Récupération des groupes (5 classes)
v=array()
res=cbind(DATATEST,v)
labels(res)
res[,35]=NA
for(i in 1:nrow(cluster)){
  for(j in 1:nrow(res)){
    if(j==rownames(cluster)[i]){
      res[j,35]=cluster[i,34]
      print("_________")
      print(i)
      print(j)
      print(rownames(cluster)[i])
    }
  }
}
for(i in 1:nrow(res)){
  if(is.na(res[i,35])){
    res[i,35]=4 
  }
}
labels(res)
colnames(res)[35]="Cluster"
res=cbind(data_lorr[,1:3],res[,3:35])
res=res[,-35]
res=data.frame(res,row.names=1)
write.csv2(res,file = "DATA_LORR_Class.csv")


#Description modalité
#Description modalité

write.csv2(condes(res,num.var=33,proba=0.05)$quanti,file = "Lorr_DescCorr33.csv")
write.csv2(condes(res,num.var=1,proba=0.05)$quanti,file = "Lorr_DescCorr1.csv")

write.csv2(condes(res,num.var=34,proba=0.05)$quanti,file = "Lorr_DescCorr34.csv")

#Moyennes par classes

png("Poit_Boxplot_PopClass.png", bg="transparent", width=500, height=500)
layout(matrix(c(1:4),2,2))
for(i in 1:4){boxplot(res[which(res[,34]==i),1],main=paste(names(res)[1],"groupe",i))}
layout(1)
dev.off()
png("Poit_Boxplot_SommeClass.png", bg="transparent", width=500, height=500)
layout(matrix(c(1:6),2,2))
for(i in 1:4){boxplot(res[which(res[,34]==i),2],main=paste(names(res)[2],"groupe",i))}
layout(1)
dev.off()
png("Hist_NbFinClass.png", bg="transparent", width=500, height=500)
layout(matrix(c(1:4),2,2))
for(i in 1:4){hist(res[which(res[,34]==i),33],main=paste(names(res)[33],"groupe",i),xlab="NB.FIN")}
layout(1)
dev.off()

resumePOP<-aggregate(res[2],res[34], summary)
write.csv2(resumePOP,file = "Lorr_Tab_resume_pop.csv")
resumeMONTANT<-aggregate(res[1],res[34], summary)
write.csv2(resumeMONTANT,file = "Lorr_Tab_resume_somme.csv")
resumeRESTE<-aggregate(res[,c(-1,-2,-33,-34)],res[34], sum)
write.csv2(resumeRESTE,file = "Lorr_Tab_resume_dom_voc_classe.csv")
t1=table(res[,1],rowSums(res[3:7])) #nb fin/montant
write.csv2(t1,file = "Lorr_Tab_nbfin_montant.csv")
t2=table(rowSums(res[,3:7]),res[,34]) #nb fin/classe
write.csv2(t2,file = "Lorr_Tab_nbfin_classe.csv")


LM=data.frame(DATA_LORR_PF,row.names=1)
#######TESTS
#DISTRIBUTION NORMALE ?
t1.1=ks.test(LM[,1],"pnorm")
rt1.1=c(t1.1$data.name,t1.1$method,t1.1$alternative,t1.1$statistic,t1.1$p.value)
names(rt1.1)<-c("Données","Methode","Alternative","Stat","P-value")
t1.2=ks.test(LM[which(LM[,34]==1),1],"pnorm")
rt1.2=c(t1.2$data.name,t1.2$method,t1.2$alternative,t1.2$statistic,t1.2$p.value)
names(rt1.2)<-c("Données","Methode","Alternative","Stat","P-value")
t1.3=ks.test(LM[which(LM[,34]==1),1],"pnorm")
rt1.3=c(t1.3$data.name,t1.3$method,t1.3$alternative,t1.3$statistic,t1.3$p.value)
names(rt1.3)<-c("Données","Methode","Alternative","Stat","P-value")

t1.4=ks.test(LM[which(LM[,34]==2),1],"pnorm")
t1.5=ks.test(LM[which(LM[,34]==2),1],"pnorm")
#MM DISTRIB ?
ks.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==2),1])
ks.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==3),1])
ks.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==4),1])
ks.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==3),1])
ks.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==4),1])
ks.test(LM[which(LM[,34]==3),1],LM[which(LM[,34]==4),1])

#MOYENNE INTER CLUST SOMME
wilcoxwilcoxwilcox.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==2),1],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==3),1],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==4),1],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==3),1],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==4),1],var.equal=T)
wilcox.test(LM[which(LM[,34]==3),1],LM[which(LM[,34]==4),1],var.equal=T)

#VARIANCE INTER CLUST SOMME
var.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==2),1],var.equal=T)
var.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==3),1],var.equal=T)
var.test(LM[which(LM[,34]==1),1],LM[which(LM[,34]==4),1],var.equal=T)
var.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==3),1],var.equal=T)
var.test(LM[which(LM[,34]==2),1],LM[which(LM[,34]==4),1],var.equal=T)
var.test(LM[which(LM[,34]==3),1],LM[which(LM[,34]==4),1],var.equal=T)

#MOYENNE INTER CLUST POP
wilcox.test(LM[which(LM[,34]==1),2],LM[which(LM[,34]==2),2],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),2],LM[which(LM[,34]==3),2],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),2],LM[which(LM[,34]==4),2],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),2],LM[which(LM[,34]==3),2],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),2],LM[which(LM[,34]==4),2],var.equal=T)
wilcox.test(LM[which(LM[,34]==3),2],LM[which(LM[,34]==4),2],var.equal=T)

#MOYENNE INTER CLUST POT FISC 
wilcox.test(LM[which(LM[,34]==1),35],LM[which(LM[,34]==2),35],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),35],LM[which(LM[,34]==3),35],var.equal=T)
wilcox.test(LM[which(LM[,34]==1),35],LM[which(LM[,34]==4),35],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),35],LM[which(LM[,34]==3),35],var.equal=T)
wilcox.test(LM[which(LM[,34]==2),35],LM[which(LM[,34]==4),35],var.equal=T)
wilcox.test(LM[which(LM[,34]==3),35],LM[which(LM[,34]==4),35],var.equal=T)

#Somme depend NB. FIN ?
chisq.test(LM[,33],LM[,1],simulate.p.value = TRUE)

# TEST CLUST PROP E
prop.test(c(length(which(LM[which(LM[,34]==1),3]==1)),
                   length(which(LM[which(LM[,34]==2),3]==1))),
          c(length(which(LM[which(LM[,34]==1),3]==0)),
            length(which(LM[which(LM[,34]==2),3]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),3]==1)),
            length(which(LM[which(LM[,34]==3),3]==1))),
          c(length(which(LM[which(LM[,34]==1),3]==0)),
            length(which(LM[which(LM[,34]==3),3]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),3]==1)),
            length(which(LM[which(LM[,34]==4),3]==1))),
          c(length(which(LM[which(LM[,34]==1),3]==0)),
            length(which(LM[which(LM[,34]==4),3]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),3]==1)),
            length(which(LM[which(LM[,34]==3),3]==1))),
          c(length(which(LM[which(LM[,34]==2),3]==0)),
            length(which(LM[which(LM[,34]==3),3]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),3]==1)),
            length(which(LM[which(LM[,34]==4),3]==1))),
          c(length(which(LM[which(LM[,34]==2),3]==0)),
            length(which(LM[which(LM[,34]==4),3]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),3]==1)),
            length(which(LM[which(LM[,34]==4),3]==1))),
          c(length(which(LM[which(LM[,34]==3),3]==0)),
            length(which(LM[which(LM[,34]==4),3]==0)))) 
# TEST CLUST PROP R
prop.test(c(length(which(LM[which(LM[,34]==1),4]==1)),
            length(which(LM[which(LM[,34]==2),4]==1))),
          c(length(which(LM[which(LM[,34]==1),4]==0)),
            length(which(LM[which(LM[,34]==2),4]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),4]==1)),
            length(which(LM[which(LM[,34]==3),4]==1))),
          c(length(which(LM[which(LM[,34]==1),4]==0)),
            length(which(LM[which(LM[,34]==3),4]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),4]==1)),
            length(which(LM[which(LM[,34]==4),4]==1))),
          c(length(which(LM[which(LM[,34]==1),4]==0)),
            length(which(LM[which(LM[,34]==4),4]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),4]==1)),
            length(which(LM[which(LM[,34]==3),4]==1))),
          c(length(which(LM[which(LM[,34]==2),4]==0)),
            length(which(LM[which(LM[,34]==3),4]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),4]==1)),
            length(which(LM[which(LM[,34]==4),4]==1))),
          c(length(which(LM[which(LM[,34]==2),4]==0)),
            length(which(LM[which(LM[,34]==4),4]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),4]==1)),
            length(which(LM[which(LM[,34]==4),4]==1))),
          c(length(which(LM[which(LM[,34]==3),4]==0)),
            length(which(LM[which(LM[,34]==4),4]==0)))) 
# TEST CLUST PROP D
prop.test(c(length(which(LM[which(LM[,34]==1),5]==1)),
            length(which(LM[which(LM[,34]==2),5]==1))),
          c(length(which(LM[which(LM[,34]==1),5]==0)),
            length(which(LM[which(LM[,34]==2),5]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),5]==1)),
            length(which(LM[which(LM[,34]==3),5]==1))),
          c(length(which(LM[which(LM[,34]==1),5]==0)),
            length(which(LM[which(LM[,34]==3),5]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),5]==1)),
            length(which(LM[which(LM[,34]==4),5]==1))),
          c(length(which(LM[which(LM[,34]==1),5]==0)),
            length(which(LM[which(LM[,34]==4),5]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),5]==1)),
            length(which(LM[which(LM[,34]==3),5]==1))),
          c(length(which(LM[which(LM[,34]==2),5]==0)),
            length(which(LM[which(LM[,34]==3),5]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),5]==1)),
            length(which(LM[which(LM[,34]==4),5]==1))),
          c(length(which(LM[which(LM[,34]==2),5]==0)),
            length(which(LM[which(LM[,34]==4),5]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),5]==1)),
            length(which(LM[which(LM[,34]==4),5]==1))),
          c(length(which(LM[which(LM[,34]==3),5]==0)),
            length(which(LM[which(LM[,34]==4),5]==0)))) 
# TEST CLUST PROP R
prop.test(c(length(which(LM[which(LM[,34]==1),6]==1)),
            length(which(LM[which(LM[,34]==2),6]==1))),
          c(length(which(LM[which(LM[,34]==1),6]==0)),
            length(which(LM[which(LM[,34]==2),6]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),6]==1)),
            length(which(LM[which(LM[,34]==3),6]==1))),
          c(length(which(LM[which(LM[,34]==1),6]==0)),
            length(which(LM[which(LM[,34]==3),6]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),6]==1)),
            length(which(LM[which(LM[,34]==4),6]==1))),
          c(length(which(LM[which(LM[,34]==1),6]==0)),
            length(which(LM[which(LM[,34]==4),6]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),6]==1)),
            length(which(LM[which(LM[,34]==3),6]==1))),
          c(length(which(LM[which(LM[,34]==2),6]==0)),
            length(which(LM[which(LM[,34]==3),6]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),6]==1)),
            length(which(LM[which(LM[,34]==4),6]==1))),
          c(length(which(LM[which(LM[,34]==2),6]==0)),
            length(which(LM[which(LM[,34]==4),6]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),6]==1)),
            length(which(LM[which(LM[,34]==4),6]==1))),
          c(length(which(LM[which(LM[,34]==3),6]==0)),
            length(which(LM[which(LM[,34]==4),6]==0))))    
# TEST CLUST PROP EPC
prop.test(c(length(which(LM[which(LM[,34]==1),7]==1)),
            length(which(LM[which(LM[,34]==2),7]==1))),
          c(length(which(LM[which(LM[,34]==1),7]==0)),
            length(which(LM[which(LM[,34]==2),7]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),7]==1)),
            length(which(LM[which(LM[,34]==3),7]==1))),
          c(length(which(LM[which(LM[,34]==1),7]==0)),
            length(which(LM[which(LM[,34]==3),7]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),7]==1)),
            length(which(LM[which(LM[,34]==4),7]==1))),
          c(length(which(LM[which(LM[,34]==1),7]==0)),
            length(which(LM[which(LM[,34]==4),7]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),7]==1)),
            length(which(LM[which(LM[,34]==3),7]==1))),
          c(length(which(LM[which(LM[,34]==2),7]==0)),
            length(which(LM[which(LM[,34]==3),7]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),7]==1)),
            length(which(LM[which(LM[,34]==4),7]==1))),
          c(length(which(LM[which(LM[,34]==2),7]==0)),
            length(which(LM[which(LM[,34]==4),7]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),7]==1)),
            length(which(LM[which(LM[,34]==4),7]==1))),
          c(length(which(LM[which(LM[,34]==3),7]==0)),
            length(which(LM[which(LM[,34]==4),7]==0))))  
length(which(LM[which(LM[,34]==4),6]==0))))    
# TEST CLUST PROP C
prop.test(c(length(which(LM[which(LM[,34]==1),8]==1)),
            length(which(LM[which(LM[,34]==2),8]==1))),
          c(length(which(LM[which(LM[,34]==1),8]==0)),
            length(which(LM[which(LM[,34]==2),8]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),8]==1)),
            length(which(LM[which(LM[,34]==3),8]==1))),
          c(length(which(LM[which(LM[,34]==1),8]==0)),
            length(which(LM[which(LM[,34]==3),8]==0))))
prop.test(c(length(which(LM[which(LM[,34]==1),8]==1)),
            length(which(LM[which(LM[,34]==4),8]==1))),
          c(length(which(LM[which(LM[,34]==1),8]==0)),
            length(which(LM[which(LM[,34]==4),8]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),8]==1)),
            length(which(LM[which(LM[,34]==3),8]==1))),
          c(length(which(LM[which(LM[,34]==2),8]==0)),
            length(which(LM[which(LM[,34]==3),8]==0))))
prop.test(c(length(which(LM[which(LM[,34]==2),8]==1)),
            length(which(LM[which(LM[,34]==4),8]==1))),
          c(length(which(LM[which(LM[,34]==2),8]==0)),
            length(which(LM[which(LM[,34]==4),8]==0))))    
prop.test(c(length(which(LM[which(LM[,34]==3),8]==1)),
            length(which(LM[which(LM[,34]==4),8]==1))),
          c(length(which(LM[which(LM[,34]==3),8]==0)),
            length(which(LM[which(LM[,34]==4),8]==0)))) 

by(LM,LM[,34],summary)
boxplot(LM[,1]~LM[,34])
boxplot(LM[which(LM[,34]!=4),1]~LM[which(LM[,34]!=4),34])
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==1),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==2),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==3),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==4),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
abline(lm(LM[,1]~LM[,34]),col="green")
m1<-lm(SOMME~)



###RLM
#ANCOVA
LMANCOV=data.frame(Lorr_ANCOV,row.names=1)



LMANCOV[,2]<-factor(LMANCOV[,2])
LMANCOV[,3]<-factor(LMANCOV[,3])
LMANCOV[,4]<-factor(LMANCOV[,4])
LMANCOV[,5]<-factor(LMANCOV[,5])
LMANCOV[,9]<-factor(LMANCOV[,9])


#VISUALISATION
######################## OBSERVATION LIEN Y / Xi
par(mfrow=c(3,2))
for(j in 2:7){
  plot(LMANCOV[,j],LMANCOV[,1],ylab="SOMME",xlab=names(LMANCOV)[j]);abline(h=0)
}
layout(1)

plot(LMANCOV[,9],LMANCOV[,1],ylab="SOMME",xlab=names(LMANCOV)[9]);abline(h=0)

coplot(LMANCOV[,1]~LMANCOV[,3]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,4]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,5]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,6]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,7]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,8]|LMANCOV[,2])
coplot(LMANCOV[,1]~LMANCOV[,9]|LMANCOV[,2])

#Test homogéinité var
bartlett.test(LMANCOV[,1],LMANCOV[,2]) #égalité variances clust
bartlett.test(LMANCOV[,1],LMANCOV[,3]) #égalité ss dom #pas ok
bartlett.test(LMANCOV[,1],LMANCOV[,4]) #égalité voc
bartlett.test(LMANCOV[,1],LMANCOV[,5]) #égalité dom
bartlett.test(LMANCOV[,1],LMANCOV[,6]) # égalité pop
bartlett.test(LMANCOV[,1],LMANCOV[,7]) #égalité variances nb. fin
bartlett.test(LMANCOV[,1],LMANCOV[,8]) #égalité PF
bartlett.test(LMANCOV[,1],LMANCOV[,9]) #égalité COMBIN

ANNOV1=lm(SOMME~COMBIN+domaine+sous.domaine+vocation+Cluster+PF4Taxes+Population+Nb.fin.,LMANCOV)
plot(ANNOV1)
summary(ANNOV1)

#Etude des résidus

e<-ANNOV1$residuals
qqnorm(e,datax=T,ylab="Quantiles obs",xlab="Quantile Th")

#GRAPH RESIDUS 
par(mfrow=c(3,2))
for(j in 1:6){
  plot(LMANCOV[,j],e,ylab="Résidus",xlab=names(LMANCOV)[j]);abline(h=0)
}
layout(1)
par(mfrow=c(3,2))
for(j in 1:6){
  plot(LMANCOV[,j+7],e,ylab="Résidus",xlab=names(LMANCOV)[j+7]);abline(h=0)
}
layout(1)
n=nrow(LMANCOV)
p=length(ANNOV1$coefficients)


#calcul res standardd.
res.standard<-rstandard(ANNOV1)
alpha<-0.1
seuil.standard<-qt(1-alpha/2,n-p-1)
plot(LMANCOV[,1],res.standard)
abline(h=-seuil.standard)
abline(h=seuil.standard)
abline(h=0)

ab.standard<-LMANCOV[res.standard< -seuil.standard|res.standard>seuil.standard,]
nrow(ab.standard)

#calcul res studentisé
res.student<-rstudent(ANNOV1)
seuil.student<-qt(1-alpha/2,n-p-2)
plot(LMANCOV[,1],res.student)
abline(h=-seuil.student)
abline(h=seuil.student)
abline(h=0)
ab.student<-LMANCOV[res.student< -seuil.student|res.student>seuil.student,]
nrow(ab.student)

#Point de levier
atypiques<-influence.measures(ANNOV1)
attributes(atypiques)
atypiques$infmat
res.hat<-atypiques$infmat[,"hat"]
seuil.hat<-2*(p+1)/n
ab.hat<-LMANCOV[res.hat>seuil.hat,]
nrow(ab.hat)
#Traitement des points atypiques
b.standard<-(res.standard < -seuil.standard | res.standard > seuil.standard)
b.student <-(res.student < -seuil.student|res.student > seuil.student)
d.hat <-(res.hat > seuil.hat)
b.suspicious <- b.standard | b.student | b.hat
b.not.suspicious <- !b.suspicious
anov_clean=data.frame(LMANCOV[which(b.not.suspicious==TRUE),])
anov_clean_P=anov_clean[-which(anov_clean[,1]<=0),]

#UTILISATION DU CLEAN
ANNOV2=lm(SOMME~COMBIN+domaine+sous.domaine+vocation+Cluster+PF4Taxes+Population+Nb.fin.,anov_clean_P)
plot(ANNOV2)
summary(ANNOV2,)

#TEST NORMAL
shapiro.test(ANNOV2$residuals)

#Test Indep res
dwtest(ANNOV2, alternative="two.side")

#Test hétérocédasticité verif
library(lmtest)
bptest(ANNOV2)

#NORMALISATION
BOXC=boxcox(ANNOV2,lambda=seq(-2,2,length=50),plotit=TRUE, eps=1/50, xlab=expression(lambda))
lambda=0.1
ANNOV2=lm(((SOMME^lambda)-1/lambda)~COMBIN+domaine+sous.domaine+vocation+Cluster+PF4Taxes+Population+Nb.fin.,anov_clean_P)
summary(ANNOV2)
plot(ANNOV2)


#TEST NORMAL
shapiro.test(ANNOV2$residuals)

#Test Indep res
dwtest(ANNOV2, alternative="two.side")

#Test hétérocédasticité verif
library(lmtest)
bptest(ANNOV2)



#Moyennes res
mean(ANNOV2$residuals)
anova(ANNOV2)

 #TEST DIFFERENTS MODELS


anov3.1=cbind(anov_clean_P[,2],ANNOV3.1$fitted.values)
qplot(anov3.1[,1],colour=factor(anov3.1[,1]),geom="point")

#GRAPHIQUES EN FOLIES
qplot(log(SOMME),data=anov_clean_P,colour=COMBIN,,geom="bar",position="fill")
qplot(log(SOMME),data=anov_clean_P,colour=Cluster)
qplot(log(SOMME),data=anov_clean_P,colour=domaine,geom="bar",position="fill")
qplot(log(SOMME),Cluster,data=anov_clean_P,colour=Cluster)
qplot(SOMME,Cluster,data=anov_clean_P,colour=Cluster)
qplot(log(SOMME),Population,data=anov_clean_P,colour=Cluster)
qplot(log(SOMME),PF4Taxes,data=anov_clean_P,colour=Cluster)
qplot(log(SOMME),Nb.fin.,data=anov_clean_P,colour=Cluster)
qplot(log(SOMME),,data=anov_clean_P,colour=Cluster)
qplot(log(COMBIN),data=anov_clean_P,colour=Cluster)
qplot(exp(ANNOV2$fitted.values),anov_clean_P[,1])




TukeyHSD(ANNOV1)
model.matrix(ANNOV1)
plot(ANNOV1$fitted,LM[,1])



plot(((LMANCOV[,1]^1.35)-1/1.35)~LMANCOV[,3])
plot(LMANCOV[,1]~LMANCOV[,3])
LMANCOV=LMANCOV[-which(LMANCOV[,1]<=0),]
BOXC=boxcox(lm(SOMME~Cluster+sous.domaine+vocation+domaine+Population+Nb.fin.+PF4Taxes+COMBIN,LMANCOV),lambda=seq(-2,2,length=50),plotit=TRUE, eps=1/50, xlab=expression(lambda))
resANCOVF=Anova(lm(SOMME~Cluster+sous.domaine+vocation+domaine+Population+Nb.fin.+PF4Taxes+COMBIN,LMANCOV))
plot(resANCOVF)
summary(resANCOVF)

resANCOV1=lm(log(SOMME)~CLUSTER+POPULATION+NB.FIN,LMANCOV)
plot(resANCOV1)
summary(resANCOV1)





BOXC=boxcox(lm(SOMME~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+T+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.B+VOC.A+VOC.F+VOC.C+Nb.fin.+PF4Taxes+Cluster,LMC)
       ,lambda=seq(-2,2,length=50),plotit=TRUE, eps=1/50, xlab=expression(lambda))
boxplot.stats(LM,coef=1)
hist(((LM[,1]
hist(((LM[,1]^0.1)-1)/0.1)
hist(log(LM[,1]))
#Sur modele clean
model_clean1<-lm(SOMME~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+T+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.B+VOC.A+VOC.F+VOC.C+Nb.fin.+PF4Taxes+Cluster,LMC)
model_clean2<-lm(((SOMME^1.35)-1/1.35)~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+T+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.B+VOC.A+VOC.F+VOC.C+Nb.fin.+PF4Taxes+Cluster,LMC)
model_clean3<-lm(((SOMME^2)-1/2)~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+T+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.B+VOC.A+VOC.F+VOC.C+Nb.fin.+PF4Taxes+Cluster,LMC)

summary(model_clean3)
mean(model_clean2$fitted.values)
summary(model_clean2)
summary(model_clean3)
plot(model_clean3)
plot(model_clean$re)
#Detection colinéarité si supp R2 mauvais
mcxx<-cor(LMC)
qplot(x=Var1, y=Var2, data=melt(mcxx), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
mcxx<-mcxx^2
mcxx


t=NULL
for(i in 1:nrow(mcxx)){
    for(j in 1:ncol(mcxx)){
    if(mcxx[i,j]> 0.5273-(0.5273*0.05) &  mcxx[i,j]< 0.5273+(0.5273*0.05)& !is.na(mcxx[i,j]) & mcxx[i,j]!=1){
      print("__________________")
      print(labels(mcxx)[[1]][i])
      print(labels(mcxx)[[2]][j])
    }
    }
}

#Colinéarité VIF
LMC2=cbind(LMC[,-1],LMC[,1])
names(LMC2)[35]="SOMME"
labels(LMC2)
n=ncol(LMC2)
r2<-double(n)
Lab=names(LMC2)
LMC2[,34]
names(LMC2)
for(j1 in 1:(n)){
  str_formule <- paste(Lab[j1],"~")
  for(j2 in 1:(n)){
    if(j2!=j1){
      str_formule <- paste(str_formule, Lab[j2],"+")
    }
  }
  str_formule<-substr(str_formule,1,nchar(str_formule)-2)
  print(str_formule)
  formule <- as.formula(str_formule)
  regtest <- lm(formule,data=LMC2)
  print(summary(regtest))
  resume.regtest <-summary(regtest)
  print(resume.regtest$r.squared)
  r2[j1] <-resume.regtest$r.squared
}


vif <- 1/(1-r2)
names(vif) <- names(LMC)[1:38]
vif
acf(model_clean$residuals)
library(MASS)
reg3<-lm(formula = SOMME ~  E + D + C + APV + LL + SSDOM.TH  + SSDOM.M + VOC.D  + VOC.A  + 
           Cl1 + Cl2 + Cl3, data = LMC)
summary(reg3)
plot(reg3)
LMAIC=stepAIC(model_clean3,trace=TRUE,direction="backward")
summary(LMAIC)
lm(formula = SOMME ~ Population + E + D + C + APV + LL + SSDOM.TH + 
     SSDOM.C + SSDOM.M + SSDOM.D + VOC.D + VOC.B + VOC.A + PF4Taxes + 
     Cl1 + Cl2 + Cl3, data = LMC)
mean(LMAIC$residuals)
reg2<-lm(SOMME~1,data=LMC)
LMAIC=stepAIC(reg,trace=TRUE,direction="backward")
LMAIC2=stepAIC(reg2,scope=list(lower="SOMME~1",upper="~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+T+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.B+VOC.A+VOC.F+VOC.C+Nb.fin.+PF4Taxes+Cl1+Cl2+Cl3+Cl4"),direction="forward")
plot(LMAIC)
summary(LMAIC2)
plot(RES1.2)
summary(LMAIC2)
summary(LMAIC)
Coef.LMF=list(LMAIC$coefficients)
Coef.LMF
dimnames(LMC)
library(lmtest)
dwtest(LMAIC)
t1=NULL
t1=array()
  for(i in 1: nrow(LMC)){
    t1[i]=Coef.LMF[[1]][1]
    for(j in 2: length(Coef.LMF[[1]])) {
    t1[i]=t1[i]+(LMC[i, names(Coef.LMF[[1]][j])]*Coef.LMF[[1]][j])
  }
}
t1

Coef.LMFINI=list(model_clean$coefficients)
Coef.LMFINI
t2=array()
t2=NULL
for(i in 1: nrow(LMC)){
  t2[i]=Coef.LMFINI[[1]][1]
  for(j in 2: length(Coef.LMF[[1]])) {
    if(is.na(Coef.LMFINI[[1]][j])==FALSE){t2[i]=t2[i]+(LMC[i, names(Coef.LMFINI[[1]][j])]*Coef.LMFINI[[1]][j])
  }
}
}
t2
is.na
plot(LMC[,1])

=t
plot((LMC[,1]-SOMMELMF))
length(which((LMC[,1]-SOMMELMF)>1| (LMC[,1]-SOMMELMF)< -1))/nrow(LMC)
LMF=data.frame(LMF)
=

summary(LMF)
dwtest(LMF)
