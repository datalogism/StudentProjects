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
data_poit=data.frame(DATA.POITOU)
index=c(1:nrow(data_poit))
Operateur=data_poit[,1]
labels(data_poit)
data_poit=data_poit[,-34]

colnames(data_lorr)<-colnames(data_poit)
data_tout=rbind(data_lorr,data_poit)
data_tout=cbind(data_tout,array(c(rep("lorr",1221),rep("poitou",nrow(data_tout)-1221))))
colnames(data_tout)[34]="région"

T=data.frame(TOUT_NEW2, row.names=1)
labels(T)
T_Lorr=T[which(T[,5]=="Lorraine"),]

T_Poit=T[which(T[,5]=="Poitou-Charente"),]
S_Lorr=S[which(S[,8]=="Lorraine"),]
S_Poit=S[which(S[,8]=="Poitou-Charente"),]
qplot(Nb.fin., data=T,colour=Region, geom="bar", position="fill")
qplot(log(SOMME), data=T,colour=Region, geom="bar", position="fill")
p.tmp<-ggplot(S, aes(x=factor(1),fill=factor(domaine)))+geom_bar(width=1)
p.tmp
p.tmp+coord_polar(thetha="y")
p.tmp+coord_polar()
ggplot(S,aes(factor(domaine),fill=factor(domaine)))+geom_bar(width=1)+coord_polar()
T_temp=S[which(S[,2]!="non def"),]
ggplot(T_temp,aes(factor(sous.domaine),fill=factor(sous.domaine)))+geom_bar(width=1)+coord_polar()
S=data.frame(TOUT_NEW, row.names=1)
ggplot(T_temp,aes(factor(vocation),fill=factor(vocation)))+geom_bar(width=1)+coord_polar()
S=data.frame(TOUT_NEW, row.names=1)
png("_nuageDePointIntro.png", bg="transparent", width=500, height=500)
pairs(T[1:15])
dev.off()

#Test MoyennesSOMME/Region
mean(T_Lorr[,1])
mean(T_Poit[,1])
  wilcox.test(T_Lorr[,1],T_Poit[,1])
fisher.test
#Test distrib SOMME/Region
ks.test(T_Lorr[,1],T_Poit[,1])

#Test MoyennesSOMME/ssDom/Region
m1=aggregate(S_Lorr[,1],list(S_Lorr[,2]), mean)
m1
m2=aggregate(S_Poit[,1],list(S_Poit[,2]), mean)
m2
wilcox.test(m1[,2],m2[,2])
#Test MoyennesSOMME/Dom/Region
m1=aggregate(S_Lorr[,1],list(S_Lorr[,3]), mean)
m1
m2=aggregate(S_Poit[,1],list(S_Poit[,3]), mean)
m2
wilcox.test(S_Lorr[which(S_Lorr[,4]=="AD"),1],S_Poit[which(S_Poit[,4]=="AD"),1])
wilcox.test(S_Lorr[which(S_Lorr[,4]=="APV"),1],S_Poit[which(S_Poit[,4]=="APV"),1])
wilcox.test(S_Lorr[which(S_Lorr[,4]=="CA"),1],S_Poit[which(S_Poit[,4]=="CA"),1])
wilcox.test(S_Lorr[which(S_Lorr[,4]=="LL"),1],S_Poit[which(S_Poit[,4]=="LL"),1])

wilcox.test(S_Lorr[which(S_Lorr[,4]=="M"),1],S_Poit[which(S_Poit[,4]=="M"),1])

wilcox.test(S_Lorr[which(S_Lorr[,4]=="PAT"),1],S_Poit[which(S_Poit[,4]=="PAT"),1])

wilcox.test(S_Lorr[which(S_Lorr[,4]=="PLU"),1],S_Poit[which(S_Poit[,4]=="PLU"),1])

wilcox.test(S_Lorr[which(S_Lorr[,4]=="SV"),1],S_Poit[which(S_Poit[,4]=="SV"),1])

wilcox.test(S_Lorr[which(S_Lorr[,4]=="T"),1],S_Poit[which(S_Poit[,4]=="T"),1])


c(m1[1,2],m2[1,2])
#Test MoyennesSOMME/vocationSV/Region
m1=aggregate(S_Lorr[which(S_Lorr[,4]!="non def"),1],list(S_Lorr[,3]), mean)
m1
m2=aggregate(S_Poit[which(S_Poit[,4]!="non def"),1],list(S_Poit[,3]), mean)
m2
wilcox.test(m1[3,2],m2[3,2])
m2[1,2]
#Test MoyennesSOMME/NbFin/Region
m1=aggregate(S_Lorr[,1],list(S_Lorr[,5]), mean)
m1
m2=aggregate(S_Poit[,1],list(S_Poit[,5]), mean)
m2
wilcox.test(m1[,2],m2[,2])


chisq.test(as.factor(T_Lorr[,1]),as.factor(T_Poit[,1]))


library(gridExtra)
library(ggplot2)

#boxplot
b1<-ggplot(S, aes(S[,8], S[,1])) + 
  geom_boxplot(aes(fill = S[,8],color=S[,8])) +
  theme(legend.position = "top")+
  geom_jitter(alpha=I(1/4), aes(color=S[,8])) +
  theme(legend.position = "none")
b1
#jitter plot
b2<-ggplot(S, aes(S[,8], S[,1])) + 
  geom_jitter(alpha=I(1/4), aes(color=S[,8])) +
  theme(legend.position = "none")
b2
#GPLOT
gplot <- ggplot(S, aes(S[,1], S[,2])) + 
  geom_point(aes(color=S[,8])) +
  geom_vline(xintercept=mean(S[,1]), color="black")+
  geom_vline(xintercept=c(quantile(S[,1],0.25),quantile(S[,1],0.75)), color="grey")
  
gplot  
gplot <- ggplot(S, aes(S[,1], S[,3])) + 
  geom_point(aes(color=S[,8]) )+
  geom_vline(xintercept=mean(S[,1]), color="black")+
  geom_vline(xintercept=c(quantile(S[,1],0.25),quantile(S[,1],0.75)), color="grey")

gplot  
gplot <- ggplot(S, aes(S[,1], S[,4])) + 
  geom_point(aes(color=S[,8]) ) +
  geom_vline(xintercept=mean(S[,1]), color="black")+
  geom_vline(xintercept=c(quantile(S[,1],0.25),quantile(S[,1],0.75)), color="grey")
gplot  
gplot <- ggplot(S, aes(S[,1], S[,5])) + 
  geom_point(aes(color=S[,8]) )+
  geom_vline(xintercept=mean(S[,1]), color="black")+
  geom_vline(xintercept=c(quantile(S[,1],0.25),quantile(S[,1],0.75)), color="grey")
gplot
gplot <- ggplot(S, aes(S[,1], S[,6])) + 
  geom_point(aes(color=S[,8],size=S[,5) )
geom_vline(xintercept=mean(S[,1]), color="black")+
  geom_vline(xintercept=c(quantile(S[,1],0.25),quantile(S[,1],0.75)), color="grey")

ggplot(S, aes(S[,1], fill = S[,8])) + geom_density(alpha = 0.2)
ggplot(S, aes(log(S[,1]), fill = S[,8])) + geom_density(alpha = 0.2)

ggplot(S, aes(log(S[,1]), color = S[,2])) + geom_density(alpha = 0.1)
ggplot(S, aes(log(S[,1]), color = S[,3])) + geom_density(alpha = 0.1)
ggplot(S, aes(log(S[,1]), color = S[,4])) + geom_density(alpha = 0.1)



#####################################################################"TOUT


#Mise en colonne des variables domaines, sous domaines et vocation
v1=array()
v2=array()
v3=array()
v4=array()
v5=array()
data_tout2=data_tout
data_tout2=cbind(data_tout2,v1,v2,v3,v4,v5)
colnames(data_tout2)[c(35,36,37,38,39)]=c("Domaine","SousDomaine","Vocation","VA","Groupe")
labels(data_tout2)
#Modif labels
ssmod=labels(data_tout2)[[2]][18:27]
ssmod
for(i in 1:length(ssmod)){
  ssmod[i]=substr(ssmod[i],7,nchar(ssmod[i]))}
colnames(data_tout2)[18:27]=ssmod
voc=labels(data_tout2)[[2]][28:33]
voc
for(i in 1:length(voc)){
  voc[i]=substr(voc[i],5,nchar(voc[i]))}
colnames(data_tout2)[28:33]=voc

#Reshape Dom
for(i in 1 :nrow(data_tout2)){
  for(j in 1: 9){
    if(data_tout2[i,j+8]==1){data_tout2[i,35]=labels(data_tout2)[[2]][j+8]}
  }
}
data_tout2
#Reshape ssDom
for(i in 1 :nrow(data_tout2)){
  for(j in 1: 10){
    if(data_tout2[i,j+17]==1){data_tout2[i,36]=labels(data_tout2)[[2]][j+17]}
  }
}
data_tout2

#Reshape voc
for(i in 1 :nrow(data_tout2)){
  for(j in 1: 6){
    if(data_tout2[i,j+27]==1){data_tout2[i,37]=labels(data_tout2)[[2]][j+27]}
  }
}
data_tout2
data_tout2[,37]
data_tout2=data_tout2[,-c(9:33)]

#Suppression NA
for(i in 1 :nrow(data_tout2)){
  for(j in 10 :12) {
    if(is.na(data_tout2[i,j])){data_tout2[i,j]="non def"}
  }
}

#Trouver les valeurs atypiques
t=NULL
png("Tout_NuageDePoints_TOUT.png", bg="transparent", width=500, height=500)
plot(data_tout[,2], main=paste("Nuage de points des montants versés à chaque opérateur"), ylab="Montant en euro")
dev.off()
png("Tout_BoxPlot_TOUT.png", bg="transparent", width=500, height=500)
boxplot(data_tout[,2], main=paste("Boîte à moustaches de la répartition des financements"), horizontal=TRUE,xlab="Montant en euro")
dev.off()
summary(data_tout[,2])
Q1=as.numeric(quantile(data_tout[,2],0.25))
Q3=as.numeric(quantile(data_tout[,2],0.75))

Val.At.min=Q1-1.5*(Q3-Q1)
Val.At.max=Q3+1.5*(Q3-Q1)
indice=list(which(data_tout[,2]>Val.At.max | data_tout[,2]<Val.At.min))


#Récupérer Outliers

t=NULL
v=array()
t=data_tout
t=cbind(t,v)
t[,35]=rep(0,nrow(t))
for(i in 1:length(indice[[1]])){
  t[indice[[1]][i],35]=1
}
t[,35]
colnames(t)[35]="VA"
sum(t[,35])

data_tout_VA=t
write.csv2(data_tout_VA,file = "Tout_VA.csv")
data_tout2[,13]=data_tout_VA[,35]
u1=table(data_tout2[,13],data_tout2[,10])
data.frame(u1)
qchisq(u1)
write.csv2(u1,file = "Tout_VA_DOM.csv")
u2=table(data_tout2[,13],data_tout2[,11])
u2
write.csv2(u2,file = "Tout_VA_SSDOM.csv")
u3=table(data_tout2[,13],data_tout2[,12])
write.csv2(u3,file = "Tout_VA_VOC.csv")
labels(data_tout2)
write.csv2(data_tout2,file = "Tout_MOD_SSMOD_VOC.csv")
row.names(data_tout2)<-NULL
data_tout2=data_tout2[,-1]
labels(data_tout2)
#STAT DOM/SS_DOM

stat1=table(data_tout2[,11],data_tout2[,9])
stat1
write.csv2(stat1,file = "Tout_DOMbyVOC.csv")
chisq.test(stat1)# ssdom depend voc
stat2=table(data_tout2[,11],data_tout2[,12])
stat2
stat1=data.frame(stat1)
ggplot(stat1,aes(x=Var2,y=Freq,fill=Var1))+geom_bar("identity")
write.csv2(stat2,file = "Tout_VAbyVoc.csv")
chisq.test(stat2)# va depend voc
stat3=table(data_tout2[,10],data_tout2[,12])#va depend ssdom
stat3
write.csv2(stat3,file = "Tout_VAbySsDom.csv")
chisq.test(stat3)
stat4=table(data_tout2[,9],data_tout2[,12])#va depend dom
matrix(stat4)
write.csv2(stat4,file = "Tout_VAbyDom.csv")
chisq.test(stat4)




#Jeux de données sur les rangs (minimise erreur dues val atypiques)
DATATEST=NULL
DATATEST=cbind(rank(data_tout[2]),rank(data_tout[3]),data_tout[,c(4:34)])
v1=array()
v2=array()
v1=DATATEST[,3]+DATATEST[,4]+DATATEST[,5]+DATATEST[,6]+DATATEST[,7]
v2=data_tout_VA[,35]
DATATEST=cbind(DATATEST,v1,v2)
colnames(DATATEST)[34]="Nb fin."
colnames(DATATEST)[35]="VA"

colnames(DATATEST)[1]="Rang Montant"
colnames(DATATEST)[2]="Rang Population"
DATATEST=data.frame(DATATEST)
DATATEST[,1]=scale(DATATEST[,1],scale=TRUE,center=TRUE)

DATATEST[,34]=scale(DATATEST[,34],scale=TRUE,center=TRUE)
DATATEST[,33]=factor(DATATEST[,33])

#ACP
library(FactoMineR)
res.acpT=NULL
res.acpT=PCA(DATATEST[,c(1,3:16,33)],scale.unit=FALSE,quali.sup=16,
             ind.sup=which(DATATEST[,35]==1), quanti.sup=1,ncp=8, graph=F)
labels(DATATEST)
eig=res.acpT$eig
eig
write.csv2(eig,file = "Tout_VP.csv")

#Nb Axes analyse
png("Tout_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(res.acpT$eig[,2],type="l",main="Tout_Eboulli des valeurs propres")
dev.off()

NBAxe=0
for(i in 1:nrow(res.acpT$eig)){
  if(res.acpT$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
res.acpT$ind$cos2

sjp.pca(res.acpT$ind$coord, plotEigenvalues=T, type="circle")

#Projections
png("Tout_Proj_Ind_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 2),choix="ind")
dev.off()
png("Tout_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 2), choix="var")
dev.off()
png("Tout_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 3), choix="ind")
dev.off()
png("Tout_Proj_Var_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 3), choix="var")
dev.off()
png("Tout_Proj_Ind_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(2, 3), choix="ind")
dev.off()
png("Tout_Proj_Var_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 3), choix="var")
dev.off()
png("Tout_Proj_Ind_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 4), choix="ind")
dev.off()
png("Tout_Proj_Var_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 4), choix="var")
dev.off()
png("Tout_Proj_Ind_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(2, 4), choix="ind")
dev.off()
png("Tout_Proj_Var_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 4), choix="var")
dev.off()
png("Tout_Proj_Ind_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, ,label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(3, 4), choix="ind")
dev.off()
png("Tout_Proj_Var_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(3, 4), choix="var")
dev.off()


t=res.acpT$var$cor
write.csv2(t,file = "Tout_ACP_corr.csv")

library(ggplot2)
library(reshape2)
#Heatmap : correlations
png("Tout_Corr_VariablesAC.png", bg="transparent", width=1500, height=1500
    qplot(x=Var1, y=Var2, data=melt(res.acpT$var$cor),  fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1),high = "green", low="blue")
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
write.csv2(t,file = "Tout_ACP_contrib_var.csv")
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
write.csv2(t,file = "Tout_ACP_contrib_ind.csv")

TtestKhi=data.frame(t) 
stat5=table(TtestKhi[,1],data_tout2[which(data_tout2[,12]==0),9])
stat5
write.csv2(stat5,file = "Tout_ContribByDomAxe1.csv")
rowSums(stat4)
chisq.test(stat5, rowSums(stat4),simulate.p.value = TRUE) 

#A CHANGER POUR AUTRES
RepartDom=c(174,57,49,59,34,89,10,400,349)
RepartDom=RepartDom/1221

stat5Bis=colSums(stat5)
chisq.test(stat5Bis,p=RepartDom,simulate.p.value = TRUE)

stat6=table(TtestKhi[,2],data_tout2[which(data_tout2[,11]==0),8])
stat6
write.csv2(stat6,file = "Tout_ContribByDomAxe2.csv")
chisq.test(stat6)

stat6Bis=colSums(stat6)
chisq.test(stat6Bis,p=RepartDom,simulate.p.value = TRUE)

stat7=table(TtestKhi[,3],data_tout2[which(data_tout2[,11]==0),8])
stat7
write.csv2(stat7,file = "Tout_ContribByDomAxe3.csv")
chisq.test(stat7,simulate.p.value = TRUE)

stat7Bis=colSums(stat7)
chisq.test(stat7Bis,p=RepartDom,simulate.p.value = TRUE)


stat8=table(TtestKhi[,4],data_tout2[which(data_tout2[,11]==0),8])
stat8
write.csv2(stat8,file = "Tout_ContribByDomAxe4.csv")
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
write.csv2(t,file = "Tout_ACP_qualite_var.csv")

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
write.csv2(t,file = "Tout_ACP_qualite_ind.csv")


#la classification
#CAH


DATATEST=lapply(DATATEST,as.numeric)
DATATEST<-data.frame(DATATEST)
res.acpT2=PCA(DATATEST[,c(1,3:16,33)],scale.unit=F,
              ind.sup=which(DATATEST[,35]==1), quanti.sup=1,ncp=8, graph=F)
res.hcpc=NULL
res.hcpc = HCPC(res.acpT2, metric="euclidean", method="ward.D",graph=F)
cluster=res.hcpc$data.clust
png("Tout_CAH_Arbre.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="tree")
dev.off()
png("Tout_CAH_barplot.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="bar")
dev.off()
png("Tout_CAH_map_1_2.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,2))
dev.off()
png("Tout_CAH_map_2_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,3))
dev.off()
png("Tout_CAH_map_1_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,3))
dev.off()
png("Tout_CAH_map_1_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,4))
dev.off()
png("Tout_CAH_map_2_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,4))
dev.off()
png("Tout_CAH_map_3_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(3,4))
dev.off()

#Récupération des groupes (5 classes)
v=array()
res=cbind(DATATEST,v)
labels(res)
res[,36]=NA
for(i in 1:nrow(cluster)){
  for(j in 1:nrow(res)){
    if(j==rownames(cluster)[i]){
      res[j,36]=cluster$clust[i]
    }
  }

}
for(i in 1:nrow(res)){
  if(is.na(res[i,36])){
    res[i,36]=4 
  }
}
labels(res)
colnames(res)[36]="Cluster"
res=cbind(data_tout[,1:2],res[,3:36])
res=data.frame(res,row.names=1)
write.csv2(res,file = "DATA_TOUT_Class.csv")
colnames(res)[35]

#Description modalité
#Description modalité

write.csv2(condes(res,num.var=33,proba=0.05)$quanti,file = "Tout_DescCorr33.csv")
write.csv2(condes(res,num.var=1,proba=0.05)$quanti,file = "Tout_DescCorr1.csv")

write.csv2(condes(res,num.var=34,proba=0.05)$quanti,file = "Tout_DescCorr34.csv")
write.csv2(condes(res,num.var=35,proba=0.05)$quanti,file = "Tout_DescCorr35.csv")
#Moyennes par classes

png("Poit_Boxplot_SommeClass.png", bg="transparent", width=500, height=500)
layout(matrix(c(1:4),2,2))
for(i in 1:4){boxplot(res[which(res[,35]==i),1],main=paste(names(res)[1],"groupe",i))}
layout(1)
dev.off()

png("Hist_NbFinClass.png", bg="transparent", width=500, height=500)
layout(matrix(c(1:4),2,2))
for(i in 1:4){hist(res[which(res[,35]==i),33],main=paste(names(res)[33],"groupe",i),xlab="NB.FIN")}
layout(1)
dev.off()

resumeMONTANT<-aggregate(res[1],res[35], summary)
write.csv2(resumeMONTANT,file = "Tout_Tab_resume_somme.csv")
resumeRESTE<-aggregate(res[,c(-1,-2,-33,-35)],res[35], sum)
write.csv2(resumeRESTE,file = "Tout_Tab_resume_dom_voc_classe.csv")
t1=table(res[,1],res[,33]) #nb fin/montant
write.csv2(t1,file = "Tout_Tab_nbfin_montant.csv")
t2=table(rowSums(res[,3:7]),res[,34]) #nb fin/classe
write.csv2(t2,file = "Tout_Tab_nbfin_classe.csv")


LM=data.frame(DATA_TOUT_PF,row.names=1)

#######TESTS
#DISTRIBUTION NORMALE ?
ggplot(T_Tout,aes(x=T_Tout[,1]))+
  geom_histogram(aes(y=..density..),binwidth=.5, colour="grey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
t1.1=ks.test(T_Tout[,1],"pnorm")
rt1.1=c(t1.1$data.name,t1.1$method,t1.1$alternative,t1.1$statistic,t1.1$p.value)
names(rt1.1)<-c("Données","Methode","Alternative","Stat","P-value")
t1.2=ks.test(T_Tout[which(DATATEST[,34]==1),1],"pnorm")
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
#####################################################################"LORRAINE


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
DATATEST[,1]=scale(DATATEST[,1],scale=TRUE,center=TRUE)
DATATEST[,33]=factor(DATATEST[,33])

#ACP
library(FactoMineR)
res.acpT=NULL
res.acpT=PCA(DATATEST[,c(1,3:16,33)],scale.unit=FALSE,quali.sup=16,
             ind.sup=which(DATATEST[,34]==1), quanti.sup=1,ncp=8, graph=F)
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
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 2),choix="ind")
dev.off()
png("Lorr_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 2), choix="var")
dev.off()
png("Lorr_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 3), choix="ind")
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
plot.PCA(res.acpT, ,label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(3, 4), choix="ind")
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

#A CHANGER POUR AUTRES
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
DATATEST=lapply(DATATEST,as.numeric)
DATATEST<-data.frame(DATATEST)
res.acpT2=PCA(DATATEST[,c(1,3:16,33)],scale.unit=F,
             ind.sup=which(DATATEST[,34]==1), quanti.sup=1,ncp=8, graph=F)
res.hcpc=NULL
res.hcpc = HCPC(res.acpT2, metric="euclidean", method="ward.D",graph=F)
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
      res[j,36]=cluster[i,34]
      print("_________")
      print(i)
      print(j)
      print(rownames(cluster)[i])
    }
  }
}
for(i in 1:nrow(res)){
  if(is.na(res[i,36])){
    res[i,36]=4 
  }
}
labels(res)
colnames(res)[36]="Cluster"
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
ggplot(T_Lorr,aes(x=T_Lorr[,1]))+
  geom_histogram(aes(y=..density..),binwidth=.5, colour="grey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
t1.1=ks.test(T_Lorr[,1],"pnorm")
rt1.1=c(t1.1$data.name,t1.1$method,t1.1$alternative,t1.1$statistic,t1.1$p.value)
names(rt1.1)<-c("Données","Methode","Alternative","Stat","P-value")
t1.2=ks.test(T_Lorr[which(DATATEST[,34]==1),1],"pnorm")
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
boxplot(CLASSE[,1]~CLASSE[,9])
boxplot(LM[which(LM[,34]!=4),1]~LM[which(LM[,34]!=4),34])
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==1),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==2),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==3),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(LM[which(LM[,34]==4),c(-36,-37,-38,-39)]),method="spearman"), fill=value, geom="tile", na.rm=T)  +   scale_fill_gradient2(limits=c(-1, 1))
abline(lm(LM[,1]~LM[,34]),col="green")
m1<-lm(SOMME~)
#####################################################################"POITOU-CHARENTE


#Mise en colonne des variables domaines, sous domaines et vocation
v1=array()
v2=array()
v3=array()
v4=array()
v5=array()
data_poit2=data_poit
data_poit2=cbind(data_poit2,v1,v2,v3,v4,v5)
colnames(data_poit2)[c(34,35,36,37,38)]=c("Domaine","SousDomaine","Vocation","VA","Groupe")
labels(data_poit2)
#Modif labels
ssmod=labels(data_poit2)[[2]][18:27]
ssmod
for(i in 1:length(ssmod)){
  ssmod[i]=substr(ssmod[i],7,nchar(ssmod[i]))}
colnames(data_poit2)[18:27]=ssmod
voc=labels(data_poit2)[[2]][28:33]
voc
for(i in 1:length(voc)){
  voc[i]=substr(voc[i],5,nchar(voc[i]))}
colnames(data_poit2)[28:33]=voc

#Reshape Dom
for(i in 1 :nrow(data_poit2)){
  for(j in 1: 9){
    if(data_poit2[i,j+8]==1){data_poit2[i,34]=labels(data_poit2)[[2]][j+8]}
  }
}
data_poit2
#Reshape ssDom
for(i in 1 :nrow(data_poit2)){
  for(j in 1: 10){
    if(data_poit2[i,j+17]==1){data_poit2[i,35]=labels(data_poit2)[[2]][j+17]}
  }
}
data_poit2

#Reshape voc
for(i in 1 :nrow(data_poit2)){
  for(j in 1: 6){
    if(data_poit2[i,j+27]==1){data_poit2[i,36]=labels(data_poit2)[[2]][j+27]}
  }
}
data_poit2
data_poit2[,36]
data_poit2=data_poit2[,-c(9:33)]

#Suppression NA
for(i in 1 :nrow(data_poit2)){
  for(j in 10 :11) {
    if(is.na(data_poit2[i,j])){data_poit2[i,j]="non def"}
  }
}

#Trouver les valeurs atypiques
t=NULL
png("Poit_NuageDePoints_TOUT.png", bg="transparent", width=500, height=500)
plot(data_poit[,2], main=paste("Nuage de points des montants versés à chaque opérateur"), ylab="Montant en euro")
dev.off()
png("Poit_BoxPlot_TOUT.png", bg="transparent", width=500, height=500)
boxplot(data_poit[,2], main=paste("Boîte à moustaches de la répartition des financements"), horizontal=TRUE,xlab="Montant en euro")
dev.off()
summary(data_poit[,2])
Q1=as.numeric(quantile(data_poit[,2],0.25))
Q3=as.numeric(quantile(data_poit[,2],0.75))

Val.At.min=Q1-1.5*(Q3-Q1)
Val.At.max=Q3+1.5*(Q3-Q1)
indice=list(which(data_poit[,2]>Val.At.max | data_poit[,2]<Val.At.min))


#Récupérer Outliers

t=NULL
v=array()
t=data_poit
t=cbind(t,v)
t[,34]=rep(0,nrow(t))
for(i in 1:length(indice[[1]])){
  t[indice[[1]][i],34]=1
}
t[,34]
colnames(t)[34]="VA"
sum(t[,34])

data_poit_VA=t
write.csv2(data_poit_VA,file = "Poit_VA.csv")
data_poit2[,12]=data_poit_VA[,34]
u1=table(data_poit2[,12],data_poit2[,9])
write.csv2(u1,file = "Poit_VA_DOM.csv")
u2=table(data_poit2[,12],data_poit2[,10])
write.csv2(u2,file = "Poit_VA_SSDOM.csv")
u3=table(data_poit2[,12],data_poit2[,11])
write.csv2(u3,file = "Poit_VA_VOC.csv")
data_poit2[,11]
labels(data_poit2)
write.csv2(data_poit2,file = "Poit_MOD_SSMOD_VOC.csv")
row.names(data_poit2)<-NULL
data_poit2=data_poit2[,-1]
labels(data_poit2)
#STAT DOM/SS_DOM

stat1=table(data_poit2[,10],data_poit2[,9])
stat1
write.csv2(stat1,file = "Poit_DOMbyVOC.csv")
chisq.test(stat1,simulate.p.value = TRUE)# ssdom depend voc
stat1=table(data_poit2[which(data_poit2[,10]!="non def"),10],data_poit2[which(data_poit2[,10]!="non def"),9])
stat1
stat2=table(data_poit2[,11],data_poit2[,10])
stat2
write.csv2(stat2,file = "Poit_VAbyVoc.csv")
chisq.test(stat2,simulate.p.value = TRUE)# va depend voc
stat2=data=data.frame(stat2)
ggplot(factor(stat2$Var1), data=stat2,fill=stat2$Var2, geom="bar",position="fill")
hist(stat2)
stat3=table(data_poit2[,9],data_poit2[,11])#va depend ssdom
stat3
write.csv2(stat3,file = "Poit_VAbySsDom.csv")
chisq.test(stat3,simulate.p.value = TRUE)
stat4=table(data_poit2[,8],data_poit2[,11])#va depend dom
stat4
write.csv2(stat4,file = "Poit_VAbyDom.csv")
chisq.test(stat4,simulate.p.value = TRUE)




#Jeux de données sur les rangs (minimise erreur dues val atypiques)
DATATEST=NULL
DATATEST=cbind(rank(data_poit[2]),rank(data_poit[3]),data_poit[,c(4:33)])
v1=array()
v2=array()
v1=DATATEST[,3]+DATATEST[,4]+DATATEST[,5]+DATATEST[,6]+DATATEST[,7]
v2=data_poit_VA[,34]
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
DATATEST[,1]=scale(DATATEST[,1],scale=TRUE,center=TRUE)
DATATEST[,33]=factor(DATATEST[,33])

#ACP
library(FactoMineR)
res.acpT=NULL
res.acpT=PCA(DATATEST[,c(1,3:16,33)],scale.unit=FALSE,quali.sup=16,
             ind.sup=which(DATATEST[,34]==1), quanti.sup=1,ncp=8, graph=F)
labels(DATATEST)
eig=res.acpT$eig
eig
write.csv2(eig,file = "Poit_VP.csv")

#Nb Axes analyse
png("Poit_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(res.acpT$eig[,2],type="l",main="Poit_Eboulli des valeurs propres")
dev.off()

NBAxe=0
for(i in 1:nrow(res.acpT$eig)){
  if(res.acpT$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
res.acpT$ind$cos2


#Projections
png("Poit_Proj_Ind_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 2),choix="ind")
dev.off()
png("Poit_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 2), choix="var")
dev.off()
png("Poit_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 3), choix="ind")
dev.off()
png("Poit_Proj_Var_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 3), choix="var")
dev.off()
png("Poit_Proj_Ind_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(2, 3), choix="ind")
dev.off()
png("Poit_Proj_Var_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 3), choix="var")
dev.off()
png("Poit_Proj_Ind_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(1, 4), choix="ind")
dev.off()
png("Poit_Proj_Var_1_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(1, 4), choix="var")
dev.off()
png("Poit_Proj_Ind_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, label=c("var","quali"),select="cos2 0.7",habillage=17,axes=c(2, 4), choix="ind")
dev.off()
png("Poit_Proj_Var_2_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(2, 4), choix="var")
dev.off()
png("Poit_Proj_Ind_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, ,label=c("var","quali"),select="cos2 0.7",habillage=16,axes=c(3, 4), choix="ind")
dev.off()
png("Poit_Proj_Var_3_4.png", bg="transparent", width=500, height=500)
plot.PCA(res.acpT, axes=c(3, 4), choix="var")
dev.off()


t=res.acpT$var$cor
write.csv2(t,file = "Poit_ACP_corr.csv")

library(ggplot2)
library(reshape2)
#Heatmap : correlations
png("Poit_Corr_VariablesAC.png", bg="transparent", width=1500, height=1500)
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
write.csv2(t,file = "Poit_ACP_contrib_var.csv")
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
write.csv2(t,file = "Poit_ACP_contrib_ind.csv")

TtestKhi=data.frame(t) 
stat5=table(TtestKhi[,1],data_poit2[which(data_poit2[,11]==0),8])
stat5
write.csv2(stat5,file = "Poit_ContribByDomAxe1.csv")

chisq.test(stat5,simulate.p.value = TRUE) 

#A CHANGER POUR AUTRES
RepartDom=c(174,57,49,59,34,89,10,400,349)
RepartDom=RepartDom/1221

stat5Bis=colSums(stat5)
chisq.test(stat5Bis,p=RepartDom,simulate.p.value = TRUE)

stat6=table(TtestKhi[,2],data_poit2[which(data_poit2[,11]==0),8])
stat6
write.csv2(stat6,file = "Poit_ContribByDomAxe2.csv")
chisq.test(stat6)

stat6Bis=colSums(stat6)
chisq.test(stat6Bis,p=RepartDom,simulate.p.value = TRUE)

stat7=table(TtestKhi[,3],data_poit2[which(data_poit2[,11]==0),8])
stat7
write.csv2(stat7,file = "Poit_ContribByDomAxe3.csv")
chisq.test(stat7,simulate.p.value = TRUE)

stat7Bis=colSums(stat7)
chisq.test(stat7Bis,p=RepartDom,simulate.p.value = TRUE)


stat8=table(TtestKhi[,4],data_poit2[which(data_poit2[,11]==0),8])
stat8
write.csv2(stat8,file = "Poit_ContribByDomAxe4.csv")
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
write.csv2(t,file = "Poit_ACP_qualite_var.csv")

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
write.csv2(t,file = "Poit_ACP_qualite_ind.csv")


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
DATATEST=lapply(DATATEST,as.numeric)
DATATEST<-data.frame(DATATEST)
res.acpT2=PCA(DATATEST[,c(1,3:16,33)],scale.unit=F,
              ind.sup=which(DATATEST[,34]==1), quanti.sup=1,ncp=8, graph=F)
res.hcpc=NULL
res.hcpc = HCPC(res.acpT2, metric="euclidean", method="ward.D",graph=F)
cluster=res.hcpc$data.clust
png("Poit_CAH_Arbre.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="tree")
dev.off()
png("Poit_CAH_barplot.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="bar")
dev.off()
png("Poit_CAH_map_1_2.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,2))
dev.off()
png("Poit_CAH_map_2_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,3))
dev.off()
png("Poit_CAH_map_1_3.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,3))
dev.off()
png("Poit_CAH_map_1_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(1,4))
dev.off()
png("Poit_CAH_map_2_4.png", bg="transparent", width=1500, height=1500)
plot(res.hcpc, choice="map",axes=c(2,4))
dev.off()
png("Poit_CAH_map_3_4.png", bg="transparent", width=1500, height=1500)
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
res=cbind(data_poit[,1:3],res[,3:35])
res=res[,-35]
res=data.frame(res,row.names=1)
write.csv2(res,file = "DATA_LPOIT_Class.csv")


#Description modalité
#Description modalité
 
write.csv2(condes(res,num.var=33,proba=0.05)$quanti,file = "Poit_DescCorr33.csv")
write.csv2(condes(res,num.var=1,proba=0.05)$quanti,file = "Poit_DescCorr1.csv")

write.csv2(condes(res,num.var=34,proba=0.05)$quanti,file = "Poit_DescCorr34.csv")

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
write.csv2(resumePOP,file = "Poit_Tab_resume_pop.csv")
resumeMONTANT<-aggregate(res[1],res[34], summary)
write.csv2(resumeMONTANT,file = "Poit_Tab_resume_somme.csv")
resumeRESTE<-aggregate(res[,c(-1,-2,-33,-34)],res[34], sum)
write.csv2(resumeRESTE,file = "Poit_Tab_resume_dom_voc_classe.csv")
t1=table(res[,1],rowSums(res[3:7])) #nb fin/montant
write.csv2(t1,file = "Poit_Tab_nbfin_montant.csv")
t2=table(rowSums(res[,3:7]),res[,34]) #nb fin/classe
write.csv2(t2,file = "Poit_Tab_nbfin_classe.csv")


LM=data.frame(DATA_POIT_PF,row.names=1)

#######TESTS
#DISTRIBUTION NORMALE ?
ggplot(T_Poit,aes(x=T_Poit[,1]))+
  geom_histogram(aes(y=..density..),binwidth=.5, colour="grey", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")
t1.1=ks.test(T_Poit[,1],"pnorm")
rt1.1=c(t1.1$data.name,t1.1$method,t1.1$alternative,t1.1$statistic,t1.1$p.value)
names(rt1.1)<-c("Données","Methode","Alternative","Stat","P-value")
t1.2=ks.test(T_Poit[which(DATATEST[,34]==1),1],"pnorm")
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
