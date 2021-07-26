#Les données
library(ade4)


data_lorr=data.frame(DATA.LORRAINE)
index=c(1:nrow(data_lorr))
Operateur=data_lorr[,1]
labels(data_lorr)
data_lorr=data_lorr[,-34]

lorr_stat<-data_lorr



#Mise en colonne des variables domaines, sous domaines et vocation
v1=array()
v2=array()
v3=array()
v4=array()
v5=array()
data_lorr2=data_lorr
data_lorr2=cbind(data_lorr2,v1,v2,v3,v4,v5)
colnames(data_lorr2)[c(34,35,36,37,38)]=c("Domaine","SousDomaine","Vocation","VA","Groupe")
labels(data_lorr)
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

#Reshape voc
for(i in 1 :nrow(data_lorr2)){
  for(j in 1: 6){
    if(data_lorr2[i,j+27]==1){data_lorr2[i,36]=labels(data_lorr2)[[2]][j+27]}
  }
}
data_lorr2[,36]
data_lorr2=data_lorr2[,-c(9:33)]

data_lorr[,2]
#Suppression NA
for(i in 1 :nrow(data_lorr2)){
  for(j in 10 :11) {
  if(is.na(data_lorr2[i,j])){data_lorr2[i,j]="non def"}
  }
}

#Trouver les valeurs atypiques
t=NULL
TrouveValAtypiq<-function(t){
  indice=NULL
  
  indice=matrix(data=NA, ncol=1,nrow=9)
  tab_temp=NULL
  for(m in 9:17){
        tab_temp=NULL
       
        tab_temp=t[,m]*t[,2]
        png(paste("Lorr_NuageDePoints_",m,".png"), bg="transparent", width=500, height=500)
        plot(tab_temp[which(tab_temp!=0)], main=paste("Nuage de points des montants versés à chaque opérateur \n du domaine",m-8), ylab="Montant en euro")
        dev.off()
        png(paste("Lorr_BoxPlot_",m,".png"), bg="transparent", width=500, height=500)
        boxplot(tab_temp[which(tab_temp!=0)], main=paste("Boîte à moustaches de la répartition des financements \n du domaine",m-8), horizontal=TRUE,xlab="Montant en euro")
        dev.off()
        print(summary(tab_temp))
        Q1=as.numeric(quantile(tab_temp[which(tab_temp!=0)],0.25))
        Q3=as.numeric(quantile(tab_temp[which(tab_temp!=0)],0.75))
    
        Val.At.min=Q1-1.5*(Q3-Q1)
        Val.At.max=Q3+1.5*(Q3-Q1)
        indice[m-8]=list(which((tab_temp>Val.At.max | tab_temp<Val.At.min) & tab_temp!=0))
         
  }
  return (indice)
  }  
e=TrouveValAtypiq(data_lorr)
e
#Compte nb va
CompteVA<-function(liste){
  d=array()
  for(i in 1:length(liste)){
    d=c(d,print(length(liste[[i]])))
  }
return(d)
}
rowSums(rowSums(CompteVA(e)))


#Récupérer Outliers
RecupVA<-function(t,liste){
  f=array()
  for(i in 1:length(liste)){
    f=c(f,liste[[i]])
  }
  f=f[-1]
  
  v=array()
  t=data_lorr
  t=cbind(t,v)
  t[,35]=rep(0,nrow(t))
  t[f,35]=rep(1,length(f))
  colnames(t)[35]="VA"
  return(t)
}


data_lorr_VA=RecupVA(data_lorr,e)
write.csv2(data_lorr_VA,file = "Lorr_VA.csv")
u1=table(data_lorr2[,12],data_lorr2[,9])
write.csv2(u1,file = "Lorr_VA_DOM.csv")
u2=table(data_lorr2[,12],data_lorr2[,10])
write.csv2(u2,file = "Lorr_VA_SSDOM.csv")
u3=table(data_lorr2[,12],data_lorr2[,10])
write.csv2(u3,file = "Lorr_VA_VOC.csv")
data_lorr2[,12]=data_lorr_VA[,35]

write.csv2(data_lorr2,file = "Lorr_MOD_SSMOD_VOC.csv")
row.names(data_lorr2)<-NULL
data_lorr2=data_lorr2[,-1]
labels(data_lorr2)
#STAT DOM/SS_DOM
stat1=table(data_lorr2[,9],data_lorr2[,10])
stat1
chisq.test(stat1,simulate.p.value = TRUE)# ssdom depend voc
stat2=table(data_lorr2[,10],data_lorr2[,11])
stat2
chisq.test(stat2,simulate.p.value = TRUE)# va depend voc
stat3=table(data_lorr2[,9],data_lorr2[,11])#va depend ssdom
stat3
chisq.test(stat3,simulate.p.value = TRUE)
stat4=table(data_lorr2[,8],data_lorr2[,11])#va depend dom
stat4
chisq.test(stat4,simulate.p.value = TRUE)


#Jeux de données sur les rangs (minimise erreur dues val atypiques)
 data_lorr3=cbind(rank(data_lorr2[1]),rank(data_lorr2[2]),data_lorr2[,c(3:11)])
labels(data_lorr3)
#data_lorr3=cbind(data_lorr2[,1],rank(data_lorr2[2]),data_lorr2[,c(3:34)])
v=array()
v=data_lorr4[,3]+data_lorr4[,4]+data_lorr4[,5]+data_lorr4[,6]+data_lorr4[,7]
DATATEST=cbind(data_lorr4,v)
colnames(DATATEST)[34]="Nb fin."
for(i in 1:nrow(DATATEST)){
  if(DATATEST[i,34]==1){
    DATATEST[i,34]="1f."
  }
  if(DATATEST[i,34]==2){
    DATATEST[i,34]="2f."
  }
  if(DATATEST[i,34]==3){
    DATATEST[i,34]="3f."
  }
  if(DATATEST[i,34]==4){
    DATATEST[i,34]="4f."
  }
  if(DATATEST[i,34]==5){
    DATATEST[i,34]="5f."
  }
}
DATATEST=data.frame(DATATEST)
#Centrage et réduction des données
#data_lorr3CR<-cbind(scale(data_lorr3[,1:7], center = TRUE, scale = TRUE),data_lorr3[,c(8:11)])
#data_lorr3CR<-cbind(data_lorr3[,1],scale(data_lorr3[,c(-1,-33,-34)], center = TRUE, scale = TRUE),data_lorr3[,c(33,34)])
#data_lorr3CR=data.frame(data_lorr3CR)

#ACP
library(FactoMineR)
#res.acp=PCA(data_lorr3[,c(-9,-10,-11)],scale.unit=T,quanti.sup=1,quali.sup=8,
 #            ind.sup=which(data_lorr3CR[,11]==1), ncp=7, graph=T)
res.acpT=NULL
res.acpT=PCA(DATATEST[,-c(17:33)],scale.unit=T,quanti.sup=1,quali.sup=17,
            ind.sup=which(DATATEST[,33]==1), ncp=8, graph=T)
labels(DATATEST)
res.acpT$
labels(data_lorr3CR)
eig=res.acpT$eig
eig
write.csv2(eig,file = "Lorr_VP.csv")

#Nb Axes analyse
png("Lorr_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(res.acpT$eig[,2],type="l",main="Eboulli des valeurs propres")
dev.off()

boxplot( data_lorr[,1])

NBAxe=0
for(i in 1:nrow(res.acpT$eig)){
  if(res.acpT$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
res.acpT$ind$cos2
#Description axes
dimdesc(res.acpT, axe=1:4, proba=0.05)

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

TtestKhi=data.frame(t) #
stat5=table(TtestKhi[,1],data_lorr2[which(data_lorr2[,11]==0),8])
stat5
#chisq.test(stat5) A CAUSE COL. PLU

RepartDom=c(174,57,49,59,34,89,10,400,349)
RepartDom=RepartDom/1221

stat5Bis=colSums(stat5)
chisq.test(stat5Bis,p=RepartDom,simulate.p.value = TRUE)

#Int Conf
#B.inf=RepartDom-1.96*(sqrt(RepartDom*(1-RepartDom)/1221))
#B.sup=RepartDom+1.96*(sqrt(RepartDom*(1-RepartDom)/1221))
#for(i in 1:9){
#  if((stat5[1,i])/rowSums(stat5)[1]<B.inf[i]|(stat5[1,i])/rowSums(stat5)[1]>B.sup[i]){
    
#  }
#}
#RepartDom2=rbind(RepartDom, RepartDom)
#chisq.test(stat5,p=RepartDom,simulate.p.value = TRUE)$residual

stat6=table(TtestKhi[,2],data_lorr2[which(data_lorr2[,11]==0),8])
stat6
chisq.test(stat6,simulate.p.value = TRUE)
$expected
$residual
stat6Bis=colSums(stat6)
chisq.test(stat6Bis,p=RepartDom,simulate.p.value = TRUE)

stat7=table(TtestKhi[,3],data_lorr2[which(data_lorr2[,11]==0),8])
stat7
chisq.test(stat7,simulate.p.value = TRUE)

stat7Bis=colSums(stat7)
chisq.test(stat7Bis,p=RepartDom,simulate.p.value = TRUE)
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

Quali=cbind(res.acpT$ind$cos2[,1]+res.acpT$ind$cos2[,2],res.acpT$ind$cos2[,1]+res.acpT$ind$cos2[,3],res.acpT$ind$cos2[,3]+res.acpT$ind$cos2[,2])
colnames(Quali)=c("1+2","1+3","2+3")
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

#Prepa ACP ok
#data_lorr4=data_lorr
#data_lorr4=data_lorr4[,-1]
v6=array()
#data_lorr4=cbind(data_lorr4,v6)
DATATEST=cbind(DATATEST,v6)
data_lorr4[,34]=data_lorr_VA[,35]
colnames(data_lorr4)[33]<-"VA"
#Jeux de données sur les rangs (minimise erreur dues val atypiques)
data_lorr4=cbind(rank(data_lorr4[1]),rank(data_lorr4[2]),data_lorr4[,c(3:33)])
labels(data_lorr4)
#Centrage et réduction des données
#data_lorr4CR<-cbind(scale(data_lorr4[,1:7], center = TRUE, scale = TRUE),data_lorr4[,c(8:33)])
#labels(data_lorr4CR)
res.acpT2=PCA(DATATEST[,-c(32:34)],scale.unit=T,quanti.sup=1,
             ind.sup=which(DATATEST[,33]==1), ncp=8, graph=T)
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

cluster[,c17)]
res=data.frame(data_lorr_VA, row.names=1)
res[,33]=NA
for(i in 1:nrow(cluster)){
  for(j in 1:nrow(res)){
  if(j==rownames(cluster)[i]){
    res[j,33]=cluster[i,34]
    print("_________")
    print(i)
    print(j)
    print(rownames(cluster)[i])
  }
  }
}
colnames(res)[33]="Cluster"
colnames(res)[16]="TR"
labels(cluster)
[,33]
rownames(cluster)
)
DATA_LORR_Class=res
v=array()
v=DATA_LORR_Class[,3]+DATA_LORR_Class[,4]+DATA_LORR_Class[,5]+DATA_LORR_Class[,6]+DATA_LORR_Class[,7]
DATA_LORR_Class=cbind(DATA_LORR_Class,v)
colnames(DATA_LORR_Class)[35]="Nb fin."
write.csv2(DATA_LORR_Class,file = "DATA_LORR_Class.csv")

#Stat nb. fin
chisq.test(cbind(DATA_LORR_Class[35],DATA_LORR_Class[which,1]))

#Description modalité
catdes(res,num.var=33, proba=0.05)

condes(res,num.var=1,proba=0.05)

#Moyennes par classes


layout(matrix(c(1:6),2,3))
for(i in 1:5){boxplot(res[which(res[,33]==i),1],main=paste(names(res)[1],"groupe",i))}
layout(1)
layout(matrix(c(1:6),2,3))
for(i in 1:5){boxplot(res[which(res[,33]==i),2],main=paste(names(res)[2],"groupe",i))}
layout(1)


resumePOP<-aggregate(res[2],res[33], summary)
write.csv2(resumePOP,file = "Lorr_Tab_resume_pop.csv")
resumeMONTANT<-aggregate(res[1],res[33], summary)
write.csv2(resumeMONTANT,file = "Lorr_Tab_resume_somme.csv")
resumeRESTE<-aggregate(res[,c(-1,-2,-33)],res[33], sum)
write.csv2(resumeRESTE,file = "Lorr_Tab_resume_dom_voc_classe.csv")
t1=table(res[,1],rowSums(res[,3:7])) #nb fin/montant
write.csv2(t1,file = "Lorr_Tab_nbfin_montant.csv")
t2=table(rowSums(res[,3:7]),res[,33]) #nb fin/classe
write.csv2(t2,file = "Lorr_Tab_nbfin_classe.csv")

plot(res[,1],res[,3])
labels(DATA_LORR_Class)



##########################RLM

Mod_Optimal=matrix(data=NULL,,33+5)
Combin=matrix(,)
for(i in 1:length(labels(DATA_LORR_Class)[[2]])){
Combin=combn(labels(DATA_LORR_Class)[[2]],i)

}
Combin
#LM=DATA_LORR_Class[-which(DATA_LORR_Class[,34]==1),]
LM=DATA_LORR_Class
model<-lm(SOMME~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+TR+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.F+VOC.C+Cluster+VA,LM)

model
attributes(model)
summary(model)
plot(model)

#Etude des résidus
e<-model$residuals
qqnorm(e,datax=T,ylab="Quantiles obs",xlab="Quantile Th")
ddl=nrow(LM)-length(model$coefficients)-3
#calcul res standard.
res.standard<-rstandard(model)
alpha<-0.1
seuil.standar<-qt(1-alpha/2,ddl)
plot(LM[,1],res.standard)
abline(h=-seuil.standar)
abline(h=seuil.standar)
abline(h=0)
ab.standar<-DATA_LORR_Class[res.standard< -seuil.standar|res.standard>seuil.standar,]
nrow(ab.standar)

#calcul res studentisé
res.student<-rstudent(model)
seuil.student<-qt(1-alpha/2,ddl)
plot(LM[,1],res.student)
abline(h=-seuil.student)
abline(h=seuil.student)
abline(h=0)
ab.student<-DATA_LORR_Class[res.student< -seuil.student|res.student>seuil.student,]
nrow(ab.student)

#Point de levier
atypiques<-influence.measures(model)
attributes(atypiques)
atypiques$infmat
res.hat<-atypiques$infmat[,"hat"]
seuil.hat<-2*(length(model$coefficients)-1+1)/nrow(LM)
ab.hat<-LM[res.hat>seuil.hat,]
nrow(ab.hat)
#Traitement des points atypiques

b.suspicious<-rbind(ab.hat,ab.standar,ab.student)
nrow(b.suspicious)
b.suspicious=unique(b.suspicious)
nrow(b.suspicious)
DATA_LORR_CLEAN=LM
for(i in 1:nrow(DATA_LORR_CLEAN)){
for(j in 1:nrow(b.suspicious)){
  if(rownames(DATA_LORR_CLEAN)[i]==rownames(b.suspicious)[j]){DATA_LORR_CLEAN=DATA_LORR_CLEAN[-i,]
  print(c(i,j))}
  
}
}
nrow(DATA_LORR_CLEAN)

#Sur modele clean

model_clean<-lm(SOMME~Population+E+R+D+EPCI+C+SV+APV+CA+LL+PAT+M+PLU+AD+TR+SSDOM.TH+SSDOM.ADR+SSDOM.C+SSDOM.M+SSDOM.D+SSDOM.AL+SSDOM.PLU+SSDOM.AP+SSDOM.PV+SSDOM.ADEC+VOC.E+VOC.D+VOC.F+VOC.C+Cluster+VA,DATA_LORR_CLEAN)
summary(model_clean)
plot(model_clean)
#Detection colinéarité si supp R2 mauvais
mcxx<-cor(DATA_LORR_CLEAN[,-1])
mcxx<-mcxx^2
mcxx
which((which(mcxx>0.4164))!=(which(mcxx==1)))

#Colinéarité VIF
n=ncol(DATA_LORR_CLEAN)
r2<-double(n)

for(j1 in 1:(n)){
  str_formule <- paste(names(DATA_LORR_CLEAN[j1]),"~")
  for(j2 in 1:(n)){
    if(j2!=j1){
      str_formule <- paste(str_formule, names(DATA_LORR_CLEAN[j2]),"+")
    }
  }
}
str_formule<-substr(str_formule,1,nchar(str_formule)-2)
str_formule
formule <- as.formula(str_formule)
regtest <- lm(formule,data=DATA_LORR_CLEAN)
resume.regtest <-summary(regtest)
resume.regtest$r.squared
r2[j1] <-resume.regtest$r.squared
vif <- 1/(1-r2)
names(vif) <- names(DATA_LORR_CLEAN)[1:34]
vif
str_formule
plot(model$fitted,rstudent(model),xlabel="fitted values", ylabel="standardozed residuals")
abline(h=2,col="red");
abline(h=-2,col="red");
anov<-aov(SOMME~Population+E+R+D+EPCI+C+VA,DATA_LORR_Class)
summary(anov)
plot(anov)
plot(rstudent(anov))
plot(anov$residuals)
acf(anov$residuals)

