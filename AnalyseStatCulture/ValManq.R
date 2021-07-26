install.packages("")
pL=potentiel_fiscal_2008_Lorr
pP=potentiel_fiscal_2008_Poit
as.numeric(data.frame(pL))
(pot_Lorr[,5:18])
pL[,5]

#Lorraine 
L_res.pca<-PCA(pL[,c(5:6,8:13,15:18)])
L_nb<-estim_ncpPCA(pL[,c(5:6,8:13,15:18)], ncp.min=0, ncp.max=5)
L_com<-imputePCA(pL[,c(5:6,8:13,15:18)],ncp=L_nb$ncp,scale=T)
L_complet=cbind(pL[,1:4],L_com$completeObs)
write.csv2(L_complet,file = "Lorr_potentiel_fiscal_2008_recomp.csv")

L_res.pca<-PCA(L_com$completeObs)
png("Lorr_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(L_res.pca$eig[,2],type="l",main="Eboulli des valeurs propres")
dev.off()

NBAxe=0
for(i in 1:nrow(L_res.pca$eig)){
  if(L_res.pca$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
L_res.pca$ind$cos2

#Projections
png("PotLorr_Proj_Ind_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(1, 2),choix="ind")
dev.off()
png("PotLorr_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, axes=c(1, 2), choix="var",habillage="var")
dev.off()
png("PotLorr_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(1, 3), choix="ind")
dev.off()
png("PotLorr_Proj_Var_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, axes=c(1, 3), choix="var")
dev.off()
png("PotLorr_Proj_Ind_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(2, 3), choix="ind")
dev.off()
png("PotLorr_Proj_Var_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(L_res.pca, axes=c(2, 3), choix="var")
dev.off()



t=L_res.pca$var$cor
write.csv2(t,file = "PotLorr_ACP_corr.csv")

library(ggplot2)
library(reshape2)
#Heatmap : correlations
png("PotLorr_Corr_VariablesAC.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(L_res.pca$var$cor), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

#Contributions à l'inertie 
t=NULL
t=matrix(data = NA, nrow = nrow(L_res.pca$var$contrib), ncol = ncol(L_res.pca$var$contrib), byrow = FALSE,       dimnames = labels(L_res.pca$var$contrib))
for(j in 1:ncol(L_res.pca$var$contrib)){
  for(i in 1:nrow(L_res.pca$var$contrib)){
    if(L_res.pca$var$contrib[i,j]>mean(L_res.pca$var$contrib[,j])){
      if(L_res.pca$var$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "PotLorr_ACP_contrib_var.csv")
t=NULL
t=matrix(data = NA, nrow = nrow(L_res.pca$ind$contrib), ncol = ncol(L_res.pca$ind$contrib), byrow = FALSE,
         dimnames = labels(L_res.pca$ind$contrib))
for(j in 1:ncol(L_res.pca$ind$contrib)){
  for(i in 1:nrow(L_res.pca$ind$contrib)){
    if(L_res.pca$ind$contrib[i,j]>mean(L_res.pca$ind$contrib[,j])){
      if(L_res.pca$ind$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "PotLorr_ACP_contrib_ind.csv")

#Qualité
Quali=cbind(L_res.pca$var$cos2[,1]+L_res.pca$var$cos2[,2],
            L_res.pca$var$cos2[,1]+L_res.pca$var$cos2[,3],
            L_res.pca$var$cos2[,3]+L_res.pca$var$cos2[,2],
            L_res.pca$var$cos2[,1]+L_res.pca$var$cos2[,4],
            L_res.pca$var$cos2[,2]+L_res.pca$var$cos2[,4],
            L_res.pca$var$cos2[,3]+L_res.pca$var$cos2[,4]
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
write.csv2(t,file = "PotLorr_ACP_qualite_var.csv")

Quali=cbind(L_res.pca$ind$cos2[,1]+L_res.pca$ind$cos2[,2],
            L_res.pca$ind$cos2[,1]+L_res.pca$ind$cos2[,3],
            L_res.pca$ind$cos2[,3]+L_res.pca$ind$cos2[,2],
            L_res.pca$ind$cos2[,1]+L_res.pca$ind$cos2[,4],
            L_res.pca$ind$cos2[,2]+L_res.pca$ind$cos2[,4],
            L_res.pca$ind$cos2[,3]+L_res.pca$ind$cos2[,4]
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
write.csv2(t,file = "PotLorr_ACP_qualite_ind.csv")

#POITOU CHARENTE######################" 
P_res.pca<-PCA(pP[,c(5:6,8:13,15:18)])
P_nb<-estim_ncpPCA(pP[,c(5:6,8:13,15:18)], ncp.min=0, ncp.max=10,scale=T)
P_com<-imputePCA(pP[,c(5:6,8:13,15:18)],ncp=P_nb$ncp,scale=T)
P_complet=cbind(pP[,1:4],P_com$completeObs)
write.csv2(P_complet,file = "Poit_potentiel_fiscal_2008_recomp.csv")

P_res.pca<-PCA(L_com$completeObs)
png("Poit_ValPropres.png", bg="transparent", width=1500, height=1500)
plot(P_res.pca$eig[,2],type="l",main="Pot_Poit_Eboulli des valeurs propres")
dev.off()

NBAxe=0
for(i in 1:nrow(P_res.pca$eig)){
  if(P_res.pca$eig[i,1]>=1){    NBAxe=NBAxe+1    }
  
}
NBAxe
P_res.pca$ind$cos2

#Projections
png("PotPoit_Proj_Ind_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(1, 2),choix="ind")
dev.off()
png("PotPoit_Proj_Var_1_2.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, axes=c(1, 2), choix="var",habillage="var")
dev.off()
png("PotPoit_Proj_Ind_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(1, 3), choix="ind")
dev.off()
png("PotPoit_Proj_Var_1_3.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, axes=c(1, 3), choix="var")
dev.off()
png("PotPoit_Proj_Ind_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, label=c("var","quali"),select="cos2 0.7",axes=c(2, 3), choix="ind")
dev.off()
png("PotPoit_Proj_Var_2_3.png", bg="transparent", width=500, height=500)
plot.PCA(P_res.pca, axes=c(2, 3), choix="var")
dev.off()



t=P_res.pca$var$cor
write.csv2(t,file = "PotPoit_ACP_corr.csv")

library(ggplot2)
library(reshape2)
#Heatmap : correlations
png("PotPoit_Corr_VariablesAC.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(P_res.pca$var$cor), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

#Contributions à l'inertie 
t=NULL
t=matrix(data = NA, nrow = nrow(P_res.pca$var$contrib), ncol = ncol(P_res.pca$var$contrib), byrow = FALSE,       dimnames = labels(P_res.pca$var$contrib))
for(j in 1:ncol(P_res.pca$var$contrib)){
  for(i in 1:nrow(P_res.pca$var$contrib)){
    if(P_res.pca$var$contrib[i,j]>mean(P_res.pca$var$contrib[,j])){
      if(P_res.pca$var$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "PotPoit_ACP_contrib_var.csv")
t=NULL
t=matrix(data = NA, nrow = nrow(P_res.pca$ind$contrib), ncol = ncol(P_res.pca$ind$contrib), byrow = FALSE,
         dimnames = labels(P_res.pca$ind$contrib))
for(j in 1:ncol(P_res.pca$ind$contrib)){
  for(i in 1:nrow(P_res.pca$ind$contrib)){
    if(P_res.pca$ind$contrib[i,j]>mean(P_res.pca$ind$contrib[,j])){
      if(P_res.pca$ind$coord[i,j]>0){t[i,j]="+"
      }else{t[i,j]="-"
      }
    }
  }
}
write.csv2(t,file = "PotPoit_ACP_contrib_ind.csv")

#Qualité
Quali=cbind(P_res.pca$var$cos2[,1]+P_res.pca$var$cos2[,2],
            P_res.pca$var$cos2[,1]+P_res.pca$var$cos2[,3],
            P_res.pca$var$cos2[,3]+P_res.pca$var$cos2[,2],
            P_res.pca$var$cos2[,1]+P_res.pca$var$cos2[,4],
            P_res.pca$var$cos2[,2]+P_res.pca$var$cos2[,4],
            P_res.pca$var$cos2[,3]+P_res.pca$var$cos2[,4]
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
write.csv2(t,file = "PotPoit_ACP_qualite_var.csv")

Quali=cbind(P_res.pca$ind$cos2[,1]+P_res.pca$ind$cos2[,2],
            P_res.pca$ind$cos2[,1]+P_res.pca$ind$cos2[,3],
            P_res.pca$ind$cos2[,3]+P_res.pca$ind$cos2[,2],
            P_res.pca$ind$cos2[,1]+P_res.pca$ind$cos2[,4],
            P_res.pca$ind$cos2[,2]+P_res.pca$ind$cos2[,4],
            P_res.pca$ind$cos2[,3]+P_res.pca$ind$cos2[,4]
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
write.csv2(t,file = "PotPoit_ACP_qualite_ind.csv")
