#Declarations tableaux

tab_VA_ArtPl<-data.frame(ARTS_PLASTIQUES_corrige_SUM, row.names=1)
tab_VA_AUTRES_DOMAINES<-data.frame(AUTRES_DOMAINES_corrige_SUM, row.names=1)
tab_VA_AUTRES_DOMAINES2<-data.frame(sansVA_A_D_1[1:148,],row.names=1)
tab_VA_CINEMA_AUDIOVISUEL<-data.frame(CINEMA_AUDIOVISUEL_corrige_SUM, row.names=1)
tab_VA_LIVRES_ET_LECTURE<-data.frame(LIVRES_ET_LECTURE_corrige_SUM, row.names=1)
tab_VA_MUSEES<-data.frame(MUSEES_corrige_SUM, row.names=1)
tab_VA_PATRIMOINE<-data.frame(PATRIMOINE_corrige_SUM, row.names=1)
tab_VA_PLURIDISCIPLINAIRE<-data.frame(PLURIDISCIPLINAIRE_corrige_SUM, row.names=1)
tab_VA_SPECTACLE.VIVANT<-data.frame(SPECTACLE.VIVANT_corrige_SUM, row.names=1)
tab_VA_SPECTACLE.VIVANT2<-data.frame(sansVA_SV_1[1:135,], row.names=1)
tab_VA_TRANSVERSAL<-data.frame(TRANSVERSAL_corrige_SUM, row.names=1)
tab_VA_TRANSVERSAL2<-data.frame(sansVA_T_1[1:277,], row.names=1)

#Trouver les valeurs atypiques
TrouveValAtypiq<-function(t){
tab=t
r=matrix()
for (i in 1:nrow(tab)){
    r[i]<-tab[i]
}

j=1
for (j in 1:length(r)){
   r[j]=sub(",",".",r[j])
}
r=as.numeric(r)
r
hist(r)
print(summary(r))
plot(r, main="Nuage de points des montants\n versés à chaque opérateur", ylab="Montant en euro")
boxplot(r, main="Boîte à moustaches \n de la répartition des financements ", horizontal=TRUE,xlab="Montant en euro")
Q1=as.numeric(quantile(r,0.25))
Q3=as.numeric(quantile(r,0.75))
Val.At.min=Q1-1.5*(Q3-Q1)
Val.At.max=Q3+1.5*(Q3-Q1)

indice=NULL
indice=array()
i=1
for (k in 1:length(r)){
  if(r[k]>Val.At.max |r[k]<Val.At.min){
    indice[i]=k
    i=i+1
    
  }
}
indice
i=1
val.atypiq=NULL
val.atypiq=array()
for(i in 1:length(indice)){
  val.atypiq[i]=rownames(tab)[indice[i]]
  i=i+1 
}
return (val.atypiq)
}

#Supprimer les Outliers
supprimeVA<-function(t,liste){
  i=1
  tf=NULL
  tf=t
  for(i in 1:length(liste)){
    indice=grep(as.character(liste[i]),as.character(row.names(tf)))
    print(row.names(tf[indice,]))
    print(nrow(tf))
    print(indice)
    tf<-tf[-indice,]
  } 
  
  return(tf)
}
a=data.frame(VA_SV_1, row.names=1)
a
b=data.frame(ZOOM_SV_corrige, row.names=1)
b
B=supprimeVA(b,a)

#####ArtPl

a1=TrouveValAtypiq(as.matrix(tab_VA_ArtPl[16]))
a1
                                                                                                                                                                                                                                                                                                        
length(TrouveValAtypiq(as.matrix(tab_VA_ArtPl[16])))/nrow(tab_VA_ArtPl)*100

write.csv2(a1,file = "VA_Art_P_V_1.csv")
A1=supprimeVA(tab_VA_ArtPl,a1)
write.csv2(A1,file = "sansVA_Art_Plastique_Et_Visuel.csv")
b1=TrouveValAtypiq(as.matrix(A1[16]))
write.csv2(b1,file = "VA_Art_P_V_2.csv")
b1

#######CINEMA_AUDIOVISUEL
a2=TrouveValAtypiq(as.matrix(tab_VA_CINEMA_AUDIOVISUEL[11]))
a2
mean(tab_VA_CINEMA_AUDIOVISUEL[11])
write.csv2(a2,file = "VA_CA_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_CINEMA_AUDIOVISUEL[11])))/nrow(tab_VA_CINEMA_AUDIOVISUEL)*100
A2=supprimeVA(tab_VA_CINEMA_AUDIOVISUEL,a2)
write.csv2(A2,file = "sansVA_CA1.csv")
b2=TrouveValAtypiq(as.matrix(A2[11]))
b2
B2=supprimeVA(A2,b2)
c2=TrouveValAtypiq(as.matrix(B2[11]))
c2
C2=supprimeVA(B2,c2)
d2=TrouveValAtypiq(as.matrix(C2[11]))
d2
D2=supprimeVA(C2,d2)
length(TrouveValAtypiq(as.matrix(C2[11])))/nrow(C2)*100
chisq.test(as.matrix(abs(D2[1:10])))
######## LIVRES_ET_LECTURE
a3=TrouveValAtypiq(as.matrix(tab_VA_LIVRES_ET_LECTURE[13]))
a3
write.csv2(a3,file = "VA_L_L_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_LIVRES_ET_LECTURE[13])))/nrow(tab_VA_LIVRES_ET_LECTURE)*100
A3=supprimeVA(tab_VA_LIVRES_ET_LECTURE,a3)
write.csv2(A3,file = "sansVA_L_L1.csv")
b3=TrouveValAtypiq(as.matrix(A3[13]))
b3
B3=supprimeVA(A3,b3)
c3=TrouveValAtypiq(as.matrix(B3[13]))
c3
C3=supprimeVA(B3,c3)
d3=TrouveValAtypiq(as.matrix(C3[13]))
length(TrouveValAtypiq(as.matrix(C3[13])))/nrow(C3)*100


########AUTRES DOMAINES
a4=TrouveValAtypiq(as.matrix(tab_VA_AUTRES_DOMAINES2[9]))
a4
write.csv2(a4,file = "VA_A_D_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_AUTRES_DOMAINES[9])))/nrow(tab_VA_AUTRES_DOMAINES)*100
A4=supprimeVA(tab_VA_AUTRES_DOMAINES,a4)
write.csv2(A4,file = "sansVA_A_D_1.csv")
b4=TrouveValAtypiq(as.matrix(sansVA_A_D_1[10]))
b4
B4=supprimeVA(A4,b4)
c4=TrouveValAtypiq(as.matrix(B4[9]))
c4
C4=supprimeVA(B4,c4)
d4=TrouveValAtypiq(as.matrix(C4[9]))
length(TrouveValAtypiq(as.matrix(C4[9])))/nrow(C4)*100

######## MUSEES
a5=TrouveValAtypiq(as.matrix(tab_VA_MUSEES[11]))
a5
write.csv2(a5,file = "VA_M_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_MUSEES[11])))/nrow(tab_VA_PATRIMOINE)*100
A5=supprimeVA(tab_VA_MUSEES,a5)
write.csv2(A5,file = "sansVA_M_1.csv")
b5=TrouveValAtypiq(as.matrix(A5[11]))
b5
B5=supprimeVA(A5,b5)
c5=TrouveValAtypiq(as.matrix(B5[11]))
c5
C5=supprimeVA(B5,c5)
d5=TrouveValAtypiq(as.matrix(C5[11]))
length(TrouveValAtypiq(as.matrix(C5[11])))/nrow(C5)*100

######## PATRIMOINE
a6=TrouveValAtypiq(as.matrix(tab_VA_PATRIMOINE[11]))
a6
write.csv2(a6,file = "VA_P_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_PATRIMOINE[11])))/nrow(tab_VA_PATRIMOINE)*100
A6=supprimeVA(tab_VA_PATRIMOINE,a6)
write.csv2(A6,file = "sansVA_P_1.csv")
b6=TrouveValAtypiq(as.matrix(A6[11]))
b6
B6=supprimeVA(A6,b6)
c6=TrouveValAtypiq(as.matrix(B6[11]))
c6
C6=supprimeVA(B6,c6)
d6=TrouveValAtypiq(as.matrix(C6[11]))
length(TrouveValAtypiq(as.matrix(C6[11])))/nrow(C6)*100
############### PLURIDISCIPLINAIRE
a7=TrouveValAtypiq(as.matrix(tab_VA_PLURIDISCIPLINAIRE[7]))
a7
write.csv2(a7,file = "VA_Pluri_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_PLURIDISCIPLINAIRE[7])))/nrow(tab_VA_PLURIDISCIPLINAIRE)*100
A7=supprimeVA(tab_VA_PLURIDISCIPLINAIRE,a7)
write.csv2(A6,file = "sansVA_Pluri_1.csv")
b7=TrouveValAtypiq(as.matrix(A7[7]))
b7
B7=supprimeVA(A7,b7)
c7=TrouveValAtypiq(as.matrix(B7[7]))
c7
C7=supprimeVA(B7,c7)
d7=TrouveValAtypiq(as.matrix(C7[7]))
length(TrouveValAtypiq(as.matrix(C7[7])))/nrow(C7)*100
D7=supprimeVA(C7,d7)
e7=TrouveValAtypiq(as.matrix(D7[7]))
E7=supprimeVA(D7,e7)
f7=TrouveValAtypiq(as.matrix(E7[7]))
############### Spectacle vivant
a8=TrouveValAtypiq(as.matrix(tab_VA_SPECTACLE.VIVANT2[24]))
a8
write.csv2(a8,file = "VA_SV_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_SPECTACLE.VIVANT[24])))/nrow(tab_VA_SPECTACLE.VIVANT)*100
A8=NULL
A8=supprimeVA(tab_VA_SPECTACLE.VIVANT,a8)
b8=TrouveValAtypiq(as.matrix(A8[24]))
b8
B8=supprimeVA(A8,b8)
c8=TrouveValAtypiq(as.matrix(B8[24]))
c8
C8=supprimeVA(B8,c8)
d8=TrouveValAtypiq(as.matrix(C8[24]))
length(TrouveValAtypiq(as.matrix(C8[24])))/nrow(C8)*100  
############### TRANSVERSAL
a9=TrouveValAtypiq(as.matrix(tab_VA_TRANSVERSAL2[22]))
a9
write.csv2(a9,file = "VA_T_1.csv")
length(TrouveValAtypiq(as.matrix(tab_VA_TRANSVERSAL[22])))/nrow(tab_VA_TRANSVERSAL)*100
nrow(tab_VA_TRANSVERSAL)
A9=supprimeVA(tab_VA_TRANSVERSAL,a9)
b9=TrouveValAtypiq(as.matrix(A8[22]))
b9
B9=supprimeVA(A9,b9)
c9=TrouveValAtypiq(as.matrix(B9[22]))
c9
C9=supprimeVA(B9,c9)
d9=TrouveValAtypiq(as.matrix(C9[22]))