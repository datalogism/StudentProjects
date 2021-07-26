
#Déclaration des tableaux Sans outliers
tab_sansVA_Mtt<-data.frame(TOUT2_SansVA_Montant, row.names=1)
t<-tab_sansVA_Mtt[1:19]           
write.table(tab_sansVA_Mtt,"clipboard",sep="\t", row.names=T)
tab_sansVANB<-data.frame(TOUT2_SansVA_Nb)
tt<-tab_sansVANB[1:19] 
tt
b<-data.frame(TOUT,row.names=1)
bb<-b[1:27]

#Matrices de correlation
qplot(x=Var1, y=Var2, data=melt(cor(t)), fill=value, geom="tile", na.rm=T) +
scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(t(t))), fill=value, geom="tile", na.rm=T) +
scale_fill_gradient2(limits=c(-1, 1))

qplot(x=Var1, y=Var2, data=melt(cor(tt)), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
qplot(x=Var1, y=Var2, data=melt(cor(t(tt))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))


tab_<-data.frame(ZOOM_SV_SsDomaine_Mtt_VA, row.names=1)
tab_
#Représentation de la répartition nombre/montant sans outliers
png("ZOOM_SV_VA_Nb_vocation.png", bg="transparent", width=1500, height=1500)
table.value(tab_/rowSums(tab_), grid=T, clegend=0)
dev.off()
png("Lien_Sans_VA_Montant_bycol.png", bg="transparent", width=1500, height=1500)
table.value(t/colSums(t), grid=T, clegend=0)
dev.off()
png("Lien_Sans_VA_Montant_byrow.png", bg="transparent", width=1500, height=1500)
table.value(t/rowSums(t), grid=T, clegend=0)
dev.off()

png("Lien_Sans_VA_Nb_Tot.png", bg="transparent", width=1500, height=1500)
table.value(tt, grid=T, clegend=0)
dev.off()
png("Lien_Sans_VA_Nb_bycol.png", bg="transparent", width=1500, height=1500)
table.value(tt/colSums(tt), grid=T, clegend=0)
dev.off()
png("Lien_Sans_VA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tt/rowSums(tt), grid=T, clegend=0)
dev.off()

######################## ZOOM SV

tab1<-data.frame(ZOOM_SV_SsDomaine_Mtt_SsVATRUE, row.names=1)
png("Lien_SsDom_SansVA_Mtt_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab1/rowSums(tab1), grid=T, clegend=0)
dev.off()
png("Corr_SsDom_SansVA_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab1.1<-data.frame(ZOOM_SV_SsDomaine_Nb_SsVATRUE, row.names=1)
png("Lien_SsDom_SansVA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab1.1/rowSums(tab1.1), grid=T, clegend=0)
dev.off()
png("Corr_SsDom_SansVA_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab1.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab2.1<-data.frame(ZOOM_SV_Vocation_Nb_SsVATRUE, row.names=1)
png("Lien_Voc_SansVA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab2.1/rowSums(tab2.1), grid=T, clegend=0)
dev.off()
png("Corr_Voc_SansVA_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab2.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab2<-data.frame(ZOOM_SV_Vocation_Mtt_SsVATRUE, row.names=1)
png("Lien_Voc_SansVA_Mtt_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab2/rowSums(tab2), grid=T, clegend=0)
dev.off()
png("Corr_Voc_SansVA_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab2))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab3<-data.frame(ZOOM_SV_SsDomaine_Mtt_VA, row.names=1)
png("Lien_SsDom_VA_Mtt_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab3/rowSums(tab3), grid=T, clegend=0)
dev.off()
png("Corr_SsDom_VA_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab3))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab3.1<-data.frame(ZOOM_SV_SsDomaine_Nb_VA, row.names=1)
png("Lien_SsDom_VA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab3.1/rowSums(tab3.1), grid=T, clegend=0)
dev.off()
png("Corr_SsDom_VA_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab3.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab4.1<-data.frame(ZOOM_SV_Vocation_Nb_VA, row.names=1)
png("Lien_Voc_VA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab4.1/rowSums(tab4.1), grid=T, clegend=0)
dev.off()
png("Corr_Voc_VA_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab4.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()


tab4<-data.frame(ZOOM_SV_Vocation_Mtt_VA[-6,], row.names=1)
png("Lien_Voc_VA_Mtt_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab4/rowSums(tab4), grid=T, clegend=0)
dev.off()
png("Corr_Voc_VA_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab4))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()


tab5<-data.frame(ZOOM_SV_Vocation_Mtt, row.names=1)
png("Corr_Voc_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab5))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()


tab5.1<-data.frame(ZOOM_SV_Vocation_Nb, row.names=1)
png("Corr_Voc_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab5.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()


tab6<-data.frame(ZOOM_SV_SsDomaine_Nb, row.names=1)
png("Corr_SsDomaine_Nb.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab6))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

tab6.1<-data.frame(ZOOM_SV_SsDomaine_Mtt, row.names=1)
png("Corr_SsDomaine_Mtt.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(tab6.1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()

Tab_TOUTVA_Montant[,-29]
tab7<-data.frame(Tab_TOUTVA_Montant[,-29], row.names=1)
png("Lien_Va_Mtt_byrow.png", bg="transparent", width=1500, height=1500)
table.value(tab7/rowSums(tab7), grid=T, clegend=0)
dev.off()


#Graphiques pour outliers : heatmap et table.value
png("HeatMap_VA_Montant_Var.png", bg="transparent", width=1500, height=1500)
T1<-data.frame(Tab_TOUTVA_Nb, row.names=1)
T2<-data.frame(Tab_TOUTVA_Montant, row.names=1)

png("HeatMap_VA_Montant_Var.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(T2)), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()
png("HeatMap_VA_Montant_Ind.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(T2))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()
png("Lien_VA_Montant_Tot.png", bg="transparent", width=1500, height=1500)
table.value(T2, grid=T,clegend=0)
dev.off()
png("Lien_VA_Montant_byrow.png", bg="transparent", width=1500, height=1500)
table.value(T2/rowSums(T2), grid=T,clegend=0)
dev.off()

png("HeatMap_VA_Nb_Var.png", bg="transparent", width=1500, height=1500)
T2.2<-data.frame(Tab_TOUTVA_Nb, row.names=1)
qplot(x=Var1, y=Var2, data=melt(cor(T1)), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()
png("HeatMap_VA_Nb_Ind.png", bg="transparent", width=1500, height=1500)
qplot(x=Var1, y=Var2, data=melt(cor(t(T1))), fill=value, geom="tile", na.rm=T) +
  scale_fill_gradient2(limits=c(-1, 1))
dev.off()
png("Lien_VA_Nb_Tot.png", bg="transparent", width=1500, height=1500)
table.value(T2.2, grid=T,clegend=0)
dev.off()
png("Lien_VA_Nb_byrow.png", bg="transparent", width=1500, height=1500)
table.value(T2.2/rowSums(T1), grid=T,clegend=0)
dev.off()

test <- data.frame(ZOOM_SV_corrige, row.names=1)


CA(test[1:23])


(as.data.frame(ZOOM_SV_corrige)[26] == "pratique en amateur")
