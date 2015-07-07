rm(list=ls());gc()
setwd("C:/Users/aguillot/Documents/Projets Perso/EJE - 2211 - Bercy/Loto/EJE-2211/Donnees_ARJEL/")

library(data.table)
library(ggplot2)
library(xtable)

jour=fread("Costes_jour_x1x2_20150311.csv",sep=";",colClasses=c(  
  "integer",
  "integer",
  "NULL",
  "character",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric",
  "integer",
  "numeric"
  ))
mois=fread("Costes_mois_x1x2_20150311.csv",sep=";",header=TRUE,colClasses=c(
  "integer",
  "integer",
  "NULL",
  "character",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric"))
joueur=fread("Costes_joueur_x1x2_20150311.csv",sep=";",header=TRUE,colClasses=c(
  "integer",
  "integer",
  "numeric",
  "character",
  "character",
  "character"))
setnames(jour,paste("V",c(1,2,4:20),sep=""),c(
  "numero_joueur",
  "numero_compte",
  "jour",
  "paris_sportifs_nombre",
  "paris_sportifs_mises",
  "paris_hippiques_nombre",
  "paris_hippiques_mises",
  "caves_nombre",
  "caves_euros",
  "depots_nombre",
  "depots_valeur",
  "retrait_nombre",
  "retrait_valeur",
  "bonus_ps_nombre",
  "bonus_ps_montant",
  "bonus_ph_valeur",
  "bonus_ph_montant",
  "bonus_jc_valeur",
  "bonus_jc_montant"))

#Nouvelles variables
mois[,mois:=month(as.Date(mois))]
joueur[,civilite:=droplevels(factor(civilite,levels=c("M","M/MME","M/U","MME","U"),labels=c("M","M/MME","M","MME","M")))]
joueur[,last_jeu:=as.Date(last_jeu)]
joueur[,first_account:=as.Date(first_account)]
joueur[,base_1_an:=first_account<"2014-01-01"]
jour[,':='(jour=mday(as.Date(jour)),mois=month(as.Date(jour)))]

#Nb zeros

nrow(jour[paris_sportifs_nombre !=0 & paris_hippiques_nombre !=0 ,])/nrow(jour[paris_sportifs_nombre !=0 ])
nrow(jour[paris_sportifs_nombre !=0 & caves_nombre !=0 ,])/nrow(jour[caves_nombre !=0 ])

jour_nzero=jour[,lapply(.SD,function(x)sum(x==0))]/nrow(jour)
joueur[,lapply(.SD,function(x)sum(x==0))]/nrow(joueur)
mois_nzero=mois[,lapply(.SD,function(x)sum(x==0))]/nrow(mois)
jour_mois[,lapply(.SD,function(x)sum(x==0))]/nrow(jour_mois)

xtable(data.frame(t(jour_nzero),c(colnames(mois_nzero),NA,NA),rbind(t(mois_nzero),NA,NA)),digits=3)


#Correlations
cor_jour=cor(jour[,!c("numero_joueur","numero_compte"),with=FALSE])
cor_mois=cor(mois[,!c("numero_joueur","numero_compte"),with=FALSE])
ind_jour=which((abs(cor_jour)>=0.5)&(cor_jour<1),arr.ind=TRUE)
ind_mois=which((abs(cor_mois)>=0.5)&(cor_mois<1),arr.ind=TRUE)
#rownames(cor_jour)[ind_jour[,1]]
#colnames(cor_jour)[ind_jour[,2]]

library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor_jour), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ACP
library(ggplot2)
library(grid)
PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
  # PC being a prcomp object
  data <- data.frame(PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) # + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  plot <- plot + geom_hline(aes(0), size=.2) + geom_vline(aes(0), size=.2, color=colors[2])
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
  plot
}

acp=prcomp(mois[,!c("numero_joueur","numero_compte"),with=FALSE],scale=TRUE)
PCbiplot(acp,colors=c("black","black","black","dark blue"))

acp_jour=prcomp(jour[,!c("numero_joueur","numero_compte"),with=FALSE],scale=TRUE)


############################
# Division par type de jeu #
############################
setcolorder(jour,c(1:3,20,4:5,14:15,6:7,16:17,8:9,18:19,10:13))
setcolorder(mois,c(1:5,10:16,6:9,17:18))

setkeyv(jour,c("numero_joueur","numero_compte","mois"))
jour_mois=jour[,lapply(.SD,sum),by=key(jour)]

#Correlations
cor_jour_mois=cor(jour_mois[,!c("numero_joueur","numero_compte"),with=FALSE])
cor_mois=cor(mois[,!c("numero_joueur","numero_compte"),with=FALSE])
ind_jour_mois=which((abs(cor_jour_mois)>=0.5)&(cor_jour_mois<1),arr.ind=TRUE)
ind_mois=which((abs(cor_mois)>=0.5)&(cor_mois<1),arr.ind=TRUE)
#rownames(cor_jour)[ind_jour[,1]]
#colnames(cor_jour)[ind_jour[,2]]

library(ggplot2)
library(reshape2)
qplot(x=Var1, y=Var2, data=melt(cor_jour_mois), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
qplot(x=Var1, y=Var2, data=melt(cor_mois), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#Fusion
setkeyv(mois,c("numero_joueur","numero_compte","mois"))
fusion_mois=mois[jour_mois]

setcolorder(fusion_mois,c(1:3,19,4:12,20:23,13:16,24:35,17:18))
cor_fusion=cor(fusion_mois[,!c("numero_joueur","numero_compte","mois","jour"),with=FALSE])
qplot(x=Var1, y=Var2, data=melt(cor_fusion), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#Divison
fusion_sport=fusion_mois[,c(1:17,30:35),with=FALSE]
fusion_hipp=fusion_mois[,c(1:4,18:25,30:35),with=FALSE]
fusion_poker=fusion_mois[,c(1:4,26:35),with=FALSE]

rm(jour,mois,jour_mois); gc()

#Agregation compte
vars_sport=colnames(fusion_sport)[-which(colnames(fusion_sport)%in%c("ps_live_mises",
                                                                     "ps_foot_mises",
                                                                     "ps_tennis_mises",
                                                                     "ps_basket_mises",
                                                                     "ps_rugby_mises",
                                                                     "ps_autres_mises",
                                                                     "ps_autres_mises",
                                                                     "bonus_ps_nombre",
                                                                     "bonus_ps_montant"))]
vars_hipp=colnames(fusion_hipp)[-which(colnames(fusion_hipp)%in%c("bonus_ph_valeur","bonus_ph_montant"))]
vars_poker=colnames(fusion_poker)[-which(colnames(fusion_poker)%in%c("bonus_jc_valeur","bonus_jc_montant"))]

sport_melt=melt(fusion_sport[,c(lapply(.SD,sum)),
                             by=key(fusion_sport),
                             .SDcols=vars_sport],
           id.vars=key(fusion_sport))
hipp_melt=melt(fusion_hipp[,c(lapply(.SD,sum)),
                             by=key(fusion_hipp),
                             .SDcols=vars_hipp],
                id.vars=key(fusion_hipp))
poker_melt=melt(fusion_poker[,c(lapply(.SD,sum)),
                             by=key(fusion_poker),
                             .SDcols=vars_poker],
                id.vars=key(fusion_poker))

sport_dcast=data.table(dcast(data=sport_melt,formula=numero_joueur*numero_compte~variable*mois),
                  key=c("numero_joueur","numero_compte"))
hipp_dcast=data.table(dcast(data=hipp_melt,formula=numero_joueur*numero_compte~variable*mois),
                       key=c("numero_joueur","numero_compte"))
poker_dcast=data.table(dcast(data=poker_melt,formula=numero_joueur*numero_compte~variable*mois),
                       key=c("numero_joueur","numero_compte"))

setkeyv(fusion_sport,c("numero_joueur","numero_compte"))
setkeyv(fusion_hipp,c("numero_joueur","numero_compte"))
setkeyv(fusion_poker,c("numero_joueur","numero_compte"))

fusion_compte_sport=fusion_sport[,c(lapply(.SD,sum)),
                                 by=key(fusion_sport),
                                 .SDcols=vars_sport][sport_dcast]
fusion_compte_hipp=fusion_hipp[,c(lapply(.SD,sum)),
                               by=key(fusion_hipp),
                               .SDcols=vars_hipp][hipp_dcast]
fusion_compte_poker=fusion_poker[,c(lapply(.SD,sum)),
                                 by=key(fusion_poker),
                                 .SDcols=vars_poker][poker_dcast]

dim(fusion_compte_sport);dim(fusion_compte_hipp);dim(fusion_compte_poker)
