rm(list=ls());gc()
setwd("C:/Users/aguillot/Documents/Projets Perso/EJE - 2211 - Bercy/Loto/Données ARJEL/")
setwd("C:/Users/antoine/Documents/Loto/Données ARJEL")
setwd("D:/Users/Benjamin/Documents/EJE-2211/")
library(data.table)
library(ggplot2)
library(xtable)

jour=fread("data/Costes_jour_x1x2_20150311.csv",sep=";",colClasses=c(  
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
mois_=fread("data/Costes_mois_x1x2_20150311.csv",sep=";",header=TRUE,colClasses=c(
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
joueur=fread("data/Costes_joueur_x1x2_20150311.csv",sep=";",header=TRUE,colClasses=c(
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

moderateurs = fread("data/Costes_compte_x1x2_20150311Clean.csv", #le nom du document
                            header = T, #oui il y a une entete
                            sep = ";", #le separateur *
                            colClasses = c(numero_joueur="integer", #le type de toute les colonnes *
                                           numero_compte="integer",
                                           date_ouverture="character",
                                           date_confirmation="character",
                                           jours_actif_12_mois="integer",
                                           mises_max_heure="character",
                                           mises_max_jour="character",
                                           mises_max_semaine="character",
                                           mises_max_mois="character",
                                           depots_max_heure="character",
                                           depots_max_jour="character",
                                           depots_max_semaine="character",
                                           depots_max_mois="character",
                                           limites_retraits="character",
                                           nombre_autointerdiction_ps="integer",
                                           nombre_autointerdiction_ph="integer",
                                           nombre_autointerdiction_jc="integer",
                                           mises_max_heure_Chgt="integer",
                                           mises_max_semaine_Chgt="integer",
                                           mises_max_mois_Chgt="integer",
                                           depots_max_heure_Chgt="integer",
                                           depots_max_jour_Chgt="integer",
                                           depots_max_semaine_Chgt="integer",
                                           depots_max_mois_Chgt="integer",
                                           limites_retraits_Chgt="integer",
                                           mises_max_heure_last="integer",
                                           mises_max_jour_last="integer",
                                           mises_max_semaine_last="integer",
                                           mises_max_mois_last="integer",
                                           depots_max_heure_last="integer",
                                           depots_max_jour_last="integer",
                                           depots_max_semaine_last="integer",
                                           depots_max_mois_last="integer",
                                           limites_retraits_last="integer",
                                           mises_max_heure_max="integer",
                                           mises_max_jour_max="integer",
                                           mises_max_semaine_max="integer",
                                           mises_max_mois_max="integer",
                                           depots_max_heure_max="integer",
                                           depots_max_jour_max="integer",
                                           depots_max_semaine_max="integer",
                                           depots_max_mois_max="integer",
                                           limites_retraits_max="integer",
                                           mises_max_heure_Baisse = "integer",
                                           mises_max_jour_Baisse="integer",
                                           mises_max_semaine_Baisse="integer",
                                           mises_max_mois_Baisse="integer",
                                           depots_max_heure_Baisse="integer",
                                           depots_max_jour_Baisse="integer",
                                           depots_max_semaine_Baisse="integer",
                                           depots_max_mois_Baisse="integer",
                                           limites_retraits_Baisse="integer",
                                           mises_max_heure_Hausse="integer",
                                           mises_max_jour_Hausse="integer",
                                           mises_max_semaine_Hausse="integer",
                                           mises_max_mois_Hausse="integer",
                                           depots_max_heure_Hausse="integer",
                                           depots_max_jour_Hausse="integer",
                                           depots_max_semaine_Hausse="integer",
                                           depots_max_mois_Hausse="integer",
                                           limites_retraits_Hausse="integer")
)
moderateurs = moderateurs[,list(numero_joueur,numero_compte,jours_actif_12_mois,
                                mises_max_semaine,depots_max_semaine,nombre_autointerdiction_ps,
                                nombre_autointerdiction_ph,nombre_autointerdiction_jc,
                                mises_max_semaine_Chgt,depots_max_semaine_Chgt,mises_max_semaine_last,
                                mises_max_semaine_max,depots_max_semaine_max,mises_max_semaine_Baisse,
                                depots_max_semaine_Baisse,mises_max_semaine_Hausse,depots_max_semaine_Hausse,
                                limites_retraits_Hausse)]
#########
#Nouvelles variables
joueur[,civilite:=droplevels(factor(civilite,levels=c("M","M/MME","M/U","MME","U"),labels=c("M","M/MME","M","MME","M")))]
joueur[,last_jeu:=as.Date(last_jeu)]
joueur[,first_account:=as.Date(first_account)]
joueur[,base_1_an:=first_account<"2014-01-01"]
jour[,':='(mois=gsub("(^2014-)|(-..$)","",jour),
           jour=gsub("^2014-..-","",jour) )]
mois_[,':='(mois=gsub("(^2014-)|(-..$)","",mois))]

########
#summing the data at month level from days level
setkey(jour,numero_joueur,numero_compte,mois)
colsSum = c("paris_sportifs_nombre",
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
            "bonus_jc_montant")
jour_mois0 = jour[,lapply(.SD,sum,na.rm = T),
                 .SDcols = colsSum,
                 by = key(jour)]
colsSum2 = c("paris_sportifs_nombre","paris_hippiques_nombre","caves_nombre")
jour_mois1 = jour[,lapply(.SD,function(x) length(x[x!=0.0])),
                 .SDcols = colsSum2,
                 by = key(jour)]
setnames(jour_mois1,c("numero_joueur","numero_compte","mois",
                      "nb_jours_actifs_ps","nb_jours_actifs_ph","nb_jours_actifs_poker"))
jour_mois = jour_mois0[jour_mois1]
###########
##adding (missing) pker variables to the month data base
setkey(mois_,numero_joueur,numero_compte,mois)
setkey(jour_mois,numero_joueur,numero_compte,mois)

mois = mois_[jour_mois[,list(numero_joueur,numero_compte,mois,
                             caves_nombre,caves_euros,bonus_jc_valeur,bonus_jc_montant,
                             retrait_nombre,retrait_valeur,depots_nombre,depots_valeur,
                             nb_jours_actifs_ps,nb_jours_actifs_ph,nb_jours_actifs_poker)]]
setkey(mois,numero_joueur,numero_compte)
setkey(moderateurs,numero_joueur,numero_compte)
mois = moderateurs[mois]

write.table(mois, #on veut ecrire la table nommee 'table'
            "data/Costes_mois_vrai.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
mois[is.na(caves_nombre),]
mois[is.na(ph_complexe_mises),]

###########
###Nb zeros
nrow(jour[paris_sportifs_nombre !=0 & paris_hippiques_nombre !=0 ,])/nrow(jour[paris_sportifs_nombre !=0 ])
nrow(jour[paris_sportifs_nombre !=0 & caves_nombre !=0 ,])/nrow(jour[caves_nombre !=0 ])

jour_nzero=jour[,lapply(.SD,function(x)sum(x==0))]/nrow(jour)
joueur[,lapply(.SD,function(x)sum(x==0))]/nrow(joueur)
mois_nzero=mois_[,lapply(.SD,function(x)sum(x==0))]/nrow(mois_)
jour_mois[,lapply(.SD,function(x)sum(x==0))]/nrow(jour_mois)

xtable(data.frame(t(jour_nzero),c(colnames(mois_nzero),NA,NA),rbind(t(mois_nzero),NA,NA)),digits=3)

##########
#Correlations (heat map)
cor_jour=cor(jour[,!c("numero_joueur","numero_compte","jour","mois"),with=FALSE])
cor_mois=cor(mois[,!c("numero_joueur","numero_compte","mois"),with=FALSE])
ind_jour=which((abs(cor_jour)>=0.5)&(cor_jour<1),arr.ind=TRUE)
ind_mois=which((abs(cor_mois)>=0.5)&(cor_mois<1),arr.ind=TRUE)
#rownames(cor_jour)[ind_jour[,1]]
#colnames(cor_jour)[ind_jour[,2]]


library(ggplot2)
library(reshape2)
colnames(cor_jour)
neworder = c("paris_sportifs_nombre",
             "paris_sportifs_mises",
             "bonus_ps_nombre",
             "bonus_ps_montant",
             "paris_hippiques_mises",
             "paris_hippiques_nombre",
             "bonus_ph_valeur",
             "bonus_ph_montant",
             "caves_nombre",
             "caves_euros",
             "bonus_jc_valeur",
             "bonus_jc_montant",
             "retrait_nombre",
             "retrait_valeur",
             "depots_nombre",
             "depots_valeur")
qplot(x=Var1, y=Var2, data=melt(cor_jour[neworder,neworder]), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("")+ylab("")+ggtitle("Correlations (jours)")

colnames(cor_mois)
neworder = c("nb_jours_actifs_ps",
             "ps_mises",
             "ps_gains",
             "ps_complexes_mises",
             "ps_live_mises",
             "ps_foot_mises",
             "ps_tennis_mises",
             "ps_basket_mises",
             "ps_rugby_mises",
             "ps_autres_mises",
             "ph_mises",
             "ph_gains",
             "ph_simple_mises",
             "ph_complexe_mises",
             "nb_jours_actifs_ph",
             "nb_jours_actifs_poker",
             "caves_nombre",
             "caves_euros",
             "bonus_jc_valeur",
             "bonus_jc_montant",
             "depots_nombre",
             "depots_valeur",
             "depots_3en12h",
             "depots_1hapresmise",
             "retrait_nombre",
             "retrait_valeur"
             )
qplot(x=Var1, y=Var2, data=melt(cor_mois[neworder,neworder]), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("")+ylab("")+ggtitle("Correlations (mois)")

########
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

acpMois=prcomp(mois[,!c("numero_joueur","numero_compte","mois"),with=FALSE],scale=TRUE)
# PCbiplot(acpMois,colors=c("black","black","black","dark blue"))

#fancy plots
# library("devtools")
# install_github("kassambara/factoextra")

fviz_pca_var(acpMois, axes = c(1, 2), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = 1) +
  scale_color_gradient2(low="blue", mid="purple",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (mois)")

fviz_pca_var(acpMois, axes = c(1, 3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = 1) +
  scale_color_gradient2(low="blue", mid="purple",
                        high="red") +
  ggtitle("ACP axes 1 & 3 (mois)")

fviz_pca_var(acpMois, axes = c(2, 3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = 1) +
  scale_color_gradient2(low="blue", mid="purple",
                        high="red") +
  ggtitle("ACP axes 2 & 3 (mois)")


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
