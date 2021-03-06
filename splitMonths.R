rm(list=ls());gc()
library(data.table)

library(ggplot2)
library(reshape2)
setwd("D:/Users/Benjamin/Documents/EJE-2211")
mois = fread("data/Costes_mois_vrai.csv", #le nom du document
              header = T, #oui il y a une entete
              sep = ";", #le separateur *
              colClasses = c(numero_joueur="character", #le type de toute les colonnes *
                             numero_compte="character",
                             mois = "character",
                             ps_mises ="integer",
                             ps_gains="integer",
                             ph_mises="integer",
                             ph_gains="integer",
                             ph_simple_mises="numeric",
                             ph_complexe_mises="numeric",
                             ps_complexes_mises="numeric",
                             ps_live_mises="numeric",
                             ps_foot_mises="numeric",
                             ps_tennis_mises="numeric",
                             ps_basket_mises="numeric",
                             ps_rugby_mises="numeric",
                             ps_autres_mises="numeric",
                             depots_3en12h="integer",
                             depots_1hapresmise="integer",
                             caves_nombre="integer",
                             caves_euros="numeric",
                             bonus_jc_valeur="integer",
                             bonus_jc_montant="numeric",
                             retrait_nombre="integer",
                             retrait_valeur="numeric",
                             depots_nombre="integer",
                             depots_valeur="numeric",
                             nb_jours_actifs_ps="integer",
                             nb_jours_actifs_ph="integer",
                             nb_jours_actifs_poker="integer",
                             jours_actif_12_mois="integer",
                             mises_max_semaine="numeric",
                             depots_max_semaine="numeric",
                             nombre_autointerdiction_ps="integer",
                             nombre_autointerdiction_ph="integer",
                             nombre_autointerdiction_jc="integer",
                             mises_max_semaine_Chgt="integer",
                             depots_max_semaine_Chgt="integer",
                             mises_max_semaine_last="numeric",
                             mises_max_semaine_max="numeric",
                             depots_max_semaine_max="numeric",
                             mises_max_semaine_Baisse="integer",
                             depots_max_semaine_Baisse="integer",
                             mises_max_semaine_Hausse="integer",
                             depots_max_semaine_Hausse="integer",
                             limites_retraits_Hausse="integer"
                               )
)

joueur=fread("data/Costes_joueur_x1x2_20150311.csv",
             sep=";",
             header=TRUE,
             colClasses=c(
               numero_joueur="character",
               nombre_de_comptes="character",
               age_2014_12="numeric",
               civilite="character",
               last_jeu="character",
               first_account="character"))

joueur[civilite=="M/U",civilite := "M"]
joueur[civilite=="M/MME",civilite := "U"]
unique(joueur[,civilite])
setkey(mois,numero_joueur)
setkey(joueur,numero_joueur)
mois_improved = joueur[mois]
mois_improved[,nb_jours_exposes := as.numeric(as.Date(last_jeu)-as.Date(first_account))]

mois_improved[,multi_jeu := 0]
mois_improved[nb_jours_actifs_ps > 0 & nb_jours_actifs_ph > 0|
                nb_jours_actifs_ps > 0 & nb_jours_actifs_poker > 0|
                nb_jours_actifs_ph > 0 & nb_jours_actifs_poker > 0
              ,multi_jeu := 1]
setkey(mois_improved,numero_joueur ,mois)
# mois_improved[,nb_mois_exposes := as.integer(nb_jours_actifs_ps+
#                                                nb_jours_actifs_ph+
#                                                nb_jours_actifs_poker > 0)
#               ,by = key(mois_improved)]

setkey(mois_improved,numero_joueur)
mois_improved[,multi_jeu := max(multi_jeu),by = key(mois_improved)]
# mois_improved[,nb_jours_exposes := max(nb_jours_exposes),by = key(mois_improved)]

mois_improved[multi_jeu > 0,nombre_de_comptes]
# mois_improved[nb_mois_exposes==14 & numero_joueur==12519,]
# mois_improved[,nb_mois_exposes]

str(mois_improved)
###############
###split DB
poker = mois_improved[nb_jours_actifs_poker> 0,
                      list(numero_joueur, #le type de toute les colonnes *
                           numero_compte,
                           mois,
                           age_2014_12,
                           civilite,
                           depots_nombre,
                           depots_valeur,
                           depots_3en12h,
                           depots_1hapresmise,
                           bonus_jc_valeur,
                           bonus_jc_montant,
                           retrait_nombre,
                           retrait_valeur,
                           nb_jours_exposes,
                           multi_jeu,
                           mises_max_semaine,
                           depots_max_semaine,
                           mises_max_semaine_Chgt,
                           depots_max_semaine_Chgt,
                           mises_max_semaine_last,
                           mises_max_semaine_max,
                           depots_max_semaine_max,
                           mises_max_semaine_Baisse,
                           depots_max_semaine_Baisse,
                           mises_max_semaine_Hausse,
                           depots_max_semaine_Hausse,
                           limites_retraits_Hausse,
                           nb_jours_actifs_poker,
                           caves_nombre,
                           caves_euros,
                           nombre_autointerdiction_jc
                           )]
ph = mois_improved[nb_jours_actifs_ph> 0,
                      list(numero_joueur, #le type de toute les colonnes *
                           numero_compte,
                           mois,
                           age_2014_12,
                           civilite,
                           depots_nombre,
                           depots_valeur,
                           depots_3en12h,
                           depots_1hapresmise,
                           bonus_jc_valeur,
                           bonus_jc_montant,
                           retrait_nombre,
                           retrait_valeur,
                           nb_jours_exposes,
                           multi_jeu,
                           mises_max_semaine,
                           depots_max_semaine,
                           mises_max_semaine_Chgt,
                           depots_max_semaine_Chgt,
                           mises_max_semaine_last,
                           mises_max_semaine_max,
                           depots_max_semaine_max,
                           mises_max_semaine_Baisse,
                           depots_max_semaine_Baisse,
                           mises_max_semaine_Hausse,
                           depots_max_semaine_Hausse,
                           limites_retraits_Hausse,
                           nb_jours_actifs_ph,
                           ph_mises,
                           ph_gains,
                           ph_simple_mises,
                           ph_complexe_mises,
                           nombre_autointerdiction_ph
                           )]
ps = mois_improved[nb_jours_actifs_ps> 0,
                      list(numero_joueur, #le type de toute les colonnes *
                           numero_compte,
                           mois,
                           age_2014_12,
                           civilite,
                           depots_nombre,
                           depots_valeur,
                           depots_3en12h,
                           depots_1hapresmise,
                           bonus_jc_valeur,
                           bonus_jc_montant,
                           retrait_nombre,
                           retrait_valeur,
                           nb_jours_exposes,
                           multi_jeu,
                           mises_max_semaine,
                           depots_max_semaine,
                           mises_max_semaine_Chgt,
                           depots_max_semaine_Chgt,
                           mises_max_semaine_last,
                           mises_max_semaine_max,
                           depots_max_semaine_max,
                           mises_max_semaine_Baisse,
                           depots_max_semaine_Baisse,
                           mises_max_semaine_Hausse,
                           depots_max_semaine_Hausse,
                           limites_retraits_Hausse,
                           nb_jours_actifs_ps,
                           ps_mises,
                           ps_gains,
                           ps_complexes_mises,
                           ps_live_mises,
                           ps_foot_mises,
                           ps_tennis_mises,
                           ps_basket_mises,
                           ps_rugby_mises,
                           ps_autres_mises,
                           nombre_autointerdiction_ps
                           )]

write.table(poker, #on veut ecrire la table nommee 'table'
            "data/poker_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
write.table(ph, #on veut ecrire la table nommee 'table'
            "data/ph_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
write.table(ps, #on veut ecrire la table nommee 'table'
            "data/ps_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

################
###Correlation (players)

setkey(mois_improved,numero_joueur)
neworder = c("nb_jours_actifs_ps",
             "nombre_autointerdiction_ps",
             "ps_mises",
             "ps_gains",
             "ps_complexes_mises",
             "ps_live_mises",
             "ps_foot_mises",
             "ps_tennis_mises",
             "ps_basket_mises",
             "ps_rugby_mises",
             "ps_autres_mises",
             "nombre_autointerdiction_ph",
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
             "nombre_autointerdiction_jc",
             "depots_max_semaine_Chgt",
             "depots_max_semaine_max",
             "depots_max_semaine_Baisse",
             "depots_max_semaine_Hausse",
             "depots_nombre",
             "depots_valeur",
             "depots_3en12h",
             "depots_1hapresmise",
             "retrait_nombre",
             "retrait_valeur",
             "limites_retraits_Hausse",
             "jours_actif_12_mois",
             "mises_max_semaine_Chgt",
             "mises_max_semaine_last",
             "mises_max_semaine_max",
             "mises_max_semaine_Baisse",
             "mises_max_semaine_Hausse")
joueur_mois = mois_improved[,lapply(.SD,sum,na.rm = T),
                            by = key(mois_improved),
                            .SDcols = neworder]
cor_joueur=cor(joueur_mois[,!c("numero_joueur"),with=FALSE])
qplot(x=Var1, y=Var2, data=melt(cor_joueur[neworder,neworder]), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("")+ylab("")+ggtitle("Correlations (joueurs)")


######
# Base 1 an
base_1_an=mois_improved[as.Date(first_account)<"2014-01-01",]

#Division par type de jeu (observation et variable)
poker_1_an = base_1_an[nb_jours_actifs_poker> 0,list(
                       numero_joueur, #le type de toute les colonnes *
                       numero_compte,
                       mois,
                       age_2014_12,
                       civilite,
                       depots_nombre,
                       depots_valeur,
                       depots_3en12h,
                       depots_1hapresmise,
                       bonus_jc_valeur,
                       bonus_jc_montant,
                       retrait_nombre,
                       retrait_valeur,
                       nb_jours_exposes,
                       multi_jeu,
                       mises_max_semaine,
                       depots_max_semaine,
                       mises_max_semaine_Chgt,
                       depots_max_semaine_Chgt,
                       mises_max_semaine_last,
                       mises_max_semaine_max,
                       depots_max_semaine_max,
                       mises_max_semaine_Baisse,
                       depots_max_semaine_Baisse,
                       mises_max_semaine_Hausse,
                       depots_max_semaine_Hausse,
                       limites_retraits_Hausse,
                       nb_jours_actifs_poker,
                       caves_nombre,
                       caves_euros,
                       nombre_autointerdiction_jc
                      )]
ph_1_an = base_1_an[nb_jours_actifs_ph> 0,
                   list(numero_joueur, #le type de toute les colonnes *
                        numero_compte,
                        mois,
                        age_2014_12,
                        civilite,
                        depots_nombre,
                        depots_valeur,
                        depots_3en12h,
                        depots_1hapresmise,
                        bonus_jc_valeur,
                        bonus_jc_montant,
                        retrait_nombre,
                        retrait_valeur,
                        nb_jours_exposes,
                        multi_jeu,
                        mises_max_semaine,
                        depots_max_semaine,
                        mises_max_semaine_Chgt,
                        depots_max_semaine_Chgt,
                        mises_max_semaine_last,
                        mises_max_semaine_max,
                        depots_max_semaine_max,
                        mises_max_semaine_Baisse,
                        depots_max_semaine_Baisse,
                        mises_max_semaine_Hausse,
                        depots_max_semaine_Hausse,
                        limites_retraits_Hausse,
                        nb_jours_actifs_ph,
                        ph_mises,
                        ph_gains,
                        ph_simple_mises,
                        ph_complexe_mises,
                        nombre_autointerdiction_ph
                   )]
ps_1_an = base_1_an[nb_jours_actifs_ps> 0,
                   list(numero_joueur, #le type de toute les colonnes *
                        numero_compte,
                        mois,
                        age_2014_12,
                        civilite,
                        depots_nombre,
                        depots_valeur,
                        depots_3en12h,
                        depots_1hapresmise,
                        bonus_jc_valeur,
                        bonus_jc_montant,
                        retrait_nombre,
                        retrait_valeur,
                        nb_jours_exposes,
                        multi_jeu,
                        mises_max_semaine,
                        depots_max_semaine,
                        mises_max_semaine_Chgt,
                        depots_max_semaine_Chgt,
                        mises_max_semaine_last,
                        mises_max_semaine_max,
                        depots_max_semaine_max,
                        mises_max_semaine_Baisse,
                        depots_max_semaine_Baisse,
                        mises_max_semaine_Hausse,
                        depots_max_semaine_Hausse,
                        limites_retraits_Hausse,
                        nb_jours_actifs_ps,
                        ps_mises,
                        ps_gains,
                        ps_complexes_mises,
                        ps_live_mises,
                        ps_foot_mises,
                        ps_tennis_mises,
                        ps_basket_mises,
                        ps_rugby_mises,
                        ps_autres_mises,
                        nombre_autointerdiction_ps
                   )]

#Keys
setkeyv(poker_1_an,c("numero_joueur","numero_compte"))
setkeyv(ph_1_an,c("numero_joueur","numero_compte"))
setkeyv(ps_1_an,c("numero_joueur","numero_compte"))

#Agregation (somme) des variables numeriques
poker_1_an_=poker_1_an[,c(lapply(.SD,sum)),
                                 by=key(poker_1_an),
                                 .SDcols=c(
                                 "depots_nombre",
                                 "depots_valeur",
                                 "depots_3en12h",
                                 "depots_1hapresmise",
                                 "bonus_jc_valeur",
                                 "bonus_jc_montant",
                                 "retrait_nombre",
                                 "retrait_valeur",
                                 "caves_nombre",
                                 "caves_euros",
                                 "nombre_autointerdiction_jc")
                                 ]

#Puis fusion avec les variables propres au joueur/compte
poker_1_an=poker_1_an[,list(
  numero_joueur, #le type de toute les colonnes *
  numero_compte,
  age_2014_12,
  civilite,
  nb_jours_actifs_poker,
  nb_jours_exposes,
  multi_jeu,
                      mises_max_semaine_Chgt,
                      depots_max_semaine_Chgt,
                      mises_max_semaine_last,
                      mises_max_semaine_max,
                      depots_max_semaine_max,
                      mises_max_semaine_Baisse,
                      depots_max_semaine_Baisse,
                      mises_max_semaine_Hausse,
                      depots_max_semaine_Hausse,
                      limites_retraits_Hausse)][poker_1_an_,mult="first"]



ph_1_an_=ph_1_an[,c(lapply(.SD,sum)),
                       by=key(ph_1_an),
                       .SDcols=c(
                         "ph_mises",
                         "ph_gains",
                         "ph_simple_mises",
                         "ph_complexe_mises",
                         "depots_nombre",
                         "depots_valeur",
                         "depots_3en12h",
                         "depots_1hapresmise",
                         "retrait_nombre",
                         "retrait_valeur",
                         "nombre_autointerdiction_ph")
                       ]
ph_1_an=ph_1_an[,list(
  numero_joueur, #le type de toute les colonnes *
  numero_compte,
  age_2014_12,
  civilite,
  nb_jours_actifs_ph,
  nb_jours_exposes,
  multi_jeu,
  mises_max_semaine_Chgt,
  depots_max_semaine_Chgt,
  mises_max_semaine_last,
  mises_max_semaine_max,
  depots_max_semaine_max,
  mises_max_semaine_Baisse,
  depots_max_semaine_Baisse,
  mises_max_semaine_Hausse,
  depots_max_semaine_Hausse,
  limites_retraits_Hausse)][ph_1_an_,mult="first"]

ps_1_an_=ps_1_an[,c(lapply(.SD,sum)),
                 by=key(ps_1_an),
                 .SDcols=c(
                   "ps_mises",
                   "ps_gains",
                   "ps_complexes_mises",
                   "ps_live_mises",
                   "ps_foot_mises",
                   "ps_tennis_mises",
                   "ps_basket_mises",
                   "ps_rugby_mises",
                   "ps_autres_mises",
                   "depots_nombre",
                   "depots_valeur",
                   "depots_3en12h",
                   "depots_1hapresmise",
                   "retrait_nombre",
                   "retrait_valeur",
                   "nombre_autointerdiction_ps")
                 ]
ps_1_an=ps_1_an[,list(
  numero_joueur, #le type de toute les colonnes *
  numero_compte,
  age_2014_12,
  civilite,
  nb_jours_actifs_ps,
  nb_jours_exposes,
  multi_jeu,
  mises_max_semaine_Chgt,
  depots_max_semaine_Chgt,
  mises_max_semaine_last,
  mises_max_semaine_max,
  depots_max_semaine_max,
  mises_max_semaine_Baisse,
  depots_max_semaine_Baisse,
  mises_max_semaine_Hausse,
  depots_max_semaine_Hausse,
  limites_retraits_Hausse)][ps_1_an_,mult="first"]

write.table(poker_1_an, #on veut ecrire la table nommee 'table'
            "data/poker_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
write.table(ph_1_an, #on veut ecrire la table nommee 'table'
            "data/ph_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
write.table(ps_1_an, #on veut ecrire la table nommee 'table'
            "data/ps_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !











###########################
## Melting pour demultiplier les variables par mois
sport_melt=melt(fusion_sport[,c(lapply(.SD,sum)),
                             by=key(ps_1_an),
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