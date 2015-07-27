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
                             nb_jours_actifs_poker="integer"
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

as.numeric(as.Date(mois_improved[1,last_jeu])-as.Date(mois_improved[1,first_account]))
mois_improved[,]

################
###Corerlation (players)

setkey(mois_improved,numero_joueur)
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
joueur_mois = mois_improved[,lapply(.SD,sum,na.rm = T),
                            by = key(mois_improved),
                            .SDcols = neworder]
cor_joueur=cor(joueur_mois[,!c("numero_joueur"),with=FALSE])
qplot(x=Var1, y=Var2, data=melt(cor_joueur[neworder,neworder]), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  xlab("")+ylab("")+ggtitle("Correlations (joueurs)")



