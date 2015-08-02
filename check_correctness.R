rm(list=ls());gc()
library(data.table)

library(ggplot2)
library(reshape2)
setwd("D:/Users/Benjamin/Documents/EJE-2211")

library(data.table)
############################
#chechk correctnes for months
months = fread("data/Costes_mois_x1x2_20150311.csv", #le nom du document
              header = T, #oui il y a une entete
              sep = ";", #le separateur *
              colClasses = c(numero_joueur="integer", #le type de toute les colonnes *
                             numero_compte="integer",
                             operateur="integer",
                             mois="character",
                             ps_mises="integer",
                             ps_gains="integer",
                             ph_mises="integer",
                             ph_gains="integer",
                             ph_simple_mises="integer",
                             ph_complexe_mises="integer",
                             ps_complexes_mises="integer",
                             ps_live_mises="integer",
                             ps_foot_mises="integer",
                             ps_tennis_mises="integer",
                             ps_basket_mises="integer",
                             ps_rugby_mises="integer",
                             ps_autres_mises="integer",
                             depots_3en12h="integer",
                             depots_1hapresmise="integer")
)
#there is indeed problems !
months[abs((ph_simple_mises+ph_complexe_mises)-ph_mises)>0.5,]
#5 rows does not match !
months[abs((ps_autres_mises+ps_rugby_mises+ps_tennis_mises+ps_foot_mises+ps_basket_mises)-ps_mises)>0.5,]
#4146 rows do not math !

hist(months[abs((ps_autres_mises+ps_rugby_mises+ps_tennis_mises+ps_foot_mises+ps_basket_mises)-ps_mises)>0.5,
       (ps_autres_mises+ps_rugby_mises+ps_tennis_mises+ps_foot_mises+ps_basket_mises)-ps_mises],
     main = "difference dans les paris sportifs (mois)",xlab = "difference",ylab = "frequence")

months[abs((ps_autres_mises+ps_rugby_mises+ps_tennis_mises+ps_foot_mises+ps_basket_mises)-ps_mises)>0.5 |
         abs((ph_simple_mises+ph_complexe_mises)-ph_mises)>0.5,]

nrow(months[abs((ps_autres_mises+ps_rugby_mises+ps_tennis_mises+ps_foot_mises+ps_basket_mises)-ps_mises)>0.5 |
              abs((ph_simple_mises+ph_complexe_mises)-ph_mises)>0.5,])/nrow(months)

############################
#chechk correctnes for days
days = fread("data/Costes_jour_x1x2_20150311.csv", #le nom du document
               header = F, #oui il y a une entete
               sep = ";", #le separateur *
)													
setnames(days,c("numero_joueur",
               "numero_compte",
               "operateur",
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
               "bonus_ph_nombre",
               "bonus_ph_montant",
               "bonus_jc_nombre",
               "bonus_jc_montant"))



#########################################
#chechk correctnes for months vs sum(days)

#agregate the days (sum)
days[,mois := paste0(gsub("-..$","",jour),"-01")]
days[1354854,mois]
setkeyv(days,c("numero_joueur",
               "numero_compte",
               "mois"))
monthsFromDays = days[,list(paris_sportifs_nombre = sum(paris_sportifs_nombre,na.rm = T),
                            paris_sportifs_mises = sum(paris_sportifs_mises,na.rm = T),
                            paris_hippiques_nombre = sum(paris_hippiques_nombre,na.rm = T),
                            paris_hippiques_mises = sum(paris_hippiques_mises,na.rm = T),
                            caves_nombre = sum(caves_nombre,na.rm = T),
                            caves_euros = sum(caves_euros,na.rm = T),
                            depots_nombre = sum(depots_nombre,na.rm = T),
                            depots_valeur = sum(depots_valeur,na.rm = T),
                            retrait_nombre = sum(retrait_nombre,na.rm = T),
                            retrait_valeur = sum(retrait_valeur,na.rm = T),
                            bonus_ps_nombre = sum(bonus_ps_nombre,na.rm = T),
                            bonus_ps_montant = sum(bonus_ps_montant,na.rm = T),
                            bonus_ph_nombre = sum(bonus_ph_nombre,na.rm = T),
                            bonus_ph_montant = sum(bonus_ph_montant,na.rm = T),
                            bonus_jc_nombre = sum(bonus_jc_nombre,na.rm = T),
                            bonus_jc_montant = sum(bonus_jc_montant,na.rm = T))
                      ,by = key(days)]

#comparing the values for some variables
setkeyv(months,c("numero_joueur",
                 "numero_compte",
                 "mois"))
setkeyv(monthsFromDays,c("numero_joueur",
                         "numero_compte",
                         "mois"))
bothMonth = cbind(months[,list(numero_joueur,
                               numero_compte,
                               mois,
                               ps_mises,
                               ph_mises)],
                  monthsFromDays[,list(paris_sportifs_mises,
                                        paris_hippiques_mises)]) 

bothMonth[abs(ps_mises-paris_sportifs_mises) > 0.5,]
#263 rows !
bothMonth[abs(ph_mises-paris_hippiques_mises) > 0.5,]
#55406 rows !
hist(bothMonth[abs(ph_mises-paris_hippiques_mises) > 0.5,ph_mises-paris_hippiques_mises],
     1000,
     main = "difference dans les paris hippique mises (jours vs mois)",
     xlab = "difference",ylab = "frequence",
     xlim = c(-1000,1000))
abline(v = c(mean(bothMonth[ph_mises> 0,ph_mises]),
             mean(bothMonth[paris_hippiques_mises> 0,paris_hippiques_mises]) ),
       col = c('blue','red'),
       lty = 2)
legend("topleft",
       legend = c("moyenne mensuelle","moyenne sum(jours)"),
       lty = 2,
       col = c("blue","red") )

bothMonth[abs(ph_mises-paris_hippiques_mises) > 0.5 |
            abs(ps_mises-paris_sportifs_mises) > 0.5, ]

nrow(bothMonth[abs(ph_mises-paris_hippiques_mises) > 0.5 |
            abs(ps_mises-paris_sportifs_mises) > 0.5, ])/nrow(bothMonth)