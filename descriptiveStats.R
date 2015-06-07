library(data.table)

table = fread("data/Costes_compte_x1x2_20150311Clean.csv", #le nom du document
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

var = "nombre_autointerdiction_ps"
vars = c("mises_max_heure_Chgt","mises_max_semaine_Chgt","depots_max_mois_Chgt",
         "nombre_autointerdiction_ps")
for(var in vars)
{
  cat(gsub("_"," ",var),"\n")
  titlePlot = paste(gsub("_"," ",var),"\n","(au moins un changement)")
  vect = unlist(table[,var,with = F],use.names = F)
  vect = vect[vect > 0]
  cat("au moins un changement : ",length(vect),"/",nrow(table),"\n")
  hist(vect,
       breaks = max(vect),
       main = titlePlot)
  cat("\n")
}

