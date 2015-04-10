##on n'a besoin de ne le faire qu'une seule fois
##ceci rajoute des fonctions qu'on peut appeler
##dans ce cas particilier cela sert à charger les donnees plus rapidement
#install.packages("data.table")
##on charge le package :
library(data.table)

##on charge les donnees
table = fread("data/Costes_compte_x1x2_20150311.csv", #le nom du document
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
                             nombre_autointerdiction_jc="integer")
)
## * les champs * sont optionnels, mais les mettre permet d'aller plus vite lors du chargement,
##d'eviter a R de faire des erreurs (croire que la colonne est des character alors que c'est des
##entiers par exemple)
##et ca permet aussi de se rappeler de quel type sont les colonnes creees dans la suite du code :-)

removeNull = function(charVect)
{
  #cette fonction va retirer la chaine de caractere "NULL" quand elle va la voir
  return(gsub("NULL","",charVect))
}
colConcerned = c("mises_max_heure","mises_max_jour","mises_max_semaine","mises_max_mois",
                 "depots_max_heure","depots_max_jour","depots_max_semaine","depots_max_mois",
                 "limites_retraits")
table[,
      (colConcerned) := lapply(.SD,removeNull),
      .SDcols = colConcerned]

