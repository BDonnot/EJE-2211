##on n'a besoin de ne le faire qu'une seule fois
##ceci rajoute des fonctions qu'on peut appeler
##dans ce cas particilier cela sert ? charger les donnees plus rapidement
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

colConcerned = c("mises_max_heure","mises_max_jour","mises_max_semaine","mises_max_mois",
                 "depots_max_heure","depots_max_jour","depots_max_semaine","depots_max_mois",
                 "limites_retraits")

removeNull = function(charVect)
{
  #cette fonction va retirer la chaine de caractere "NULL" quand elle va la voir
  return(gsub("NULL","",charVect))
}
removeComa = function(charVect)
{
  #cette fonction va retirer toutes les sequences de plusieurs virgules a la suite
  return(gsub(",+",",",charVect))  
}
removeBraceComa = function(charVect)
{
  #cette fonction va retirer toutes les sequences de la forme '{,' ou ',}'
  return(gsub(",*\\}","",gsub("\\{,*","",charVect)))  
}
removeDoubleBrace = function(charVect)
{
  #cette fonction va retirer toutes les sequences de la forme '{' ou '}'
  return(gsub("\\{","",gsub("\\}","",charVect)))
}
removeQuote = function(charVect)
{
  #cette fonction va retirer tous les guillemets
  return(gsub("\"","",charVect))
}
#on applique toutes les fonctions precedentes dans l'ordre :
table[,
      (colConcerned) := lapply(.SD,function(x) removeQuote(removeDoubleBrace(removeBraceComa(removeComa(removeNull(x)))))  ),
      .SDcols = colConcerned]


#on enleve les changements qui n'en sont pas vraiment : par exemple :
# "2012-10-13 02:33:04#100#100" : le chamgement etait de 100 à 100, donc pas de changement
# dealOneDate = function(oneDate)
# {
#   temp = strsplit(oneDate,"#")
#   res = sapply(temp,function(x) ifelse( length(x)==0 | x[2] == x[3] ,"",paste0(x,collapse = '#') ) )
#   return(res)
# }
# agregateDates = function(x)
# {
#   temp = sapply(x,dealOneDate)
#   paste0(temp[nchar(temp) > 0],collapse = ',' )
# }
# keepOnlyTrueChanges = function(x)
# {
#   temp = strsplit(x,",")
#   sapply(temp,function(vectPossibleChange) agregateDates(vectPossibleChange) ) 
# }
# table[,
#       (colConcerned) := lapply(.SD,keepOnlyTrueChanges),
#       .SDcols = colConcerned]

#nombre de changements
newCol = paste(colConcerned,"Chgt",sep = "_")
countNb = function(x)
{
  sapply(strsplit(x,","),length)
}

table[,
      (newCol) := lapply(.SD,countNb),
      .SDcols = colConcerned]
#nbs de changements a la hausse
newCol = paste(colConcerned,"Baisse",sep = "_")
countNb = function(x)
{
  sapply(strsplit(x,","),function(y) length(diff(y)) )
}
table[,
      (newCol) := lapply(.SD,countNb),
      .SDcols = colConcerned]

#nbs de changements a la baisse puis a la hausse
extractValue = function(x)
{
  sapply(x,function(y) ifelse(grepl("#",y),as.numeric(strsplit(y,'#')[[1]][2]),0))
}
countChanges = function(x,fun = countBaisse)
{
  temp=lapply(strsplit(x,","),extractValue)
  sapply(temp,fun)
}

countBaisse = function(x)
{
  temp = diff(x)
  length(temp[temp<0])
}
newCol = paste(colConcerned,"Baisse",sep = "_")
table[,
      (newCol) := lapply(.SD,countChanges,fun = countBaisse),
      .SDcols = colConcerned]
unique(table[,depots_max_mois_Baisse])
countHauuse= function(x)
{
  temp = diff(x)
  length(temp[temp>0])
}
newCol = paste(colConcerned,"Hausse",sep = "_")
table[,
      (newCol) := lapply(.SD,countChanges,fun = countHauuse),
      .SDcols = colConcerned]
unique(table[,depots_max_mois_Hausse])

##On ecrit la table :
write.table(table,
            "data/Costes_compte_x1x2_20150311Clean.csv",
            sep = ";",
            row.names = F,
            col.names = T)
