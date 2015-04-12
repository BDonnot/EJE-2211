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
      (colConcerned) := lapply(.SD,function(x) removeNull(removeComa(removeBraceComa(removeDoubleBrace(removeQuote(x)))))  ),
      .SDcols = colConcerned]


#on enleve les changements qui n'en sont pas vraiment : par exemple :
# "2012-10-13 02:33:04#100#100" : le chamgement etait de 100 à 100, donc pas de changement
dealOneDate = function(oneDate)
{
  temp = strsplit(oneDate,"#")
  res = sapply(temp,function(x) ifelse( length(x)==0 | x[2] == x[3] ,"",paste0(x,collapse = '#') ) )
  return(res)
}
agregateDates = function(x)
{
  temp = sapply(x,dealOneDate)
  paste0(temp[nchar(temp) > 0],collapse = ',' )
}
keepOnlyTrueChanges = function(x)
{
  temp = strsplit(x,",")
  sapply(temp,function(vectPossibleChange) agregateDates(vectPossibleChange) ) 
}
table[,
      (colConcerned) := lapply(.SD,keepOnlyTrueChanges),
      .SDcols = colConcerned]




str = c("2012-10-13 02:33:04#100#100","2012-10-13 02:33:04#100#101","","2014-07-06 23:45:58#1000#1000,2015-02-06 20:56:02#10001#10000")
temp = strsplit(str,",")
sapply(temp,function(x) agregateDates(x) ) 

a = table[,limites_retraits]
a[a != ""][4]
