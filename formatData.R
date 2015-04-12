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

#########################
##Formatage des donnees##
#########################
#les donnees sont parfois mal formatees
#les quelques fonctions ci-dessous vont enlever les 'NULL' ou les '{' par exemple
#afin de faciliter les futurs traitements

#le vecteur ci-dessous contient toutes les colonnes sur lesquelles nous allons travailler :
colConcerned = c("mises_max_heure","mises_max_jour","mises_max_semaine","mises_max_mois",
                 "depots_max_heure","depots_max_jour","depots_max_semaine","depots_max_mois",
                 "limites_retraits")

removeNull = function(charVect)
{
  #cette fonction va retirer la chaine de caractere "NULL" 
  #quand elle va la voir dans chaque element de charVect
  return(gsub("NULL","",charVect))
}
removeComa = function(charVect)
{
  #cette fonction va retirer toutes les sequences de plusieurs virgules a la suite 
  #quand elle va la voir dans chaque element de charVect
  return(gsub(",+",",",charVect))  
}
removeBraceComa = function(charVect)
{
  #cette fonction va retirer toutes les sequences de la forme '{,' ou ',}' 
  #quand elle va la voir dans chaque element de charVect
  return(gsub(",*\\}","",gsub("\\{,*","",charVect)))  
}
removeDoubleBrace = function(charVect)
{
  #cette fonction va retirer toutes les sequences de la forme '{' ou '}'
  #quand elle va la voir dans chaque element de charVect
  return(gsub("\\{","",gsub("\\}","",charVect)))
}
removeQuote = function(charVect)
{
  #cette fonction va retirer tous les guillemets
  #quand elle va les voir dans chaque element de charVect
  return(gsub("\"","",charVect))
}

#on applique toutes les fonctions precedentes dans l'ordre au colonnes selectionnees
table[,
      (colConcerned) := lapply(.SD,function(x) removeQuote(removeDoubleBrace(removeBraceComa(removeComa(removeNull(x)))))  ),
      .SDcols = colConcerned]
#il y a plusieurs 'astuces' ici.
#La permier est l'emploi de .SDcols, qui permet de specifier les colonnes concernees
#La seconde est l'emploi de lapply(.SD,...) qui va permettre d'appliquer à chaque colonne la fonction '...'
##et de renvoyer la colonne ainsi modifiee
#La troisieme est l'emploi de '(colConcerned) :=' qui est la facon de dire a R de ne pas creer de
##nouvelles colonnes, mais de modifier les anciennces
#Pour plus d'informations, on pourra taper la commande ?data.table
#########################
##nombre de changements##
#########################

#le vecteur newcol va representer dans la suite les nouveaux noms des variables.
#Les nouveaux noms sont composes des anciens noms auxquels on a rajoute
#la chaine de caracteres "_Chgt". Par exemple on a transforme
#depots_max_semaine -> depots_max_semaine_Hausse_Chgt
newCol = paste(colConcerned,"Chgt",sep = "_")

countNb = function(x)
{
  #cette fonction compte le nombre de changements (a la hausse ou a la baisse)
  
  #sapply(vect/list,fun) va appliquer la fonction "fun" a chaque element
  #du vecteur 'vect' (ou de la liste 'list') et mettre tous les resultats
  #dans un unique vecteur
  #on pourra regarder l'aide via ?sapply
  
  #strsplit(charVect,char) va 'splitter' (ie decouper) chaque element du vecteur
  #de chaine de character 'charVect' en fonction de la chaine de character
  #'char' et les mettre tous dans un vecteur. Ces vecteurs seront ensuite
  #mis les uns au bout des autres dans une list.
  #on pourra consulter l'aider via ?strsplit
  sapply(strsplit(x,","),length)
}

#on applique la fonction precedentes aux variables selectionnees :
table[,
      (newCol) := lapply(.SD,countNb),
      .SDcols = colConcerned]
#On peut remarquer l'emploi de la meme syntaxe que precemment.
#.SDcols = colConcerned : les modifications concernent les memes colonnes

# lapply(.SD,countNb) va toujours appliquer la fonction countNb a chaque colonnes
# appelees par .SDcols (appelees via .SD) et renvoyer unes liste de colonnes

#on va ensuite donner a ces colonnes les noms voulus via la commande  '(newCol) :='
#il ne faut pas oublier les parentheses, sinon R va croire qu'il s'agit de la creation d'une seule
#colonne nomme 'newCol'


#nbs de changements a la baisse puis a la hausse
extractValue = function(x)
{
  #as.numeric va convertir la chaine de character en un nombre
  
  #grepl(char,stringVect) va renvoyer un vecteur res contenant autant d'elements
  #que stringVect. res contiendra 'True' ou 'False' selon que le character 'char'
  #est contenu dans la chaine testee.
  
  #ifelse(vectBool,valeurSiVrai,valeurSiFaux) va renvoyer un vecteur 'res' de meme longeur
  #que vectBool. Chaque element de 'res' correspondera a 'valeurSiVrai' ou 'valeurSiFaux'
  #selon que l'element vectBool est vrai ou faux
  sapply(x,function(y) ifelse(grepl("#",y),as.numeric(strsplit(y,'#')[[1]][2]),0) )
}
countChanges = function(x,fun = countBaisse)
{
  temp=lapply(strsplit(x,","),extractValue)
  sapply(temp,fun)
}

countBaisse = function(x)
{
  #diff(numericVect) va calculer la difference (au sens des series temporelles) du vecteur
  #de nombres 'numericVect'
  #par exemple
  #numericVect = c(1,2,4,7), 
  #diff(numericVect)
  #sera le vecteur c(1,2,3)
  temp = diff(x)
  length(temp[temp<0])
}

#on cree de nouveaux noms de colonnes en rajoutant "_Baisse"
#exactement de la meme facon qu'on l'avait fait precedemment
#avec "_Chgt"
newCol = paste(colConcerned,"Baisse",sep = "_")
#et creer les variables dont on a specifie le nom precedemment
table[,
      (newCol) := lapply(.SD,countChanges,fun = countBaisse),
      .SDcols = colConcerned]

#on fait la meme chose pour compter les 'hausses'
countHauuse= function(x)
{
  temp = diff(x)
  length(temp[temp>0])
}
newCol = paste(colConcerned,"Hausse",sep = "_")
table[,
      (newCol) := lapply(.SD,countChanges,fun = countHauuse),
      .SDcols = colConcerned]


##On ecrit la table :
write.table(table, #on veut ecrire la table nommee 'table'
            "data/Costes_compte_x1x2_20150311Clean.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

#il y a pas mal de fonctions dans ce petit script
#meme si j'ai essaye de les commenter, je pense que ce n'est pas toujours facile a comprendre
#les fonctions sur lesquelles on pourra s'attarder sont :
#apply,sapply,lapply
#fread : pour lire une table depuis le disque dur
#write.table : pour ecrire une table sur le disque dur

#les autres sont soit tres specifiques, soit un peu trop compliquees pour bien comprendre
#le code ici est assez optimise niveau temps de calcul et memoire utilisee.
#on pourrait faire des choses un peu plus facile a comprendre en faisant des boucles 'for' au lieu
#des choses de type "apply" "sapply" ou "lapply" par exemple.
