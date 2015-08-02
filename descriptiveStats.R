library(data.table)

library(ggplot2)
library(reshape2)
setwd("D:/Users/Benjamin/Documents/EJE-2211/")
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

  																

var = "nombre_autointerdiction_ps"
vars = c("mises_max_heure_Chgt","mises_max_semaine_Chgt",
         "mises_max_jour_Chgt","mises_max_mois_Chgt",
         "depots_max_heure_Chgt","depots_max_semaine_Chgt",
         "depots_max_jour_Chgt","depots_max_mois_Chgt",
         "nombre_autointerdiction_ps","nombre_autointerdiction_ph",
         "nombre_autointerdiction_jc","limites_retraits_Chgt")
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
#on enleve les mises max heure Changement (donc aussi baisses et hausses)
#on enleve les mises max jour Changement (donc aussi baisses et hausses)
#on enleve les mises max mois Changement (donc aussi baisses et hausses)

table[,`:=`(mises_max_heure_Chgt= NULL,mises_max_heure_Hausse=NULL,
            mises_max_heure_last=NULL,mises_max_heure_max=NULL,
            mises_max_mois_Chgt= NULL,mises_max_mois_Hausse=NULL,
            mises_max_mois_last=NULL,mises_max_mois_max=NULL,
            mises_max_jour_Chgt= NULL,mises_max_jour_Hausse=NULL,
            mises_max_jour_last=NULL,mises_max_jour_max=NULL,
            depots_max_heure_Chgt= NULL,depots_max_heure_Hausse=NULL,
            depots_max_heure_last=NULL,depots_max_heure_max=NULL,
            depots_max_mois_Chgt= NULL,depots_max_mois_Hausse=NULL,
            depots_max_mois_last=NULL,depots_max_mois_max=NULL,
            depots_max_jour_Chgt= NULL,depots_max_jour_Hausse=NULL,
            depots_max_jour_last=NULL,depots_max_jour_max=NULL)]

initName = names(table)
table2 = copy(table)
setcolorder(table2,sort(initName))
table2 = copy(table2[,c("nombre_autointerdiction_ps",
                        "nombre_autointerdiction_ph",
                        "nombre_autointerdiction_jc",
                        "mises_max_semaine_Chgt",
                        "depots_max_semaine_Chgt",
                        "limites_retraits_Chgt",          
                        "mises_max_semaine_last",
                        "depots_max_semaine_last", 
                        "limites_retraits_last",
                        "mises_max_semaine_max",
                        "limites_retraits_max",
                        "mises_max_semaine_Baisse",
                        "depots_max_semaine_Baisse",
                        "limites_retraits_Baisse",
                        "mises_max_semaine_Hausse",
                        "depots_max_semaine_Hausse",
                        "limites_retraits_Hausse"),with = F])

qplot(x=Var1, y=Var2, data=melt(cor(table2, use="p")), fill=value, geom="tile") +
  scale_fill_gradient2(limits=c(-1, 1))
#on a un groupe de variables assez correlees : "limite retrait Chgt", 
#"depot max semaine Chgt" et "mises max semaines Chgt"

#pour les rese les variables ont l'air assez independantes,
#en meme temps il y a encore pas mal de 0 :-)
##On ecrit la table :
write.table(table, #on veut ecrire la table nommee 'table'
            "data/Costes_compte_x1x2_20150311Clean_epuree.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            row.names = F, #les lignes n'ont pas noms particulier, on ne rajoutera donc pas une colonne du type 'nom des lignes'
            col.names = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !