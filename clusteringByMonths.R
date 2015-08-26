rm(list=ls());gc()
library(data.table)

library(ggplot2)
library(reshape2)
setwd("D:/Users/Benjamin/Documents/EJE-2211")
poker=fread("data/poker_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

ph=fread("data/ph_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
ps=fread("data/ps_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

library(grid)
library(factoextra)

###POKER
poker = poker[mois == "12",]
#attention : on enleve mise_max_semaine_max parce que donnees pourries !
cols_rem = c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
             "depots_max_semaine",
             "mises_max_semaine", #donnees aberrantes
             "mises_max_semaine_last",
             "depots_max_semaine_max",
             "mises_max_semaine_max",
             "mises_max_semaine_Chgt",
             "depots_max_semaine_Chgt"
#              "mises_max_semaine_Baisse",
#              "depots_max_semaine_Baisse", 
#              "mises_max_semaine_Hausse",
#              "depots_max_semaine_Hausse"
)
acpPoker=prcomp(poker[,!cols_rem
                      ,with=FALSE],
                scale=TRUE)
plot(acpPoker)
sexe <- fviz_pca_ind(acpPoker, geom = "point",
                     habillage=as.factor(poker[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

get_age = function(vect,breaks=c(39,59,74,inf))
{
  breaks = sort(breaks)
  
}
age <- fviz_pca_ind(acpPoker, geom = "point",
                    habillage=as.factor(cut(poker[,age_2014_12],breaks=c(0,25,35,55,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")



naxes = which.max(cumsum(acpPoker$sdev) > 0.80*sum(acpPoker$sdev))
inputHclust = predict(acpPoker,poker)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : poker par mois")
rect.hclust(resHclust, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

memb <- cutree(resHclust, k = 5)
# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpPoker, geom = "point",
                    habillage=as.factor(memb), addEllipses=F,
                    ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")



clust <- fviz_pca_ind(acpPoker,axes = c(1,3), geom = "point",
                    habillage=as.factor(memb), addEllipses=F,
                    ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

poker[,clust := memb]


for(i in 1:5)
{
  for(j in (i+1):6)
  {
    clust <- fviz_pca_ind(acpPoker,axes = c(i, j), geom = "point",
                          habillage=as.factor(memb), addEllipses=F,
                          ellipse.level= 0.95)+ theme_minimal()
    print(clust + scale_color_brewer(palette ="Set1"))
  }
}

#cluster 2
#depots_nombre ++
#nb_jours_exposes ++
#moderateurs ++
clust <- fviz_pca_ind(acpPoker,axes = c(2, 4), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")
fviz_pca_var(acpPoker, axes = c(2,4), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 2 & 4 (poker)")


#cluster 3
#-> joueur compulsif qui joue bcp (il a le max des mises, entre autre)

#cluster 4
clust <- fviz_pca_ind(acpPoker,axes = c(5, 6), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")


fviz_pca_var(acpPoker, axes = c(5, 6), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 5 & 6 (poker)")
#gros bonus
#gros chasing
#beaucoup de depot en nombre et en valeur

#cluster 5
clust <- fviz_pca_ind(acpPoker,axes = c(3, 5), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")
fviz_pca_var(acpPoker, axes = c(3, 5), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 3 & 5 (poker)")
#bcp nombre_autointerdiction_jc
#ratio_depot ++++
#ratio_cave ++++
#peu jours actifs
#peu multi-jeu


poker[,ratio_depot := depots_valeur/depots_nombre]
poker[,list(nb = .N), by = clust]
li_cols = colnames(poker)[! colnames(poker) %in% cols_rem]
poker[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]







