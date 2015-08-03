rm(list=ls());gc()
library(data.table)
library(ggplot2)
library(reshape2)

setwd("C:/Users/aguillot/Documents/Projets Perso/EJE - 2211 - Bercy/Loto/Données ARJEL/")
#setwd("D:/Users/Benjamin/Documents/EJE-2211")
poker=fread("data/poker_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

ph=fread("data/ph_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
         sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !
ps=fread("data/ps_an.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
         sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

library(grid)
library(factoextra)

## Variables mois, depots_max et mises_max supprimees precedemment
acpPoker=prcomp(poker[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                          "depots_max_semaine","mises_max_semaine")
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
                    habillage=as.factor(cut(poker[,age_2014_12],breaks=c(0,25,40,59,74,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")


naxes = which.max(cumsum(acpPoker$sdev) > 0.80*sum(acpPoker$sdev))
inputHclust = predict(acpPoker,poker)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : poker par an")
rect.hclust(resHclust, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

memb <- cutree(resHclust, k = 5)
# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpPoker, geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

clust <- fviz_pca_ind(acpPoker,axes = c(2, 3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

clust <- fviz_pca_ind(acpPoker,axes = c(4,5), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

poker[,clust := memb]
setkey(poker,clust)
poker
cols = colnames(poker)[!(colnames(poker) %in% c("numero_joueur","numero_compte","mois","age_2014_12","civilite"))]
meansByClust = poker[,lapply(.SD,mean,na.rm = T),
                     .SDcols = cols,
                     by = key(poker)]

### Paris sportifs

acpps=prcomp(ps[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                          "depots_max_semaine","mises_max_semaine")
                      ,with=FALSE],
                scale=TRUE)
plot(acpps)
sexe <- fviz_pca_ind(acpps, ,geom = "point",
                     habillage=as.factor(ps[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

get_age = function(vect,breaks=c(39,59,74,inf))
{
  breaks = sort(breaks)
  
}
age <- fviz_pca_ind(acpps, geom = "point",
                    habillage=as.factor(cut(ps[,age_2014_12],breaks=c(0,25,40,59,74,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")


naxes = which.max(cumsum(acpps$sdev) > 0.80*sum(acpps$sdev))
inputHclust = predict(acpps,ps)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : ps par an")
rect.hclust(resHclust, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

memb <- cutree(resHclust, k = 5)
# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpps, geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

clust <- fviz_pca_ind(acpps,axes = c(4, 5), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")

clust <- fviz_pca_ind(acpps,axes = c(2,4), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")