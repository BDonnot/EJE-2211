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
cols_rem=c("numero_joueur","numero_compte","age_2014_12","civilite",
"mises_max_semaine_last",
"depots_max_semaine_max",
"mises_max_semaine_max",
"mises_max_semaine_Chgt",
"depots_max_semaine_Chgt")

acpPoker=prcomp(poker[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                          "depots_max_semaine","mises_max_semaine",
                          "mises_max_semaine_last",
                          "depots_max_semaine_max",
                          "mises_max_semaine_max",
                          "mises_max_semaine_Chgt",
                          "depots_max_semaine_Chgt")
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

#Retrouver les clusters
poker[,clust := memb]
setkey(poker,clust)
poker[,ratio_depot := depots_valeur/depots_nombre]
poker[,nb := .N, by = clust]
li_cols = colnames(poker)[! colnames(poker) %in% cols_rem]
poker[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]


clust <- fviz_pca_ind(acpPoker, geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 1 & 2 (poker)")

fviz_pca_var(acpPoker, axes = c(1,2), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (poker)")

clust <- fviz_pca_ind(acpPoker,axes = c(2, 3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 2 & 3 (poker)")

fviz_pca_var(acpPoker, axes = c(2,3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 2 & 3 (poker)")

clust <- fviz_pca_ind(acpPoker,axes = c(1,3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 1 & 3 (poker)")

fviz_pca_var(acpPoker, axes = c(1,3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 3 (poker)")

clust <- fviz_pca_ind(acpPoker,axes = c(3,4), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 3 & 4 (poker)")

fviz_pca_var(acpPoker, axes = c(3,4), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 3 & 4 (poker)")


### Paris sportifs ###
acpps=prcomp(ps[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                    "depots_max_semaine","mises_max_semaine",
                    "mises_max_semaine_last",
                    "depots_max_semaine_max",
                    "mises_max_semaine_max",
                    "mises_max_semaine_Chgt",
                    "depots_max_semaine_Chgt","clust")
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

#Retrouver les clusters
ps[,clust := memb]
setkey(ps,clust)
ps[,ratio_depot := depots_valeur/depots_nombre]
ps[,.N, by = clust]
li_cols = colnames(ps)[! colnames(ps) %in% cols_rem]
ps[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]

### Suppression des 4 observations isolees (clust 2 a 5) et reiteration ###
ps2=ps[clust==1,]
acpps2=prcomp(ps2[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                    "depots_max_semaine","mises_max_semaine",
                    "mises_max_semaine_last",
                    "depots_max_semaine_max",
                    "mises_max_semaine_max",
                    "mises_max_semaine_Chgt",
                    "depots_max_semaine_Chgt","clust","ratio_depot")
                ,with=FALSE],
             scale=TRUE)
naxes2 = which.max(cumsum(acpps2$sdev) > 0.80*sum(acpps2$sdev))
inputHclust2 = predict(acpps2,ps2)[,1:naxes2]
resHclust2 = hclust(dist(inputHclust2))
plot(resHclust2,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : ps par an")
rect.hclust(resHclust2, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

sexe <- fviz_pca_ind(acpps2, ,geom = "point",
                     habillage=as.factor(ps2[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

age <- fviz_pca_ind(acpps2, geom = "point",
                    habillage=as.factor(cut(ps2[,age_2014_12],breaks=c(0,25,40,59,74,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")

memb2 <- cutree(resHclust2, k = 5)
ps2[,clust2 := memb2]
ps2[,.N, by = clust2]
ps2[,lapply(.SD,mean,na.rm = T), by = clust2,.SDcols = li_cols]

clust <- fviz_pca_ind(acpps2, geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 1 & 2 (paris sportifs)")

fviz_pca_var(acpps2, axes = c(1,2), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (paris sportifs)")

clust <- fviz_pca_ind(acpps2, geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 1 & 2 (paris sportifs)")

fviz_pca_var(acpps2, axes = c(1,2), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (paris sportifs)")


clust <- fviz_pca_ind(acpps2,axes = c(2,3), geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 2 & 3 (paris sportifs)")

fviz_pca_var(acpps2, axes = c(2,3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 2 & 3 (paris sportifs)")


clust <- fviz_pca_ind(acpps2,axes = c(3,4), geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 3 & 4 (paris sportifs)")

fviz_pca_var(acpps2, axes = c(3,4), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 3 & 4 (paris sportifs)")




### Paris hippiques ###
acpph=prcomp(ph[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                          "depots_max_semaine","mises_max_semaine",
                          "mises_max_semaine_last",
                          "depots_max_semaine_max",
                          "mises_max_semaine_max",
                          "mises_max_semaine_Chgt",
                          "depots_max_semaine_Chgt")
                      ,with=FALSE],
                scale=TRUE)
plot(acpph)
sexe <- fviz_pca_ind(acpph, geom = "point",
                     habillage=as.factor(ph[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

get_age = function(vect,breaks=c(39,59,74,inf))
{
  breaks = sort(breaks)
  
}
age <- fviz_pca_ind(acpph, geom = "point",
                    habillage=as.factor(cut(ph[,age_2014_12],breaks=c(0,25,40,59,74,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")


naxes = which.max(cumsum(acpph$sdev) > 0.80*sum(acpph$sdev))
inputHclust = predict(acpph,ph)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : poker par an")
rect.hclust(resHclust, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)
memb <- cutree(resHclust, k = 5)

#Retrouver les clusters
ph[,clust:=memb]
setkey(ph,clust)
ph[,ratio_depot := depots_valeur/depots_nombre]
ph[, .N, by = clust]
li_cols = colnames(ph)[! colnames(ph) %in% cols_rem]
ph[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]

### Omission de 3 observations extremes
ph2=ph[clust%in%c(1,2),]
acpph2=prcomp(ph2[,!c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
                    "depots_max_semaine","mises_max_semaine",
                    "mises_max_semaine_last",
                    "depots_max_semaine_max",
                    "mises_max_semaine_max",
                    "mises_max_semaine_Chgt",
                    "depots_max_semaine_Chgt","nb","clust","ratio_depot")
                ,with=FALSE],
             scale=TRUE)
plot(acpph2)
sexe <- fviz_pca_ind(acpph2, geom = "point",
                     habillage=as.factor(ph2[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

age <- fviz_pca_ind(acpph2, geom = "point",
                    habillage=as.factor(cut(ph2[,age_2014_12],breaks=c(0,25,40,59,74,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")


naxes2 = which.max(cumsum(acpph2$sdev) > 0.80*sum(acpph2$sdev))
inputHclust2 = predict(acpph2,ph2)[,1:naxes2]
resHclust2 = hclust(dist(inputHclust2))
plot(resHclust2,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : paris hippiques par an")
rect.hclust(resHclust2, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)
memb2 <- cutree(resHclust2, k = 5)

#Retrouver les clusters
ph2[,clust2 := memb2]
setkey(ph2,clust2)
ph2[,ratio_depot := depots_valeur/depots_nombre]
ph2[, .N, by = clust2]
li_cols = colnames(ph2)[! colnames(ph2) %in% cols_rem]
ph2[,lapply(.SD,mean,na.rm = T), by = clust2,.SDcols = li_cols]


clust <- fviz_pca_ind(acpph2, geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 1 & 2 (paris hippiques)")

fviz_pca_var(acpph2, axes = c(1,2), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (paris hippiques)")


clust <- fviz_pca_ind(acpph2, axes=c(2,4),geom = "point",
                      habillage=as.factor(memb2), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")+
  ggtitle("ACP axes 2 & 4 (paris hippiques)")

fviz_pca_var(acpph2, axes = c(2,4), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 2 & 4 (paris hippiques)")

