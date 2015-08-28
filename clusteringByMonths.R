rm(list=ls());gc()
library(data.table)

library(ggplot2)
library(reshape2)
setwd("D:/Users/Benjamin/Documents/EJE-2211")
library(grid)
library(factoextra)

######
###POKER
poker=fread("data/poker_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
            sep = ";", #specifie le separateur des colonnes
            header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !


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
poker[,clust := memb]
poker[,ratio_depot := depots_valeur/depots_nombre]
poker[,list(nb = .N), by = clust]
li_cols = colnames(poker)[! colnames(poker) %in% cols_rem]
poker[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]
# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpPoker, geom = "point",
                    habillage=as.factor(memb), addEllipses=F,
                    ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")



clust <- fviz_pca_ind(acpPoker,axes = c(1,3), geom = "point",
                    habillage=as.factor(memb), addEllipses=F,
                    ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")




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


########
###PARIS HIPPIQUES
ph=fread("data/ph_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
         sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !

ph = ph[mois == "12",]
ph = ph[ numero_joueur != 25654 | numero_compte != 1,] #probablement aberrant
ph = ph[numero_joueur != 25252 | numero_compte != 1,] #probablement aberrant
## ph = ph[numero_joueur != 23136 | numero_compte != 2,] #probablement aberrant
## ph = ph[numero_joueur != 19060 | numero_compte != 1,] #probablement aberrant
ph[,bonus_jc_valeur]

#attention : on enleve mise_max_semaine_max parce que donnees pourries !
cols_rem = c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
             "depots_max_semaine",
             "mises_max_semaine", #donnees aberrantes
             "mises_max_semaine_last",
             "depots_max_semaine_max",
             "mises_max_semaine_max",
             "mises_max_semaine_Chgt",
             "depots_max_semaine_Chgt",
             "bonus_jc_valeur"
             #              "mises_max_semaine_Baisse",
             #              "depots_max_semaine_Baisse", 
             #              "mises_max_semaine_Hausse",
             #              "depots_max_semaine_Hausse"
)
acpph=prcomp(ph[,!cols_rem
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
                    habillage=as.factor(cut(ph[,age_2014_12],breaks=c(0,25,35,55,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")



naxes = which.max(cumsum(acpph$sdev) > 0.80*sum(acpph$sdev))
inputHclust = predict(acpph,ph)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : ph par mois")
rect.hclust(resHclust, k = 5, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

memb <- cutree(resHclust, k = 5)
ph[,clust := memb]
ph[,ratio_depot := depots_valeur/depots_nombre]
ph[,list(nb = .N), by = clust]
li_cols = colnames(ph)[! colnames(ph) %in% cols_rem]
ph[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]


# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpph, geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")



clust <- fviz_pca_ind(acpph,axes = c(1,3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")




for(i in 1:5)
{
  for(j in (i+1):6)
  {
    clust <- fviz_pca_ind(acpph,axes = c(i, j), geom = "point",
                          habillage=as.factor(memb), addEllipses=F,
                          ellipse.level= 0.95)+ theme_minimal()
    print(clust + scale_color_brewer(palette ="Set1"))
  }
}

#cluster 2
# axe 2
fviz_pca_var(acpph, axes = c(1,2 ), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (paris hippiques)")
#-> moderateurs, enormement de changements


#cluster 3

#cluster 4
#-> bcp de mise
#bcp retrait
#bcp chasing
#depots_valeur ++
clust <- fviz_pca_ind(acpph,axes = c(3, 4), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
print(clust + scale_color_brewer(palette ="Set1"))
fviz_pca_var(acpph, axes = c(3,4), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 2 (paris hippiques)")

#cluster 5
# nombre_autointerdiction_ph



########
#PARIS SPORTIFS
ps=fread("data/ps_mois.csv", #dans le fichier 'Costes_compte_x1x2_20150311Clean.csv', situe dans le dossier "data/"
         sep = ";", #specifie le separateur des colonnes
         header = T) #on specifie par contre qu'il faut ecrire le nom des colonnes !


ps = ps[mois == "12",]
# ps = ps[ numero_joueur != 25654 | numero_compte != 1,] #probablement aberrant
# ps = ps[numero_joueur != 25252 | numero_compte != 1,] #probablement aberrant
## ps = ps[numero_joueur != 23136 | numero_compte != 2,] #probablement aberrant
## ps = ps[numero_joueur != 19060 | numero_compte != 1,] #probablement aberrant
# ps[,bonus_jc_valeur]

#attention : on enleve mise_max_semaine_max parce que donnees pourries !
cols_rem = c("numero_joueur","numero_compte","mois","age_2014_12","civilite",
             "depots_max_semaine",
             "mises_max_semaine", #donnees aberrantes
             "mises_max_semaine_last",
             "depots_max_semaine_max",
             "mises_max_semaine_max",
             "mises_max_semaine_Chgt",
             "depots_max_semaine_Chgt",
             #              "mises_max_semaine_Baisse",
             #              "depots_max_semaine_Baisse", 
             #              "mises_max_semaine_Hausse",
             #              "depots_max_semaine_Hausse"
)
acpps=prcomp(ps[,!cols_rem
                ,with=FALSE],
             scale=TRUE)
plot(acpps)
sexe <- fviz_pca_ind(acpps, geom = "point",
                     habillage=as.factor(ps[,civilite]), addEllipses=TRUE,
                     ellipse.level= 0.95)+ theme_minimal()
sexe + scale_color_brewer(palette ="Set1")

age <- fviz_pca_ind(acpps, geom = "point",
                    habillage=as.factor(cut(ps[,age_2014_12],breaks=c(0,25,35,55,150))),
                    addEllipses=TRUE,
                    ellipse.level= 0.95)+ theme_minimal()
age + scale_color_brewer(palette ="Set1")



naxes = which.max(cumsum(acpps$sdev) > 0.80*sum(acpps$sdev))
inputHclust = predict(acpps,ps)[,1:naxes]
resHclust = hclust(dist(inputHclust))
plot(resHclust,labels = FALSE,hang = -1,xlab = "",ylab = "inertie",
     main = "CAH : ps par mois")
rect.hclust(resHclust, k = 3, which = NULL, x = NULL, h = NULL,
            border = "blue", cluster = NULL)

memb <- cutree(resHclust, k = 5)
ps[,clust := memb]
ps[,ratio_depot := depots_valeur/depots_nombre]
ps[,list(nb = .N), by = clust]
li_cols = colnames(ps)[! colnames(ps) %in% cols_rem]
ps[,lapply(.SD,mean,na.rm = T), by = clust,.SDcols = li_cols]


# names(memb) = as.character(memb)
clust <- fviz_pca_ind(acpps, geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")



clust <- fviz_pca_ind(acpps,axes = c(1,3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
clust + scale_color_brewer(palette ="Set1")




for(i in 1:5)
{
  for(j in (i+1):6)
  {
    clust <- fviz_pca_ind(acpps,axes = c(i, j), geom = "point",
                          habillage=as.factor(memb), addEllipses=F,
                          ellipse.level= 0.95)+ theme_minimal()
    print(clust + scale_color_brewer(palette ="Set1"))
  }
}

#cluster 2
# axe 2
clust <- fviz_pca_ind(acpps,axes = c(3,5), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
print(clust + scale_color_brewer(palette ="Set1"))
fviz_pca_var(acpps, axes = c(3,5 ), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 3 & 5 (paris sportifs)")
#-> moderateurs, enormement de changements


#cluster 3
clust <- fviz_pca_ind(acpps,axes = c(1,3), geom = "point",
                      habillage=as.factor(memb), addEllipses=F,
                      ellipse.level= 0.95)+ theme_minimal()
print(clust + scale_color_brewer(palette ="Set1"))
fviz_pca_var(acpps, axes = c(1,3), geom = c("arrow", "text"),
             label = "var", invisible = "none", labelsize = 4,
             col.var = "x", alpha.var = "coord") +
  scale_color_gradient2(low="blue",mid="orange",
                        high="red") +
  ggtitle("ACP axes 1 & 3 (paris sportifs)")



#cluster 4 20618,1


#cluster 5 25252,1



