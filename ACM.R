#ACP

donnees_preparees <- donnees_preparees %>%
  rename(surfco_ = "taille_reco_rec",
         nb_salaries_ = "nb_salaries_rec")

d <- donnees_preparees[, c("etiq", "liseuse", "gpdiscu", "sitven", "portasso", "maillist", "obslib", "bibnum", "ressoc", "ressocspe", "logspec", "surfco_", "nb_salaries_", "ca_", "taux_retour", "pass_cais", "ca_semaine", "remise", "panier_moy", "age_stock", "taux_rotation", "compte_banq", "renta_m2", "fonds_roule")]
d <- mutate_all(d, as.factor)
d <- na.omit(d)

d2 <- donnees_preparees[, c("etiq", "liseuse", "gpdiscu", "sitven", "portasso", "maillist", "obslib", "bibnum", "ressoc", "ressocspe", "logspec", "surfco_", "nb_salaries_", "ca_", "taux_retour", "pass_cais", "ca_semaine", "remise", "panier_moy", "age_stock", "taux_rotation", "compte_banq", "renta_m2", "fonds_roule", "age_rev_rec")]
d2 <- mutate_all(d2, as.factor)
d2 <- d2 %>%
  filter(complete.cases(across(-age_rev_rec)))

d3 <- donnees_preparees[, c("etiq", "liseuse", "gpdiscu", "sitven", "portasso", "maillist", "obslib", "bibnum", "ressoc", "ressocspe", "logspec", "surfco_", "nb_salaries_", "ca_", "taux_retour", "pass_cais", "ca_semaine", "remise", "panier_moy", "age_stock", "taux_rotation", "compte_banq", "renta_m2", "fonds_roule", "diff_eco_rec")]
d3 <- mutate_all(d3, as.factor)
d3 <- d3 %>%
  filter(complete.cases(across(-diff_eco_rec)))

d4 <- d3 %>%
  select(-diff_eco_rec)

#avec FactoMineR
acm <- dudi.acm(df = d, scannf = FALSE, nf = 5)
explor::explor(acm)

fviz_eig(acm,
         barfill = "royalblue",
         barcolor = "black",
         linecolor = "red",
         title = "Histogramme des valeurs propres",
         ylab = "Pourcentage de la variance expliquée")

s.corcircle(acm$co, 1, 2, clabel = 0.5)

fviz_contrib(acm, 
             choice = "var", 
             axes = 1,
             fill = "royalblue",
             color = "black",
             title = "Histogramme des contributions à la dimension 1")
fviz_contrib(acm, 
             choice = "var", 
             axes = 2,
             fill = "royalblue",
             color = "black",
             title = "Histogramme des contributions à la dimension 2")
fviz_contrib(acm, 
             choice = "var", 
             axes = 3,
             fill = "royalblue",
             color = "black",
             title = "Histogramme des contributions à la dimension 3")

#AVEC GDAtools
acm2 <- speMCA(d)
modif.rate(acm2)$modif
#NUAGE DE POINTS sur l'axe 1 et 2
nuage_indiv <- ggcloud_indiv(acm2)
ggcloud_indiv(acm2, density="hex", hex.bin=10)

palette_custom <- c("#FF0000", "#FF0000",
                    "#FF8E00", "#FF8E00",
                    "#FFC600", "#FFC600",
                    "#FDEE00", "#FDEE00",
                    "#AA0000", "#AA0000",
                    "#AAFF00", "#AAFF00",
                    "#00FF71", "#00FF71",
                    "#8E00FF", "#8E00FF",
                    "#E0115F", "#E0115F",
                    "#FCDC12", "#FCDC12",
                    "#FFA07A", "#FFA07A",
                    "#EE1010", "#EE1010", "#EE1010",
                    "#00561B", "#00561B", "#00561B", "#00561B", "#00561B",
                    "#DB0073", "#DB0073", "#DB0073", "#DB0073",
                    "#F4A460", "#F4A460",
                    "#FF866A", "#FF866A",
                    "#FF1493", "#FF1493",
                    "#DE3163", "#DE3163",
                    "#649B88", "#649B88",
                    "#DAB30A", "#DAB30A",
                    "#79F8F8", "#79F8F8",
                    "#1034A6", "#1034A6",
                    "#73C2FB", "#73C2FB",
                    "#34C924", "#34C924")
nuage <- ggcloud_variables(acm2, shapes=FALSE, legend="none", col = palette_custom, col.by.group = TRUE)
#Projeter la variable supplémentaire sur le nuage de points
ggadd_supvar(nuage, acm2, d1$typo, col="seagreen", shape=NULL)
dimeta2(acm2, d1[,"typo"]) #--> Cette typo est très associée à l'axe 1

ggadd_supvar(nuage, acm2, d2$age_rev_rec, col="royalblue", shape=NULL)
dimeta2(acm2, d2[,"age_rev_rec"]) #--> Vraiment moyen avec l'âge

ggadd_supvar(nuage, acm2, axes = c(1, 2), d3$diff_eco_rec, col="magenta4", shape=NULL)
dimeta2(acm2, d3[,"diff_eco_rec"]) #--> Pareil, pas concluant
cordim1 <- dimdescr(acm2, d3[,"diff_eco_rec"])$dim.1$categories
cordim2 <- dimdescr(acm2, d3[,"diff_eco_rec"])$dim.2$categories

#Ellipses de correlation POUR TYPO
ggadd_kellipses(nuage_indiv, acm2, d1$typo, label=FALSE)
#Où les gens se situent dans l'ellipse
x <- ggadd_density(nuage_indiv, acm2, var=d1$typo, cat="1", density="area", ellipse=TRUE)
x <- ggadd_density(x, acm2, var=d1$typo, cat="2", density="area", ellipse=TRUE)
x <- ggadd_density(x, acm2, var=d1$typo, cat="3", density="area", ellipse=TRUE)
x +
  ggtitle("Densités des points des ellipses de concentration des classes de la CAH") +
  scale_color_manual(values = c("red", "seagreen", "blue"),
                    labels = c("1", "2", "3")) +
  theme(legend.position = "right")

#Test de typicalité
vseduc <- varsup(acm2, d1$typo)
vseduc$pval[,c(1,2)]

#NUAGE DE POINTS sur l'axe 1 et 3
nuage_indiv13 <- ggcloud_indiv(acm2, axes = c(1, 3))
ggcloud_indiv(acm2, axes = c(1, 3), density="hex", hex.bin=10)
nuage_13 <- ggcloud_variables(acm2, axes = c(1, 3), shapes = FALSE, legend = "none", col = palette_custom, col.by.group = TRUE)
#Projeter la variable supplémentaire sur le nuage de points
ggadd_supvar(nuage_13, acm2, d1$typo, col="seagreen", shape=NULL)
dimeta2(acm2, d1[,"typo"])
#Où les gens se situent dans l'ellipse
x1 <- ggadd_density(nuage_13, acm2, var=d1$typo, cat="1", density="area", ellipse=TRUE)
x1 <- ggadd_density(x, acm2, var=d1$typo, cat="2", density="area", ellipse=TRUE)
ggadd_density(x1, acm2, var=d1$typo, cat="3", density="area", ellipse=TRUE)