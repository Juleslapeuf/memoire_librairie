library(ade4)
library(ggplot2)
library(ggrepel)
library(cluster)
library(TraMineR)
library(WeightedCluster)
library(GGally)

# ==================================================
# 0. JEU DE DONNEES
# ==================================================
d <- donnees_preparees %>%
  select(
    etiq, liseuse, gpdiscu, sitven, portasso,
    maillist, obslib, bibnum, ressoc, ressocspe,
    logspec, surfco_, nb_salaries_, ca_rec_rec,
    taux_retour, pass_cais, ca_semaine, remise,
    panier_moy, age_stock, taux_rotation,
    compte_banq, renta_m2, fonds_roule
  ) %>%
  mutate(across(-c(surfco_, nb_salaries_, ca_rec_rec), as.factor)) %>%
  mutate(
    surfco_ = ordered(surfco_, levels = c(
      "Moins de 100 m2",
      "Entre 100 et 199 m2",
      "Plus de 200 m2"
    )),
    nb_salaries_ = ordered(nb_salaries_, levels = c(
      "Aucun salarié",
      "1 à 2 salariés",
      "3 à 4 salariés",
      "5 à 9 salariés",
      "10 et plus"
    )),
    ca_rec_rec = ordered(ca_rec_rec, levels = c(
      "Inférieur à 300k",
      "300k à 599k",
      "600k à 1199k",
      "1,2 M€ et plus"
    ))
  ) %>%
  na.omit()

# ==================================================
# 1. ANALYSE DES CORRESPONDANCES MULTIPLES (ACM)
# ==================================================
#avec ade4

acm <- dudi.acm(df = d, scannf = FALSE, nf = 5)
#explor::explor(acm)

# Histogramme des valeurs propres
fviz_eig(acm,
         barfill = "royalblue",
         barcolor = "black",
         linecolor = "red",
         title = "Histogramme des valeurs propres",
         ylab = "Pourcentage de la variance expliquée")

# Cercle de corrélations
s.corcircle(acm$co, 1, 2, clabel = 0.5)

# Contributions des variables aux axes
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

modalites <- as.data.frame(acm$co)
modalites$modalite <- rownames(modalites)

# Projection des variables

ggplot(modalites, aes(x = Comp1, y = Comp2, label = modalite)) +
  geom_point(size = 1.5) +
  geom_text_repel(
    size = 2.5,
    box.padding = 0.3,
    point.padding = 0.2,
    max.overlaps = Inf
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Nuage des modalités – ACM - Dimensions 1 et 2",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal()

ggplot(modalites, aes(x = Comp1, y = Comp3, label = modalite)) +
  geom_point(size = 1.5) +
  geom_text_repel(
    size = 2.5,
    box.padding = 0.3,
    point.padding = 0.2,
    max.overlaps = Inf
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Nuage des modalités – ACM - Dimensions 1 et 3",
    x = "Dimension 1",
    y = "Dimension 3"
  ) +
  theme_minimal()

individus <- as.data.frame(acm$li)
individus$individu <- rownames(individus)

# Projection des individus

ggplot(individus, aes(x = Axis1, y = Axis2)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Nuage des individus – ACM - Dimensions 1 et 2",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal()

ggplot(individus, aes(x = Axis1, y = Axis3)) +
  geom_point(size = 1.5, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Nuage des individus – ACM - Dimensions 1 et 3",
    x = "Dimension 1",
    y = "Dimension 2"
  ) +
  theme_minimal()

# ==================================================
# 2. CLASSIFICATION ASCENDANTE HIERARCHIQUE (CAH)
# ==================================================

#########
## METHODE : PHI2
#########

distance_phi2 <- ade4::dist.dudi(acm)

arbre_phi2 <- hclust(
  distance_phi2,
  method = "ward.D2"
)

fviz_dend(
  arbre_phi2,
  k = 2,
  rect = TRUE,
  show_labels = FALSE
) +
  ggtitle("CAH – Distance du Phi²")

inertie_phi2 <- sort(arbre_phi2$height, decreasing = TRUE)
plot(inertie_phi2[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 5), inertie_phi2[c(2, 3, 5)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

wardRange_phi2 <- as.clustrange(arbre_phi2, diss = distance_phi2, ncluster = 15)
plot(wardRange_phi2, 
     stat = c('ASW','CH','HC','HG','PBC'), 
     norm = "zscore", 
     main = "Indicateurs de découpage des classes du dendrogramme pour phi2")

#########
## METHODE : GOWER
#########

distance_gower <- daisy(
  d,
  metric = "gower"
)

arbre_gower <- hclust(
  as.dist(distance_gower),
  method = "average"
)

fviz_dend(
  arbre_gower,
  k = 2,
  rect = FALSE,
  show_labels = FALSE
) +
  ggtitle("CAH – Distance de Gower")

inertie_gower <- sort(arbre_gower$height, decreasing = TRUE)
plot(inertie_gower[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 5), inertie_gower[c(2, 3, 5)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

wardRange_gower <- as.clustrange(arbre_gower, diss = distance_gower, ncluster = 15)
plot(wardRange_gower, 
     stat = c('ASW','CH','HC','HG','PBC'), 
     norm = "zscore", 
     main = "Indicateurs de découpage des classes du dendrogramme pour Gower")

#########
## METHODE : EUCLIDEAN
#########

# Il faut utiliser des variables quantitatives, donc on reprend les variables avant catégorisation
d_euclidean <- donnees_preparees %>%
  select(
    etiq, liseuse, gpdiscu, sitven, portasso,
    maillist, obslib, bibnum, ressoc, ressocspe,
    logspec, taille_reco, nb_salaries, ca_rec,
    taux_retour, pass_cais, ca_semaine, remise,
    panier_moy, age_stock, taux_rotation,
    compte_banq, renta_m2, fonds_roule
  ) %>%
  mutate(
    across(
      c(etiq, liseuse, gpdiscu, sitven, portasso,
        maillist, obslib, bibnum, ressoc, ressocspe,
        logspec, taux_retour, pass_cais, ca_semaine,
        remise, panier_moy, age_stock, taux_rotation,
        compte_banq, renta_m2, fonds_roule),
      ~ as.numeric(as.character(.))
    ),
    across(
      c(taille_reco, nb_salaries, ca_rec),
      as.numeric
    )
  ) %>%
  na.omit()

# Et on standardise
d_standardise <- as.data.frame(scale(d_euclidean))

distance_euclidean <- dist(
  d_standardise,
  method = "euclidean"
)

arbre_euclidean <- hclust(
  distance_euclidean,
  method = "ward.D2"
)

fviz_dend(
  arbre_euclidean,
  k = 3,
  rect = FALSE,
  show_labels = FALSE
) +
  ggtitle("CAH – Distance Euclidienne")

inertie_euclidean <- sort(arbre_euclidean$height, decreasing = TRUE)
plot(inertie_euclidean[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")
points(c(2, 3, 5), inertie_euclidean[c(2, 3, 5)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

wardRange_euclidean <- as.clustrange(arbre_euclidean, diss = distance_euclidean, ncluster = 15)
plot(wardRange_euclidean, 
     stat = c('ASW','CH','HC','HG','PBC'), 
     norm = "zscore", 
     main = "Indicateurs de découpage des classes du dendrogramme pour Euclidienne")

# ==================================================
# 3. PROJECTION DES CLASSES SUR L'ACM
# ==================================================

groupes_phi2_2 <- cutree(arbre_phi2, k = 2)
groupes_gower_2<- cutree(arbre_gower, k = 2)
groupes_euclidean_3 <- cutree(arbre_euclidean, k = 3)

individus$classe_phi2 <- factor(groupes_phi2_2)
individus$classe_gower <- factor(groupes_gower_2)
individus$classe_euclidean <- factor(groupes_euclidean_3)

ggplot(individus, aes(x = Axis1, y = Axis2, color = classe_phi2)) +
  geom_point(size = 1.8, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "1" = "red",
      "2" = "blue"
    )
  )  +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Projection des classes – CAH phi²",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Classe"
  ) +
  theme_minimal()

ggplot(individus, aes(x = Axis1, y = Axis2, color = classe_gower)) +
  geom_point(size = 1.8, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "1" = "red",
      "2" = "blue"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Projection des classes – CAH Gower",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Classe"
  ) +
  theme_minimal()

ggplot(individus, aes(x = Axis1, y = Axis2, color = classe_euclidean)) +
  geom_point(size = 1.8, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "1" = "red",
      "2" = "blue",
      "3" = "green"
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
  labs(
    title = "Projection des classes – CAH Euclidean",
    x = "Dimension 1",
    y = "Dimension 2",
    color = "Classe"
  ) +
  theme_minimal()

# ==================================================
# 4. TABLEAUX DE MODALITÉS PAR CLASSE
# ==================================================

d1_phi2 <- d %>%
  mutate(typo = factor(groupes_phi2_2))

d1_gower <- d %>%
  mutate(typo = factor(groupes_gower_2))

d1_euclidean <- d_euclidean %>%
  mutate(typo = factor(groupes_euclidean_3))

table(d1_phi2$typo)
table(d1_gower$typo)
table(d1_euclidean$typo)

# Tableaux de répartitions
ggtable(
  d1_phi2,
  columnsX = "typo",
  columnsY = names(d1_phi2)[1:8],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")
ggtable(
  d1_phi2,
  columnsX = "typo",
  columnsY = names(d1_phi2)[9:16],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")
ggtable(
  d1_phi2,
  columnsX = "typo",
  columnsY = names(d1_phi2)[17:24],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")

ggtable(
  d1_gower,
  columnsX = "typo",
  columnsY = names(d1_gower)[1:8],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")
ggtable(
  d1_gower,
  columnsX = "typo",
  columnsY = names(d1_gower)[9:16],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")
ggtable(
  d1_gower,
  columnsX = "typo",
  columnsY = names(d1_gower)[17:24],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")

moyennes_euclidiennes <- d1_euclidean %>%
  group_by(typo) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )
moyennes_euclidiennes