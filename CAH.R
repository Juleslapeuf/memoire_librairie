#install.packages("cluster")
library(cluster)
#install.packages("TraMineR")
library(TraMineR)
#install.packages("WeightedCluster")
library(WeightedCluster)
install.packages("GGally")
library(GGally)

source("/home/onyxia/work/memoire_librairie/recodage.R")

#CAH
donnees_preparees <- donnees_preparees %>%
  rename(surfco_ = "taille_reco_rec",
         nb_salaries_ = "nb_salaries_rec")

d <- donnees_preparees %>%
  select(
    etiq, liseuse, gpdiscu, sitven, portasso,
    maillist, obslib, bibnum, ressoc, ressocspe,
    logspec, surfco_, nb_salaries_, ca_,
    taux_retour, pass_cais, ca_semaine, remise,
    panier_moy, age_stock, taux_rotation,
    compte_banq, renta_m2, fonds_roule
  )
d <- mutate_all(d, as.factor)
d <- na.omit(d)

# Classification Ascendante Hiérarchique (CAH) ----

distance <- daisy(d, metric = "euclidean")

cah <- agnes(distance, metric = "euclidean", method = "ward")

dendro <- as.hclust(cah) %>% 
  as.dendrogram()
plot(dendro, leaflab = "none", ylab = "Distance entre les noeuds", main = "Dendrogramme de la CAH")
as.hclust(cah) %>% rect.hclust(k = 3, border = "red")

nbcl <- 3
classe <- cutree(cah, nbcl) %>% as.factor()
table(classe)


# On peut aussi représenter la part d'inertie expliquée (R2) selon le nombre de classes
ahc.plots(cah, distance = distance, type = "inert")
# Ou la perte relative d'inertie expliquée (R2) à chaque nouvelle agrégation
ahc.plots(cah, distance = distance, type = "loss")

inertie <- sort(cah$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie") # Un saut plutôt à 4 ou 6
points(c(3, 4, 6), inertie[c(2, 4, 6)], col = c("green3", "red3", "blue3"), cex = 2, lwd = 3)

# Indicateurs de qualité des partitions
wardRange <- as.clustrange(cah, diss = distance, ncluster = 15)
summary(wardRange, max.rank = 2)
plot(wardRange, stat = c('ASW','CH','HC','HG','PBC'), norm = "zscore", main = "Indicateurs de découpage des classes du dendrogramme")

d1 <- d

typo <- cutree(cah, 3)
table(typo)
d1$typo <- cutree(cah, 3)
d1$typo <- factor(d1$typo)

ggtable(
  d1,
  columnsX = "typo",
  columnsY = names(d1)[1:8],
  cells = "col.prop")
ggtable(
  d1,
  columnsX = "typo",
  columnsY = names(d1)[9:16],
  cells = "col.prop")
ggtable(
  d1,
  columnsX = "typo",
  columnsY = names(d1)[17:24],
  cells = "col.prop")

ggtable(
  d1,
  columnsX = "typo",
  columnsY = names(d1)[9:16],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")
ggtable(
  d1,
  columnsX = "typo",
  columnsY = names(d1)[17:24],
  cells = "col.prop",
  fill = "std.resid") +
  labs(fill = "Résidus standardisés du Chi²") +
  theme(legend.position = "bottom")

rm(list = c("cah", "dendro", "wardRange"))