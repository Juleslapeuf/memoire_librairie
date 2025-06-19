pdml <- read_excel("Documents/parts de marché du livre.xlsx")

pourcent <- pdml %>%
  mutate_at(c("autres", "gs non spe", "gs spe", "interne", "libraire", "VPC, club, courtage"), ~. *100) %>%
  rename(gsnonspe = 'gs non spe',
         gsspe = 'gs spe',
         VPC = "VPC, club, courtage")

pourcent_long <- pivot_longer(pourcent, -années, names_to = "variable", values_to = "pourcentage")
pourcent_long$années <- as.integer(pourcent_long$années)

pourcent_specific_years <- pourcent_long %>%
  filter(années %in% c(2012, 2022)) %>%
  mutate(hjust_value = ifelse(années == 2012, 1.2, -0.2))

pourcent_position2020 <- pourcent_long %>%
  filter(années %in% c(2020)) %>%
  mutate(vjust_value = case_when(
    variable == "autres" ~ 2, #Autres
    variable == "gsnonspe" ~ 2, #gs spe
    variable == "gsspe" ~ -2, # libraires
    variable == "interne" ~ -4, #gs non spe
    variable == "VPC" ~ -1, #VPC
    variable == "libraire" ~ 4 #internet
  ))

# Parts du marché du livre

ggplot(pourcent_long, aes(x = années, y = pourcentage, color = variable)) +
  geom_line(size = 1) +  # Augmentation de l'épaisseur des lignes
  geom_text(data = subset(pourcent_long, années == 2020), aes(label = c("Autres", "GS spécialisées culture", "Librairies indépendantes", "GS non spécialisées", "Internet", "Vente par correspondance, club, courtage")), hjust = 0, vjust = pourcent_position2020$vjust_value, size = 3, color = "black") +  # Ajout du nom des lignes à la fin du graphique
  geom_text(data = subset(pourcent_specific_years), aes(label = paste0(round(pourcentage, 1), "%")), hjust = pourcent_specific_years$hjust_value, size = 3, color = "black") +  # Afficher les valeurs en pourcentage pour la dernière année
  geom_vline(xintercept = seq(2012, 2022, by = 1), linetype = "dotted", color = alpha("gray50", 0.25)) +  # Lignes pour les années
  geom_hline(yintercept = seq(0, 30, by = 5), linetype = "dashed", color = alpha("gray50", 0.25)) +  # Lignes pour les dizaines
  labs(x = NULL, y = "(en %)") +
  scale_color_discrete(name = NULL, labels = c("Autres", "Grandes surfaces spécialisées culture", "Librairies indépendantes", "Grandes surfaces non spécialisées", "Internet", "Vente par correspondance, club, courtage")) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Suppression des lignes de grille
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = "none", fill = NA),
    legend.position = "bottom"  # Position de la légende
  ) +
  scale_x_continuous(breaks = unique(pourcent_long$années)) + # Afficher toutes les années sur l'axe 
  ggtitle("Lieux d'achats de livres imprimés en France") +  # Ajout du titre
  labs(caption = "Source: Kantar - Ministère de la Culture")  # Ajout de la source

#Format des livres vendus en France en valeur auprès des particuliers
années <- c(seq(2011, 2022, by = 1))
livre_neufv <- c(95.2, 92.8, 92.6, 91.9, 91.3, 90.8, 90.3, 89.9, 89.5, 88.2, 88.0, 88.9)
livre_ocassv <- c(3.7, 5.4, 5.5, 5.9, 6.1, 6.3, 6.4, 6.7, 6.8, 7.5, 7.9, 7.2)
livre_numv <- c(1.0, 1.8, 1.9, 2.3, 2.7, 2.9, 3.2, 3.5, 3.7, 4.3, 4.1, 3.9)
livres_vendus_valeur <- data.frame(années, livre_neuf, livre_num, livre_ocass)

livre_neufvo <- c(86.3, 84.8, 83.9, 83.1, 82.2, 80.8, 80.2, 79.5, 78.9, 78.0, 79.2, 79.0)
livre_ocassvo <- c(12.7, 12.9, 13.6, 14.1, 14.4, 15.4, 15.7, 16.1, 16.5, 16.3, 15.8, 16.3)
livre_numvo <- c(1.0, 2.3, 2.5, 2.8, 3.5, 3.8, 4.1, 4.4, 4.6, 5.7, 5.0, 4.8)
livres_vendus_volume <- data.frame(années, livre_numvo, livre_ocassvo, livre_neufvo)

livres_vendus <- left_join(livres_vendus_valeur, livres_vendus_volume, by = "années")

livres_long <- pivot_longer(livres_vendus, -années, names_to = "type", values_to = "ventes")

livres_long_specificyears <- livres_long %>%
  filter(années %in% c(2011, 2022)) %>%
  mutate(hjust_value = case_when(
    années == 2022 ~ -0.2,
    années == 2011 ~ 1.2),
    vjust_value = case_when(
      type == "livre_num" ~ 1.5,
      TRUE ~ 0
    ))

livres_long2020 <- livres_long %>%
  filter(années %in% c(2020))%>%
  mutate(vjust_value = case_when(
    type == "livre_neuf" ~ -1.5,
    type == "livre_num" ~ 2, #gs spe
    type == "livre_ocass" ~ -1.75, # libraires
    type == "livre_numvo" ~ -0.4, #gs non spe
    type == "livre_ocassvo" ~ -1, #VPC
    type == "livre_neufvo" ~ 1.5
  ))

palette_couleurs <- c("springgreen", "springgreen4", "tan3", "sienna1", "purple4", "mediumorchid")

ggplot(livres_long, aes(x = années, y = ventes, color = type)) +
  geom_line(size = 1) +  # Augmentation de l'épaisseur des lignes
  geom_text(data = subset(livres_long, années == 2020), aes(label = c("Neufs en valeur", "Numériques en valeur", "D'occasion en valeur", "Numériques en volume", "D'occasion en volume", "Neufs en volume")), hjust = 0, vjust = livres_long2020$vjust_value, size = 3, color = "black") +  # Ajout du nom des lignes à la fin du graphique
  geom_text(data = subset(livres_long_specificyears), aes(label = paste0(round(ventes, 1), "%")), hjust = livres_long_specificyears$hjust_value, vjust = livres_long_specificyears$vjust_value,  size = 3, color = "black") +  # Afficher les valeurs en pourcentage pour la dernière année
  geom_vline(xintercept = seq(2011, 2022, by = 1), linetype = "dotted", color = alpha("gray50", 0.25)) +  # Lignes pour les années
  geom_hline(yintercept = seq(0, 100, by = 5), linetype = "dashed", color = alpha("gray50", 0.25)) +  # Lignes pour les dizaines
  labs(x = NULL, y = "(en %)") +
  scale_color_manual(name = NULL, values = palette_couleurs, label = c("Neufs en valeur", "Neufs en volume", "Numériques en valeur", "Numériques en volume", "D'occasion en valeur", "D'occasion en volume")) +  # Utiliser la palette de couleurs personnalisée pour les courbes
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Suppression des lignes de grille
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = , fill = NA),  # Suppression du cadre autour du graphique
    legend.position = "bottom"  # Position de la légende
  ) +
  scale_x_continuous(breaks = unique(livres_long$années)) + # Afficher toutes les années sur l'axe 
  ggtitle("Vente de livres en France auprès des particuliers") +  # Ajout du titre
  labs(caption = "Source: Kantar - Ministère de la Culture")

# Indicateurs de gestion
p <- donnees_preparees %>%
  select("taux_retour", "age_stock", "taux_rotation", "renta_m2") %>%
  tbl_summary()

p_df <- as.data.frame(p) %>%
  rename(variable = '**Characteristic**',
         valeur = '**N = 304**')%>%
  mutate(valeur1 = c(57, 31, 31, 8.9))

nouveaux_noms <- c("Somme sur le compte en banque", "CA de la semaine", "Taux de retour", "Pourcentage de remise", "Panier moyen de la semaine", "Nombre de passage en caisse de la semaine", "Âge du stock", "Taux de rotation du stock", "Besoin en fonds de roulement", "Rentabilité au m2")
noms <- c("Taux de retour", "Âge du stock", "Taux de rotation du stock", "Rentabilité au m2")

decouper_noms <- function(noms) {
  str_wrap(noms, width = 10)  # Changer la largeur pour ajuster le découpage
}

ggplot(p_df, aes(x = reorder(variable, -valeur1), y = valeur1)) +
  geom_bar(stat = "identity", fill = "royalblue", color = "black", size = 1) +  # Contours des barres
  geom_text(aes(label = paste0(valeur1, "%")), vjust = -0.5, color = "black", size = 5) +  # Valeurs dans les barres
  labs(x = NULL, y = "Oui en %", title = "Avez-vous un chiffre en tête... ? (N = 304)",
       caption = "Source : Questionnaire d'enquête") +
  theme_minimal() + # Thème minimal pour le graphique
  scale_x_discrete(labels = decouper_noms(noms)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank())

#Logiciel gestion et base de données biblio
q <- donnees_preparees %>%
  select("logspec", "bibnum") %>%
  tbl_summary()
q
q_df <- as.data.frame(q) %>%
  rename(variable = '**Characteristic**',
         valeur = '**N = 304**')%>%
  mutate(valeur1 = c(97, 48))

nomsq <- c("Logiciel spécialisé libraire", "Bibliographie numérique")

decouper_nomsq <- function(nomsq) {
  str_wrap(nomsq, width = 10)  # Changer la largeur pour ajuster le découpage
}

ggplot(q_df, aes(x = reorder(variable, -valeur1), y = valeur1)) +
  geom_bar(stat = "identity", fill = "royalblue", color = "black") +  # Contours des barres
  geom_text(aes(label = paste0(valeur1, "%")), vjust = -0.5, color = "black", size = 5) +  # Valeurs dans les barres
  labs(x = NULL, y = "Oui en %", title = "Utilisation hebdomadaire (N = 304)",
       caption = "Source : Questionnaire d'enquête") +
  theme_minimal() + # Thème minimal pour le graphique
  scale_x_discrete(labels = decouper_nomsq(nomsq)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    panel.grid.major = element_blank())

#Box-plot taille des librairies
summary_stats <- summary(donnees_preparees$taille_reco)

ggplot(data = donnees_preparees, aes(x = taille_reco)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = , fill = NA),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  geom_vline(xintercept = seq(8, 1300, by = 100), linetype = "dotted", color = alpha("gray50", 0.25)) +
  labs(
    y = NULL,
    x = "m²",
    title = "Surface commerciale des libraires (N = 300)",
    caption = "Source : Questionnaire d'enquête"
  ) +
  scale_x_continuous(limits = c(0, 1250), breaks = seq(0, 1250, by = 100))
  

#Pratiques culturelles : la lecture
pc <- read_excel("~/Documents/pratiques_lectures.xlsx")
pc <- pc %>%
  filter(annees != 1988)

pc <- pc %>%
  mutate(hjustvalue = case_when(
    annees == 1997 ~ 2.5,
    annees == 2008 ~ 0.5,
    annees == 2018 ~ -1.5
  ),
  vjustvalue = case_when(
    annees == 1997 ~ -0.5,
    annees == 2008 ~ -0.5,
    annees == 2018 ~ -0.5
  )) %>%
  mutate(vjustvaluevingt = case_when(
    annees == 1997 ~ 2,
    annees == 2008 ~ 2,
    annees == 2018 ~ 2
  ))

ggplot(pc, aes(x = age)) +
  geom_bar(aes(y = valeur_un, fill = as.factor(annees)), stat = "identity", position = "dodge", width = 0.7, color = "yellow2", size = 2) +
  geom_bar(aes(y = valeur_vingt, fill = as.factor(annees)), stat = "identity", position = "dodge", width = 0.7, color = "red2", size = 2) +
  scale_fill_manual(values = c('1997' = "royalblue", '2008' = "blue", '2018' = "navyblue"), guide = "none") +  # Définir une seule couleur pour chaque variable
  geom_text(aes(y = valeur_un, label = valeur_un),hjust = pc$hjustvalue, vjust = pc$vjustvalue, color = "black", size = 6) +
  geom_text(aes(y = valeur_vingt, label = valeur_vingt),hjust = pc$hjustvalue, vjust = pc$vjustvaluevingt, color = "white", size = 6) +
  geom_text(aes(y = 0, label = "2018"), hjust = -1, vjust = 1.5, color = "black", size = 4, position = position_dodge(width = 0.7)) +  # Texte en dessous de chaque barre
  geom_text(aes(y = 0, label = "2008"), hjust = 0.5, vjust = 1.5, color = "black", size = 4, position = position_dodge(width = 0.7)) +  # Texte en dessous de chaque barre
  geom_text(aes(y = 0, label = "1997"), hjust = 2, vjust = 1.5, color = "black", size = 4, position = position_dodge(width = 0.7)) +  # Texte en dessous de chaque barre
  geom_text(x = 0.5, y = 100, label = "Lecture : En 1997, 80 % des 15 - 19 ans ont lu au moins un livre au cours des douze derniers mois dont 18 % vingt et plus.", color = "black", size = 4, hjust = -0.4) + 
  labs(
    y = "%",
    x = NULL,
    title = "Lecture de livres selon l'âge",
    caption = 'Source : Enquête Pratiques culturelles - Ministère de la Culture'
  ) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = , fill = NA)
  )



#Taux de retour en fonction de différents indicateurs
donnees_preparees %>%
  select(taux_retour, age_rev_rec, nb_salaries_, ca_, surfco_) %>%
  tbl_summary(by = taux_retour, percent = "row")

donnees_preparees %>%
  select(age_stock, age_rev_rec, nb_salaries_, ca_, surfco_) %>%
  tbl_summary(by = age_stock, percent = "row")

donnees_preparees %>%
  select(taux_rotation, age_rev_rec, nb_salaries_, ca_, surfco_) %>%
  tbl_summary(by = taux_rotation, percent = "row")



#Rentabilité au mètre carré
ggplot(donnees_preparees, aes(y = rentabilité, x = taille_reco)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(donnees_preparees, aes(y = rentabilité, x = ca_rec)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  

#Corrélation entre taille, ca et nombre de salariés
ggplot(donnees_preparees, aes(x = taille_reco, y = ca_rec)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Taille reco", y = "CA rec", title = "Nuage de points avec droite affine")

ggplot(donnees_preparees, aes(x = taille_reco, y = nb_salaries)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Taille reco", y = "Nombre de salariés", title = "Nuage de points avec droite affine")

ggplot(donnees_preparees, aes(x = nb_salaries, y = ca_rec)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Nombre de salariés", y = "CA rec", title = "Nuage de points avec droite affine")


###Recours à un site internet de vente ou à un portail associatif en fonction de la taille de la librairie et du ca

#SITVENT ET CA
#modèle pour la suite

# Je crée le tableau croisé avec les données
g <- donnees_preparees %>%
  select(sitven, ca_) %>%
  tbl_summary(by = sitven, percent = "row")

#Je le convertis en df et ne garde que les données qui m'intéressent
g_df <- as.data.frame(g) %>%
  select(1, 3) %>%
  rename(variable = '**Characteristic**',
         valeur = '**1**, N = 116') %>%
  filter(variable != "ca_",
         variable != "Unknown") %>%
  mutate(valeur1 = c(75, 33, 50, 25)) #Je ne comprends pas pq c'est dans cet ordre ??!!

#Je recode la variable pour avoir des modalités qui apparaissent bien sur le graphique
g_df$variable <- g_df$variable %>%
  as.character() %>%
  fct_recode(
    "Inférieur à 300k (n = 91)" = "Inférieur à 300k",
    "300k à 599k (n = 96)" = "300k à 599k",
    "600k à 1199k (n = 46)" = "600k à 1199k",
    "1,2 M€ et plus (n = 28)" = "1,2 M€ et plus"
  )

table(donnees_preparees$ca_)

ordre_modalites <- c(
  "Inférieur à 300k (n = 91)",
  "300k à 599k (n = 96)",
  "600k à 1199k (n = 46)",
  "1,2 M€ et plus (n = 28)"
)

#Fonction pour écrire sur deux lignes distinctes
separer_modalites <- function(modalites) {
  modalites_separees <- gsub("\\(n =", "\n(n =", modalites)
  return(modalites_separees)
}
#Application de la fonction aux modalités du ca
modalites_separees <- separer_modalites(ordre_modalites)

#création du graphique
gplot <- ggplot(g_df, aes(x = variable, y = valeur1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = NULL, color = NULL),  # Supprimer les lignes en arrière-plan
    panel.grid.major.y = element_line(color = "lightgray", size = 0.15),  # Lignes verticales claires
    panel.grid.major.x = element_blank(),  # Supprimer les lignes en abscisse
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = "solid", fill = NA),
    legend.position = "bottom",
    plot.title = element_text(size = 10)
  ) +
  labs(x = NULL, 
       y = "%", 
       title = "Utilisation hebdomadaire de son propre site internet de vente selon le chiffre d'affaires (N = 261)") +
  geom_text(aes(y = valeur1, label = valeur1),hjust = 0.5, vjust = -0.5, color = "black", size = 4) +
  scale_x_discrete(labels = modalites_separees)

#PORTASSO ET CA
h <- donnees_preparees %>%
  select(portasso, ca_) %>%
  tbl_summary(by = portasso, percent = "row")

h_df <- as.data.frame(h) %>%
  select(1, 3) %>%
  rename(variable = '**Characteristic**',
         valeur = '**1**, N = 174') %>%
  filter(variable != "ca_",
         variable != "Unknown") %>%
  mutate(valeur1 = c(57, 58, 59, 57))

h_df$variable <- h_df$variable %>%
  as.character() %>%
  fct_recode(
    "Inférieur à 300k (n = 91)" = "Inférieur à 300k",
    "300k à 599k (n = 96)" = "300k à 599k",
    "600k à 1199k (n = 46)" = "600k à 1199k",
    "1,2 M€ et plus (n = 28)" = "1,2 M€ et plus"
  )

h_df$variable <- factor(h_df$variable, levels = ordre_modalites)

hplot <- ggplot(h_df, aes(x = variable, y = valeur1)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = NULL, color = NULL),  # Supprimer les lignes en arrière-plan
    panel.grid.major.y = element_line(color = "lightgray", size = 0.15),  # Lignes verticales claires
    panel.grid.major.x = element_blank(),  # Supprimer les lignes en abscisse
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = "solid", fill = NA),
    legend.position = "bottom",
    plot.title = element_text(size = 10)
  ) +
  labs(x = NULL, 
       y = "%", 
       title = "Utilisation hebdomadaire d'un portail associatif selon le chiffre d'affaires (N = 261) "
  ) +
  geom_text(aes(y = valeur1, label = valeur1),hjust = 0.5, vjust = -0.5, color = "black", size = 4) +
  scale_x_discrete(labels = modalites_separees) +
  scale_y_continuous(limits = c(0, 100))

#PORTASSO et TAILLE
i <- donnees_preparees %>%
  select(portasso, surfco_) %>%
  tbl_summary(by = portasso, percent = "row")

i_df <- as.data.frame(i)%>%
  select(1, 3) %>%
  rename(variable = '**Characteristic**',
         valeur = '**1**, N = 174') %>%
  filter(variable != "surfco_",
         variable != "Unknown") %>%
  mutate(valeur1 = c(59, 55, 56))

i_df$variable <- i_df$variable %>%
  as.character() %>%
  fct_recode(
    "Moins de 100 m2 (n = 168)" = "Moins de 100 m2",
    "Entre 100 et 199 m2 (n = 100)" = "Entre 100 et 199 m2",
    "Plus de 200 m2 (n = 32)" = "Plus de 200 m2"
  )

ordre_modalites_taille <- c("Moins de 100 m2 (n = 168)",
                            "Entre 100 et 199 m2 (n = 100)",
                            "Plus de 200 m2 (n = 32)")
modalites_separees_taillle <- separer_modalites(ordre_modalites_taille)
i_df$variable <- factor(i_df$variable, levels = ordre_modalites_taille)

iplot <- ggplot(i_df, aes(x = variable, y = valeur1)) +
  geom_bar(stat = "identity", fill = "royalblue", color = "black") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = NULL, color = NULL),  # Supprimer les lignes en arrière-plan
    panel.grid.major.y = element_line(color = "lightgray", size = 0.15),  # Lignes verticales claires
    panel.grid.major.x = element_blank(),  # Supprimer les lignes en abscisse
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = "solid", fill = NA),
    legend.position = "bottom",
    plot.title = element_text(size = 10)
  ) +
  labs(caption = "Questionnaire d'enquête",
       x = NULL, 
       y = "%", 
       title = "Utilisation hebdomadaire d'un portail associatif selon la surface commerciale (N = 300) "
  ) +
  geom_text(aes(y = valeur1, label = valeur1),hjust = 0.5, vjust = -0.5, color = "black", size = 4) +
  scale_x_discrete(labels = modalites_separees_taillle) +
  scale_y_continuous(limits = c(0, 100))

#SITVEN et TAILLE
j <- donnees_preparees %>%
  select(sitven, surfco_) %>%
  tbl_summary(by = sitven, percent = "row")

j_df <- as.data.frame(j)%>%
  select(1, 3) %>%
  rename(variable = '**Characteristic**',
         valeur = '**1**, N = 116') %>%
  filter(variable != "surfco_",
         variable != "Unknown") %>%
  mutate(valeur1 = c(26, 47, 72))

j_df$variable <- j_df$variable %>%
  as.character() %>%
  fct_recode(
    "Moins de 100 m2 (n = 168)" = "Moins de 100 m2",
    "Entre 100 et 199 m2 (n = 100)" = "Entre 100 et 199 m2",
    "Plus de 200 m2 (n = 32)" = "Plus de 200 m2"
  )

ordre_modalites_taille <- c("Moins de 100 m2 (n = 168)",
                            "Entre 100 et 199 m2 (n = 100)",
                            "Plus de 200 m2 (n = 32)")
modalites_separees_taillle <- separer_modalites(ordre_modalites_taille)
j_df$variable <- factor(j_df$variable, levels = ordre_modalites_taille)

jplot <- ggplot(j_df, aes(x = variable, y = valeur1)) +
  geom_bar(stat = "identity", fill = "royalblue", color = "black") +
  theme_light() +
  theme(
    panel.background = element_rect(fill = NULL, color = NULL),  # Supprimer les lignes en arrière-plan
    panel.grid.major.y = element_line(color = "lightgray", size = 0.15),  # Lignes verticales claires
    panel.grid.major.x = element_blank(),  # Supprimer les lignes en abscisse
    panel.grid.minor = element_blank(), 
    panel.border = element_rect(linetype = "solid", fill = NA),
    legend.position = "bottom",
    plot.title = element_text(size = 10)
  ) +
  labs(x = NULL, 
       y = "%", 
       title = "Utilisation hebdomadaire d'un site internet de vente selon la surface commerciale (N = 300) "
  ) +
  geom_text(aes(y = valeur1, label = valeur1),hjust = 0.5, vjust = -0.5, color = "black", size = 4) +
  scale_x_discrete(labels = modalites_separees_taillle) +
  scale_y_continuous(limits = c(0, 100))

grid.arrange(gplot, hplot, jplot, iplot, ncol = 2)

#TABLEAU DE CORRELATION
correlation_matrix <- cor(donnees_preparees[c("nb_salaries", "ca_rec", "taille_reco")], use = "pairwise.complete.obs")
# Arrondir les coefficients de corrélation à deux décimales
correlation_matrix_rounded <- round(correlation_matrix, 2)
# Créer le tableau de corrélation
table_correlation <- cbind(correlation_matrix_rounded)
tab_cor <- as.data.frame(table_correlation)

tab_cor <- tab_cor %>%
  mutate(variables = c("Nb salariés", "CA", "m2"))
tab_cor <- tab_cor %>%
  select(4, 1, 2, 3)

gt(tab_cor) %>%
  cols_label(
    taille_reco = "m2",
    ca_rec = "CA",
    nb_salaries = "Nb salariés"
  ) %>%
  tab_header(
    title = "Tableau de corrélation"
  )

#FREQUENCE DES CLASSES CAH
frequence <- freq(classe) %>%
  mutate(classe = c(1, 2, 3)) %>%
  select(4, 1, 2)

gt(frequence) %>%
  tab_header(
    title = "Fréquence des classes"
  )
gt(cordim1) %>%
  tab_header(
    title = "Coefficients de corrélations des modalités à l'axe 1"
  )

#TABLEAU DES PROPORTIONS PAR REGION
tableau_prop_regions <- nb_regions_df %>%
  mutate(prop_regions = round(frq_regions * 100 / 257, 1))

tab_regions_carto <- left_join(tableau_prop_regions, nb_regions_ursaf, by = "nom_regions")

gt(tab_regions_carto) %>%
  tab_header(
    title = "Détail des proportions de répondants par région"
  ) %>%
  cols_label(
    nom_regions = "Région",
    frq_regions = "Fréquence",
    prop_regions = "Proportion (%)",
    frq_regions_ursaf = "Fréquence",
    prop_regions_ursaf = "Proportion (%)"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )%>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2)
    ),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2)
    ),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "prop_regions"
    )
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "black",
      weight = px(2)
    ),
    locations = cells_body(
      columns = "nom_regions"
    )
  ) %>%
  tab_footnote(
    footnote = "Source : Questionnaire d'enquête 2024",
    locations = cells_column_labels(columns = c(frq_regions, prop_regions))
    ) %>%
  tab_footnote(
    footnote = "Source : URSAF 2020",
    locations = cells_column_labels(columns = c(frq_regions_ursaf, prop_regions_ursaf))
  )

#Par rapport à si oui ou non ça pourrait fonctionner sans
foncti <- donnees_preparees[, c("logspe_panne","etiq", "liseuse", "gpdiscu", "sitven", "portasso", "maillist", "obslib", "bibnum", "ressoc", "ressocspe", "logspec", "surfco_", "nb_salaries_", "ca_", "taux_retour", "pass_cais", "ca_semaine", "remise", "panier_moy", "age_stock", "taux_rotation", "compte_banq", "renta_m2", "fonds_roule")]
foncti <- foncti %>%
  filter(complete.cases(across(-"logspe_panne")))

panne1 <- c("Non", "Oui")
panne2 <- c(241, 17)
panne <- data.frame(panne1, panne2)
panne <- panne %>%
  mutate(prop_panne = round(panne2*100/258, 1))

ggplot(panne, aes(x = "", y = prop_panne, fill = panne1)) +
  geom_bar(stat = "identity", width = 1, color = "black") + # Ajout des lignes de séparation
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("royalblue", "red")) + # Changer les couleurs
  labs(
    fill = NULL,
    y = "Proportion (%)",
    x = "",
    title = "Votre librairie pourrait-elle fonctionner comme d’habitude si votre logiciel spécialisé tombait en panne ?",
    subtitle = "Champ : Individus de l'ACM",
    caption = "Source : Questionnaire d'enquête"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 1)
  ) +
  geom_text(aes(label = paste(prop_panne, "%")), 
            position = position_stack(vjust = 0.5), 
            color = "white", 
            size = 5)

