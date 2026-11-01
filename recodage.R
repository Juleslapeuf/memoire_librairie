library(tidyverse)
library(stringi)

rm(list = ls())

#Les données proviennent d'un questionnaire que j'ai fait passer par internet à des libraires 
#généralistes indépendants. Dans certains cas, j'ai fait passer le questionnaire par voie téléphonique,
#et parfois j'ai contacté directement des libraires afin de discuter de leurs réponses au questionnaire.
#Mon coeur de cible était les gérants des librairies, et il s'avère que la passation par Internet 
#n'était clairement pas le meilleur moyen d'y arriver, d'ailleurs au téléphone, il fallait souvent que 
#je demande expréssement à parler à la gérante car j'avais souvent une salariée au téléphone. 
#De fait, j'ai exclu les variables socio-démographiques et je n'ai gardé que les variables concernant 
#l'entreprise. Pour une future enquête, j'aurais des erreurs à ne pas commettre à nouveau.

doc_quest <- read.csv("https://minio.lab.sspcloud.fr/juleslapeuf/data_bis.csv")

generate_code <- function() {
  digits_part1 <- stri_rand_strings(1, 2, "[0-9]")
  letters <- stri_rand_strings(1, 2, "[A-Z]")
  digits_part2 <- stri_rand_strings(1, 2, "[0-9]")
  return(paste0(digits_part1, letters, digits_part2))
}

doc_quest <- doc_quest %>%
  mutate(code = sapply(1:nrow(doc_quest), function(x) generate_code()))

#J'ai pu contacté les librairies généralistes indépendantes par mail grâce à une liste de mails 
#récupérée auprès d'un professionnel de la chaîne du livre, et à partir de là j'ai ciblé les librairies 
#généralistes indépendantes, en faisant attention de n'avoir que des librairies dont l'activité 
#principale était la vente de livres. Même après sélection des adresses mail, il me restait encore des 
#oublis, d'où le filtre sur le type de librairie.

donnees_preparees <- doc_quest %>%
  rename(annee_naiss = 'Quelle.est.votre.année.de.naissance....AAAA.',
         codepostal = 'Quel.est.le.code.postal.de.la.commune.de.votre.librairie..',
         annee_install = 'En.quelle.année.vous.êtes.vous.installé.dans.votre.librairie....AAAA.',
         sal_tpl = 'Combien.y.a.t.il.d.employés.à.temps.plein....en.chiffre.',
         sal_tpa = 'Combien.y.a.t.il.d.employés.à.temps.partiel....en.chiffre.',
         taille = 'Quelle.est.la.surface.commerciale.de.votre.librairie....en.m...',
         ca = 'Quel.était.le.chiffre.d.affaires.de.la.librairie.l.année.dernière....en.euro.',
         type = 'Votre.librairie.est.elle.....',
         une_fois = 'Lesquels.de.ces.outils.utilisez.vous.au.moins.1.fois.par.semaine..',
         jamais = "Lesquels.de.ces.outils.n.avez.vous.jamais.utilisé.au.travail...",
         chiffre_pre = 'Au.moment.où.vous.répondez.à.ce.questionnaire..avez.vous.en.tête.un.chiffre.précis.de.....',
         inq_av = 'Sur.une.échelle.de.1.à.10..avant.la.pandémie..étiez.vous.inquiet.pour.l.avenir.de.votre.commerce....',
         inq_p = 'Sur.une.échelle.de.1.à.10..en.plein.cœur.de.la.pandémie..avril.2020...étiez.vous.inquiet.pour.l.avenir.de.votre.commerce....',
         inq_ap = 'Sur.une.échelle.de.1.à.10..êtes.vous.inquiet.pour.l.avenir.de.votre.commerce....',
         inq_source = 'Quelle.est.votre.principale.source.d.inquiétude..',
         outils_inf = 'Avez.vous.recours.plus.souvent.à.des.outils.informatiques.et.ou.numériques.depuis.la.fin.de.la.pandémie..',
         logspe_panne = 'Votre.librairie.pourrait.elle.fonctionner.comme.d.habitude.si.votre.logiciel.spécialisé.tombait.en.panne...',
         diff_eco = 'Rencontrez.vous.des.difficultés.économiques...') %>%
  select(2:20)

donnees_preparees <- donnees_preparees %>%
  filter(type == "Généraliste")

#Maintenant, on passe au recodage à la main de certaines réponses. Soit pour harmoniser les modalités 
#(quand il y a un espace au milieu d'un nombre à 6 chiffres, quand le signe € était ajouté, ou un "." 
#ou un "k", "HT"...)...

#Année de naissance
donnees_preparees <- donnees_preparees %>%
  mutate(annee_reco = case_when(
    grepl("/19", annee_naiss) ~ substr(annee_naiss, 7, 10),
    nchar(annee_naiss) == 7 ~ paste0("19", substr(annee_naiss, 6, 7)),
    grepl("/6", annee_naiss) ~ paste0("19", substr(annee_naiss, 7, 8)),
    grepl("/7", annee_naiss) ~ paste0("19", substr(annee_naiss, 7, 8)),
    nchar(annee_naiss) == 8 ~ substr(annee_naiss, 5, 8),
    grepl(".19", annee_naiss) ~ substr(annee_naiss, 7, 10),
    nchar(annee_naiss) == 6 ~ paste0("19", substr(annee_naiss, 5, 6)),
    nchar(annee_naiss) == 2 ~ paste0("19", substr(annee_naiss, 1, 2)),
    annee_naiss == '1958 ...mais la question me semble hors de propos.' ~ "1958",
    row_number() == 31 ~ NA_character_,
    annee_naiss == "2006" ~ NA_character_,
    annee_naiss == 'Grain de lire' ~ NA_character_,
    TRUE ~ as.character(annee_naiss)
  ))

#Code postal
donnees_preparees <- donnees_preparees %>%
  mutate(postcode_clear = case_when(
    codepostal == "4 librairies 81500+81600+81300+ 81000" ~ "81000",
    codepostal == "1000 Bruxelles"~ "99131",
    codepostal == "29" ~ "29000",
    codepostal == "33" ~ "33000",
    codepostal == "45" ~ "45000",
    codepostal == "75" ~ "75001",
    TRUE ~ codepostal
  )) %>%
  mutate(postcode_clear_numeric = as.numeric(postcode_clear))

#Année installation
donnees_preparees <- donnees_preparees %>%
  mutate(annee_lib_reco = case_when(
    annee_install == '0922' ~ "2022",
    annee_install == "15032017" ~ "2017",
    annee_install == "respectivement 2011-2014-2016-2017" ~ "2011",
    TRUE ~ as.character(annee_install)
  ))

#SALARIES
#Recodage de certaines modalités, car elles ont été mal remplies
donnees_preparees[1, 5] <- "0"
donnees_preparees[18, 5] <- "0"
donnees_preparees[40, 4] <- "0"
donnees_preparees[51, 5] <- "1"
donnees_preparees[54, 5] <- "0"
donnees_preparees[79, 4] <- "0"
donnees_preparees[83, 5] <- "0"
donnees_preparees[103, 5] <- "0"
donnees_preparees[132, 5] <- "0"
donnees_preparees[139, 5] <- "0"
donnees_preparees[162, 5] <- "0"
donnees_preparees[194, 5] <- "0"
donnees_preparees[226, 5] <- "0"
donnees_preparees[248, 5] <- "2"
donnees_preparees[272, 5] <- "1"
donnees_preparees[272, 4] <- "0"
donnees_preparees[300, 5] <- "0"

#Nombre de salariés à temps plein
donnees_preparees$sal_tpl_rec <- donnees_preparees$sal_tpl %>%
  as.factor() %>%
  fct_recode(
    "1" = "1 + gérante",
    "1" = "1 salariée à 35h. ",
    "1" = "1,5",
    "1" = "1.5",
    "1" = "1+1",
    "2" = "2 et 2 co gérantes ",
    "0" = "2 gérents non salaries",
    "1" = "3 (dont 2 alternants)",
    "3" = "3 salariés + 1 gérante",
    "3" = "3,8"
  ) %>%
  as.character() %>%
  as.numeric()

#Nombre de salariés à temps partiel
donnees_preparees$sal_tpa_rec <- donnees_preparees$sal_tpa %>%
  as.factor() %>%
  fct_recode(
    "1" = "0,5",
    "1" = "1 (fin d'année)",
    "2" = "1 + une apprentie",
    "1" = "1 alternante",
    "1" = "1 salarié à 30 h"
  ) %>%
  as.character() %>%
  as.numeric()

#Somme = Nombre total de salariés
donnees_preparees <- donnees_preparees %>%
  mutate(nb_salaries = sal_tpl_rec + sal_tpa_rec)

#Taille
donnees_preparees <- donnees_preparees %>%
  mutate(taille_reco = as.numeric(sub("\\D*(\\d+).*", "\\1", taille)))

donnees_preparees[304, 7] <- "200000"

#Chiffre d'affaires
donnees_preparees$ca_rec <- donnees_preparees$ca %>%
  as.factor() %>%
  fct_recode(
    NULL = "0",
    "1000000" = "1 000 000",
    "1125000" = "1 125 000",
    "1350000" = "1 350 000",
    "1800000" = "1 800 000",
    "1100000" = "1,1",
    "1200000" = "1,2 millions euros",
    "1300000" = "1,3 M €",
    "1000000" = "1.000.000",
    "1900000" = "1.900.000",
    "100000" = "100 000",
    "110000" = "110 000 € ",
    "121000" = "121 000 HT",
    "130000" = "130 000 euros ",
    "130000" = "130000€",
    "150000" = "150",
    "1500000" = "1500000 TTC",
    "1550000" = "1550000 HT",
    "160000" = "160 k€",
    "164500" = "164500 euros",
    "1700000" = "170.000.00",
    "1923000" = "1923k€",
    "197000" = "197K€",
    NULL = "1ere année",
    "2000000" = "2 M",
    "2000000" = "2 Millions",
    "200000" = "200 000",
    "200000" = "200 000 €",
    "200000" = "200000 (prévi)",
    "210000" = "210 000",
    "222892" = "222892 ttc",
    "2433000" = "2433000 €",
    "250000" = "250 000",
    "255000" = "255 000",
    "261000" = "261.000",
    "270000" = "270",
    "270000" = "270000 HT",
    "272000" = "272 000",
    "3000000" = "3 000 000",
    "3300000" = "3 300 000",
    "300000" = "300 000",
    "300000" = "300.000",
    "310000" = "310 000",
    "311529" = "311529€",
    "317000" = "317 000",
    NULL = "320000 du 01/06/23 au 23/03/2024",
    "350000" = "350 000",
    "358000" = "358000 HT",
    "360000" = "360 000",
    "382881" = "382 881",
    "392000" = "392 000",
    "392300" = "392 300 TTC",
    "400000" = "400 000 €",
    "410000" = "410.000€",
    "450000" = "450 000",
    "456000" = "456 000",
    "480000" = "480 000 ",
    "483000" = "483000 HT",
    "484712" = "484.712 HT",
    "500000" = "500 000",
    "508000" = "508 000",
    "520000" = "520 000",
    "580000" = "580 000€",
    "65000" = "65",
    "650000" = "650 000",
    "650000" = "650000 HT",
    "738000" = "738000 HT",
    NULL = "75000€ ttc (de aôut à décembre)",
    "790000" = "790.000€ H.T.",
    "870000" = "870 k€",
    "90000" = "90 000",
    "95000" = "95 000",
    NULL = "entre 07/23 et 03/24 : 285 000",
    "2000000" = "environs 2 000 000 €",
    NULL = "Etablissement moyen",
    NULL = "je ne sais plus",
    "165000" = "Pas encore de bilan comptable terminé mais 165 000",
    NULL = "pas encore fait une année complète",
    "500000" = "plus de  500 000 €",
    "4000000" = "superieur 4 millions",
    NULL = "warf warf warf",
    "840000" = "840.000",
    "555000" = "555 K€ TTC",
    "290000" = "290 000",
    "205000" = "environ 205 000 euros",
    "126000" = "126 000",
    "260000" = "260 000",
    "24000" = "240 000"
  ) %>%
  as.character() %>%
  as.numeric()

#Variables sur les pratiques numériques
donnees_preparees <- donnees_preparees %>%
  mutate(logspec = case_when(
    str_detect(une_fois, "Logiciel") ~ 1,
    str_detect(jamais, "Logiciel") ~ 0,
    TRUE ~ 0
  ),
  ressoc = case_when(
    str_detect(une_fois, "Instagram") ~ 1,
    str_detect(jamais, "Instagram") ~ 0,
    TRUE ~ 0
  ),
  ressocspe = case_when(
    str_detect(une_fois, "Babelio") ~ 1,
    str_detect(jamais, "Babelio") ~ 0,
    TRUE ~ 0
  ),
  bibnum = case_when(
    str_detect(une_fois, "Electre") ~ 1,
    str_detect(jamais, "Electre") ~ 0,
    TRUE ~ 0
  ),
  obslib = case_when(
    str_detect(une_fois, "Verso") ~ 1,
    str_detect(jamais, "Verso") ~ 0,
    TRUE ~ 0
  ),
  maillist = case_when(
    str_detect(une_fois, "Mailing") ~ 1,
    str_detect(jamais, "Mailing") ~ 0,
    TRUE ~ 0
  ),
  portasso = case_when(
    str_detect(une_fois, "associatif") ~ 1,
    str_detect(jamais, "associatif") ~ 0,
    TRUE ~ 0
  ),
  sitven = case_when(
    str_detect(une_fois, "Propre") ~ 1,
    str_detect(jamais, "Propre") ~ 0,
    TRUE ~ 0
  ),
  IA = case_when(
    str_detect(une_fois, "artificielle") ~ 1,
    str_detect(jamais, "artificielle") ~ 0,
    TRUE ~ 0
  ),
  gpdiscu = case_when(
    str_detect(une_fois, "Discord") ~ 1,
    str_detect(jamais, "Discord") ~ 0,
    TRUE ~ 0
  ),
  liseuse = case_when(
    str_detect(une_fois, "Liseuse") ~ 1,
    str_detect(jamais, "Liseuse") ~ 0,
    TRUE ~ 0
  ),
  etiq = case_when(
    str_detect(une_fois, "queteuse") ~ 1,
    str_detect(jamais, "queteuse") ~ 0,
    TRUE ~ 0
  ))

#Variables sur les indicateurs comptables
donnees_preparees <- donnees_preparees %>%
  mutate(taux_retour = case_when(
    str_detect(chiffre_pre, "retour") ~ 1,
    TRUE ~ 0
  ),
  pass_cais = case_when(
    str_detect(chiffre_pre, "passages") ~ 1,
    TRUE ~ 0
  ),
  ca_semaine = case_when(
    str_detect(chiffre_pre, "chiffre") ~ 1,
    TRUE ~ 0
  ),
  remise = case_when(
    str_detect(chiffre_pre, "pourcentage") ~ 1,
    TRUE ~ 0
  ),
  panier_moy = case_when(
    str_detect(chiffre_pre, "panier") ~ 1,
    TRUE ~ 0
  ),
  age_stock = case_when(
    str_detect(chiffre_pre, "âge") ~ 1,
    TRUE ~ 0
  ),
  taux_rotation = case_when(
    str_detect(chiffre_pre, "rotation") ~ 1,
    TRUE ~ 0
  ),
  compte_banq = case_when(
    str_detect(chiffre_pre, "somme") ~ 1,
    TRUE ~ 0
  ),
  renta_m2 = case_when(
    str_detect(chiffre_pre, "rentabilité") ~ 1,
    TRUE ~ 0
  ),
  fonds_roule = case_when(
    str_detect(chiffre_pre, "besoin") ~ 1,
    TRUE ~ 0
  )
  )

#Difficultés économiques éprouvées
donnees_preparees$diff_eco_rec <- donnees_preparees$diff_eco %>%
  as.factor() %>%
  fct_recode(
    "1" = "Non",
    "0" = "Oui"
  ) %>%
  as.numeric()

###CATEGORISATION

#CA

donnees_preparees <- donnees_preparees %>%
  mutate(
    ca_rec_rec = case_when(
      ca_rec < 300000 ~ "Inférieur à 300k",
      ca_rec < 600000 & ca_rec >= 300000 ~ "300k à 599k",
      ca_rec < 1200000 & ca_rec >= 600000 ~ "600k à 1199k",
      ca_rec >= 1200000 ~ "1,2 M€ et plus",
      TRUE ~ NA_character_
    ),
    ca_rec_rec = factor(ca_rec_rec)
  ) %>%
  mutate(
    ca_rec_rec = fct_relevel(
      ca_rec_rec,
      "Inférieur à 300k",
      "300k à 599k",
      "600k à 1199k",
      "1,2 M€ et plus"
    )
  )

#Code postal
donnees_preparees <- donnees_preparees %>%
  mutate(code_dpt = case_when(
    postcode_clear_numeric == 5000 ~ "05",
    postcode_clear_numeric == 2200 ~ "02",
    postcode_clear_numeric == 20000 ~ "2A",
    postcode_clear_numeric == 3200 ~ "03",
    postcode_clear_numeric == 5100 ~ "05",
    postcode_clear_numeric == 5200 ~ "05",
    postcode_clear_numeric == 6300 ~ "06",
    postcode_clear_numeric == 6740 ~ "06",
    postcode_clear_numeric == 7000 ~ "07",
    postcode_clear_numeric == 8000 ~ "08",
    postcode_clear_numeric == 9110 ~ "09",
    postcode_clear_numeric == 9200 ~ "09",
    postcode_clear_numeric == 9270 ~ "09",
    TRUE ~ str_sub(as.character(postcode_clear_numeric), start = 1L, end = 2L)
  ))

departements_region <- read_csv("https://minio.lab.sspcloud.fr/juleslapeuf/departements-region.csv")

donnees_preparees <- left_join(donnees_preparees, departements_region, by = c("code_dpt" = "num_dep"))

# Age
donnees_preparees <- donnees_preparees %>%
  mutate(annee_reco_num = as.numeric(annee_reco)) %>%
  mutate(age_rev = 2023 - annee_reco_num)

donnees_preparees <- donnees_preparees %>%
  mutate(
    age_rev_rec = case_when(
      age_rev <= 30 ~ "30 ans et moins",
      age_rev <= 35 ~ "31 - 35 ans",
      age_rev <= 40 ~ "36 - 40 ans",
      age_rev <= 45 ~ "41 - 45 ans",
      age_rev <= 50 ~ "46 - 50 ans",
      age_rev <= 55 ~ "51 - 55 ans",
      age_rev <= 60 ~ "56 - 60 ans",
      age_rev > 60 ~ "61 ans et plus",
      TRUE ~ NA_character_
    ),
    age_rev_rec = factor(
      age_rev_rec,
      levels = c(
        "30 ans et moins",
        "31 - 35 ans",
        "36 - 40 ans",
        "41 - 45 ans",
        "46 - 50 ans",
        "51 - 55 ans",
        "56 - 60 ans",
        "61 ans et plus"
      )
    )
  )

#Nombre de salariés
donnees_preparees <- donnees_preparees %>%
  mutate(
    nb_salaries_ = case_when(
      nb_salaries == 0 ~ "Aucun salarié",
      nb_salaries >= 1 & nb_salaries <= 2 ~ "1 à 2 salariés",
      nb_salaries >= 3 & nb_salaries <= 4 ~ "3 à 4 salariés",
      nb_salaries >= 5 & nb_salaries <= 9 ~ "5 à 9 salariés",
      nb_salaries >= 10 ~ "10 et plus",
      TRUE ~ NA_character_
    ),
    nb_salaries_ = factor(
      nb_salaries_,
      levels = c(
        "Aucun salarié",
        "1 à 2 salariés",
        "3 à 4 salariés",
        "5 à 9 salariés",
        "10 et plus"
      )
    )
  )

#Taille
donnees_preparees <- donnees_preparees %>%
  mutate(surfco_ = case_when(
    taille_reco > 199 ~ "Plus de 200 m2",
    taille_reco <= 199 & taille_reco > 99 ~ "Entre 100 et 199 m2",
    taille_reco < 100  ~ "Moins de 100 m2",
    TRUE ~ NA_character_
  ),
  surfco_ = fct_relevel(surfco_,
                                "Moins de 100 m2",
                                "Entre 100 et 199 m2",
                                "Plus de 200 m2"))

#Rentabilité au m2
donnees_preparees <- donnees_preparees %>%
  mutate(rentabilité = ca_rec/taille_reco)


#Corrélation
cor(donnees_preparees$taille_reco, donnees_preparees$ca_rec, use = "pairwise.complete.obs")
cor(donnees_preparees$sitven, donnees_preparees$portasso, use = "pairwise.complete.obs")