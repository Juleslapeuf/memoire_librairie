library(ggplot2)
library(dplyr)
library(tidyr)

df_inq <- donnees_preparees %>%
  select(ca_rec_rec, surfco_, age_rev_rec, annee_lib_reco, nb_salaries_, inq_av, inq_p, inq_ap, diff_eco)

# Calcul des moyennes
moyennes <- df_inq %>%
  summarise(
    `Avant` = mean(inq_av, na.rm = TRUE),
    `Pandémie` = mean(inq_p, na.rm = TRUE),
    `Après` = mean(inq_ap, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes$Période <- factor(
  moyennes$Période,
  levels = c("Avant", "Pandémie", "Après")
)

# Graphique
ggplot(moyennes, aes(x = Période, y = Moyenne)) +
  geom_hline(
    yintercept = 5,
    linetype = "dashed",
    color = "grey75",
    linewidth = 0.7
  ) +
  geom_point(
    color = "#315C72",
    size = 6
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Moyenne)),
    vjust = -1.3,
    size = 5,
    fontface = "bold",
    color = "#315C72"
  ) +
  scale_y_continuous(
    limits = c(1, 10),
    breaks = 1:10
  ) +
  labs(
    title = "Évolution moyenne des scores d'INQ",
    subtitle = "Comparaison des scores avant, pendant et après",
    x = NULL,
    y = "Score moyen (échelle de 1 à 10)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 17,
      color = "#263746"
    ),
    plot.subtitle = element_text(
      color = "grey45",
      size = 11
    ),
    axis.text = element_text(
      color = "#404040"
    ),
    axis.title.y = element_text(
      margin = margin(r = 12)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title.position = "plot"
  )

df_inq_ca <- donnees_preparees %>%
  drop_na(ca_rec_rec)

# MOYENNE PAR CHIFFRE D'AFFAIRES
moyennes_ca <- df_inq_ca %>%
  group_by(ca_rec_rec) %>% 
  summarise(
    Avant = mean(inq_av, na.rm = TRUE),
    Pandémie = mean(inq_p, na.rm = TRUE),
    Après = mean(inq_ap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Avant, Pandémie, Après),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes_ca$Période <- factor(
  moyennes_ca$Période,
  levels = c("Avant", "Pandémie", "Après")
)

ggplot(moyennes_ca,
       aes(x = Période, y = Moyenne,
           color = ca_rec_rec,
           group = ca_rec_rec)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(
    title = "Évolution des scores d'inquiétude selon le chiffre d'affaires",
    x = NULL,
    y = "Score moyen INQ",
    color = "Chiffre d'affaires"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# MOYENNE PAR NOMBRE DE SALARIES
df_inq_salaries <- donnees_preparees %>%
  drop_na(nb_salaries_)

moyennes_salaries <- df_inq_salaries %>%
  group_by(nb_salaries_) %>% 
  summarise(
    Avant = mean(inq_av, na.rm = TRUE),
    Pandémie = mean(inq_p, na.rm = TRUE),
    Après = mean(inq_ap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Avant, Pandémie, Après),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes_salaries$Période <- factor(
  moyennes_salaries$Période,
  levels = c("Avant", "Pandémie", "Après")
)

ggplot(moyennes_salaries,
       aes(x = Période, y = Moyenne,
           color = nb_salaries_,
           group = nb_salaries_)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(
    title = "Évolution des scores d'inquiétude selon le nombre de salariés",
    x = NULL,
    y = "Score moyen INQ",
    color = "Nombre de salariés"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# MOYENNE PAR L'AGE

df_inq_age <- donnees_preparees %>%
  drop_na(age_rev_rec)

moyennes_age <- df_inq_age %>%
  group_by(age_rev_rec) %>% 
  summarise(
    Avant = mean(inq_av, na.rm = TRUE),
    Pandémie = mean(inq_p, na.rm = TRUE),
    Après = mean(inq_ap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Avant, Pandémie, Après),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes_age$Période <- factor(
  moyennes_age$Période,
  levels = c("Avant", "Pandémie", "Après")
)

ggplot(moyennes_age,
       aes(x = Période, y = Moyenne,
           color = age_rev_rec,
           group = age_rev_rec)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(
    title = "Évolution des scores d'inquiétude selon l'âge",
    x = NULL,
    y = "Score moyen INQ",
    color = "Âge gérant.e"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# MOYENNE PAR DIFF_ECO

df_inq_diff_eco <- donnees_preparees %>%
  drop_na(diff_eco)

moyennes_diff_eco <- df_inq_diff_eco %>%
  group_by(diff_eco) %>% 
  summarise(
    Avant = mean(inq_av, na.rm = TRUE),
    Pandémie = mean(inq_p, na.rm = TRUE),
    Après = mean(inq_ap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Avant, Pandémie, Après),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes_diff_eco$Période <- factor(
  moyennes_diff_eco$Période,
  levels = c("Avant", "Pandémie", "Après")
)

ggplot(moyennes_diff_eco,
       aes(x = Période, y = Moyenne,
           color = diff_eco,
           group = diff_eco)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.5) +
  scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
  labs(
    title = "Évolution des scores d'inquiétude selon que la lib a déja connu des diff éco",
    x = NULL,
    y = "Score moyen INQ",
    color = "Difficultés économiques"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
