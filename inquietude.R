df_inq <- donnees_preparees %>%
  select(ca_rec_rec, surfco_, age_rev_rec, annee_lib_reco, nb_salaries_, inq_av, inq_p, inq_ap)

library(ggplot2)
library(dplyr)
library(tidyr)

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

moyennes_ca <- df_inq %>%
  group_by(ca_rec_rec) %>% 
  summarise(
    Avant = mean(inq_av, na.rm = TRUE),
    Pendant = mean(inq_p, na.rm = TRUE),
    Après = mean(inq_ap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(Avant, Pendant, Après),
    names_to = "Période",
    values_to = "Moyenne"
  )

moyennes_ca$Période <- factor(
  moyennes_ca$Période,
  levels = c("Avant", "Pendant", "Après")
)

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
    title = "Évolution des scores INQ selon le chiffre d'affaires",
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
