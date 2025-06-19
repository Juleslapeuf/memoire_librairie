#Version avec seulement les départements représentés

doc_carto <- donnees_preparees[, c("codepostal", "type", "etiq", "liseuse", "gpdiscu", "sitven", "portasso", "maillist", "obslib", "bibnum", "ressoc", "ressocspe", "logspec", "surfco_", "nb_salaries_", "ca_", "taux_retour", "pass_cais", "ca_semaine", "remise", "panier_moy", "age_stock", "taux_rotation", "compte_banq", "renta_m2", "fonds_roule")]
doc_carto <- doc_carto %>%
  filter(complete.cases(across(-c("codepostal", "type"))))

doc_carto <- doc_carto %>%
  select(1)

code_postaux <- doc_carto %>%
  mutate(postcode_clear = case_when(
    codepostal == "4 librairies 81500+81600+81300+ 81000" ~ "81000",
    codepostal == "1000 Bruxelles"~ "99131",
    codepostal == "29" ~ "29000",
    codepostal == "33" ~ "33000",
    codepostal == "45" ~ "45000",
    codepostal == "75" ~ "75001",
    TRUE ~ codepostal))

code_postaux <- code_postaux %>%
  mutate(postcode_clear_numeric = as.numeric(postcode_clear))

code_postaux <- code_postaux %>%
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

nb <- code_postaux %>%
  count(code_dpt, name = "frq")%>%
  filter(!is.na(code_dpt))

df_departements <- read.csv("~/Documents/M1_R/Memoire/carto/communes-departement-region.csv")%>%
  select(3, 6, 7, 15)%>%
  distinct(code_postal, .keep_all = TRUE)

df_final <- left_join(nb, code_postaux, by = "code_dpt")

df_carto <- left_join(df_final, df_departements, by = c("postcode_clear_numeric" = "code_postal")) %>%
  select(1, 2, 5:8)

france <- st_read("~/Documents/M1_R/Memoire/carto/contour-des-departements.geojson",
                  options = "ENCODING=WINDOWS-1252")

france_regions <- st_read("~/Documents/M1_R/Memoire/carto/regions.geojson",
                          options = "ENCODING=WINDOWS-1252") %>%
  filter(!(code %in% c("01", "02", "03", "04", "06")))

df_final <- left_join(france, df_carto, by = c("code" = "code_dpt"))

plot(df_final["frq"])


p <- ggplot() +
  geom_sf(data = df_final, aes(fill = frq), color = "black") +
  geom_sf(data = france_regions, fill = NA, color = "red") +
  scale_fill_viridis_c(option = "turbo", name = "Fréquence") +  # Custom color scale
  theme_minimal() +
  theme(
    legend.position = "right",  # Position of the legend
    legend.title = element_text(size = 12, face = "bold"),  # Legend title style
    legend.text = element_text(size = 10)  # Legend text style
  ) +
  labs(
    title = "Fréquence de réponses par département",
    subtitle = "En rouge les frontières des régions et le nombre de répondants par région et en noir des départements",
    caption = "Source: Questionnaire d'enquête"
  )


centroids <- st_centroid(france_regions)

print(nb_regions)

nom_regions <- c("Auvergne-Rhône-Alpes", "Bourgogne-Franche-Comté", "Bretagne", "Centre-Val de Loire",
                   "Corse", "Dom", "Grand Est", "Hauts-de-France", "Île-de-France", "Normandie", "Nouvelle-Aquitaine",
                   "Occitanie", "Pays de la Loire", "Provence-Alpes-Côte d'Azur")
frq_regions <- c(30, 8, 17, 3, 1, 16, 4, 45, 14, 47, 39, 16, 17)

nb_regions_df <- data.frame(nom_regions, frq_regions)

frq_regions_ursaf <- c(316, 94, 125, 67, 11, 64, 151, 136, 590, 96, 208, 215, 110, 189)
prop_regions_ursaf <- c(13.3, 4.0, 5.3, 2.8, 0.5, 2.7, 6.4, 5.7, 24.9, 4.0, 8.8, 9.1, 4.6, 8.0)

nb_regions_ursaf <- data.frame(nom_regions, frq_regions_ursaf, prop_regions_ursaf)

centroids_with_labels <- centroids %>%
  left_join(nb_regions_df, by = c("nom" = "nom_regions"))

pq <- p + 
  geom_sf_text(data = centroids_with_labels, aes(label = frq_regions), color = "red", size = 5)

pq

## A l'échelle des régions
nb_regions <- df_final %>%
  count(nom_region, name = "frq")


nb_regions <- as.data.frame(nb_regions)
nb_regions <- nb_regions %>%
  select(1:2)

france_regions <- st_read("~/Documents/M1_R/Memoire/carto/regions.geojson",
                          options = "ENCODING=WINDOWS-1252")

df_final_regions <- left_join(france_regions, nb_regions, by = c("nom" = "nom_region"))

plot(df_final_regions["frq"])