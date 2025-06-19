

hist(donnees_preparees$inq_av)
hist(donnees_preparees$inq_p)
hist(donnees_preparees$inq_ap)

df_inq <- donnees_preparees %>%
  select(ca_, surfco_, age_rev_rec, annee_lib_reco, nb_salaries_, inq_av, inq_p, inq_ap)

names(donnees_preparees)

df_inq <- df_inq %>%
  mutate(ap_p = inq_ap - inq_p,
         p_av = inq_p - inq_av,
         ap_av = inq_ap - inq_av)

hist(df_inq$ap_p)
hist(df_inq$ap_av)
hist(df_inq$p_av)