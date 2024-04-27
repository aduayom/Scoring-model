validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_duree_engagement) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_duree_engagement_woe = duree_engagement.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_Variation_offre_ini) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variation_offre_ini_woe = Variation_offre_ini.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_anciennete) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_anciennete_woe = anciennete.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_Variabilité_vol) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variabilité_vol_woe = Variabilité_vol.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_Variabilité_sms) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variabilité_sms_woe = Variabilité_sms.binned)

validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_age) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_age_woe = age.binned)

validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_duree_offre) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_duree_offre_woe = duree_offre.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_Reengament_type) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Reengament_type_woe = Reengament_type.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_Dead_line_engagement) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Dead_line_engagement_woe = Dead_line_engagement.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_taux_croissance_appel) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_taux_croissance_appel_woe = taux_croissance_appel.binned)


validation_1 <- woe.binning.deploy(df = validation_1,
                                      binning = woe_taux_croissance_sms) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_taux_croissance_sms_woe = taux_croissance_sms.binned)




colonnes <- colnames(apprentissage_1)
print(colonnes)

### Calcul du modèle

liste_var <- c("flag_migration_hausse", "Upgrade_tel", "Downgrade_tel", "Identif_tel", "Personnalisation","csp",
               "cl_Reengament_type_woe","cl_anciennete_woe", "cl_Variabilité_vol_woe","cl_age_woe","cl_duree_offre_woe")

formule <- as.formula(paste("flag_resiliation ~ ", paste(liste_var, collapse = " + ")))

rl <- glm(formula = formule,
          data = apprentissage_1,
          family = "binomial")


### Test de significativité globale du modèle (test du rapport de vraisemblance)

pchisq(q = rl$null.deviance - rl$deviance,
       df = rl$df.null - rl$df.residual,
       lower.tail = FALSE)

### R² de McFadden (<=> R² en régression linéaire)

1 - (rl$deviance/(-2)) / (rl$null.deviance/(-2))

