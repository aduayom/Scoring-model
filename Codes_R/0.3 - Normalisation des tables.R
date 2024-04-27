######################################
### 0.3 - NORMALISATION DES TABLES ###
######################################


# Identification des variables dates, qualitatives et quantitatives

var_dates <- base_2023 %>% 
  select(starts_with("date")) %>% 
  colnames()

var_ql <- base_2023 %>%
  select(where(is.character),
         starts_with("flag"),
         duree_offre_init, duree_offre, nb_migrations, nb_services, nb_reengagements,
         - starts_with("date"),
         - id_client, - code_postal) %>% 
  colnames()

var_qt <- base_2023 %>%
  select(- all_of(var_ql), - all_of(var_dates), - id_client, - code_postal) %>% 
  colnames()


# Conversion des colonnes dates, facteurs et entiers

base_2023 <- base_2023 %>% 
  mutate(across(all_of(var_dates), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(all_of(var_ql), as.factor),
         across(where(is.integer), as.numeric))

base_2022 <- base_2022 %>% 
  mutate(across(all_of(var_dates), ~ as.Date(.x, format = "%d/%m/%Y")),
         across(all_of(c(var_ql, "flag_resiliation")), as.factor),
         across(where(is.integer), as.numeric))
