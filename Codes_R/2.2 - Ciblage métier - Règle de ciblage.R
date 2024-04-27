##############################
### 2 - CIBLAGE METIER     ###
### 2.2 - Règle de ciblage ###
##############################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude




# Test d'une première règle de ciblage ----
# - Clients âgés entre 18 et 25 ans
# - Détenteurs d'un forfait 4H
# - Appelant en moyenne entre 3 heures et 5 heures par mois
# => 253 clients

ciblage_metier_1 <- base_2023 %>% 
  mutate(age = floor(time_length(interval(date_naissance, dmy("31/03/2023")), "years")),
         vol_appels_6m = rowMeans(pick(starts_with("vol_appels")), na.rm = TRUE) / 3600) %>% 
  filter(age >= 18 &
           age <= 25 &
           duree_offre == "4" &
           vol_appels_6m >= 3 &
           vol_appels_6m <= 5)




# Test d'une deuxième règle de ciblage ----
# - Clients âgés entre 18 et 30 ans,
# - Détenteurs d'un forfait de 4H ou moins,
# - Appelant en moyenne moins de 4 heures par mois
# => 2978 clients

ciblage_metier_2 <- base_2023 %>% 
  mutate(age = floor(time_length(interval(date_naissance, dmy("31/03/2023")), "years")),
         vol_appels_6m = rowMeans(pick(starts_with("vol_appels")), na.rm = TRUE) / 3600) %>% 
  filter(age >= 18 &
           age <= 30 &
           duree_offre %in% c("0.5", "1", "2", "3", "4") &
           vol_appels_6m <= 4)




# Ciblage final ----
# Sélection aléatoire de 2000 clients parmi les 2978 identifiés avec le deuxième test

set.seed(12345)
ciblage_metier <- ciblage_metier_2 %>% 
  slice_sample(n = 2000) %>% 
  select(id_client)




# Export pour le test de campagne (nom à personnaliser) ----

write.table(x = ciblage_metier,
            file = "Ciblages/C2_Kienner.txt",
            fileEncoding = "UTF-8",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            na = "")
