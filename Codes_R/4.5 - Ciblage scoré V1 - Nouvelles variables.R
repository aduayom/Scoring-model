#################################
### 4 - CIBLAGE SCORE V1      ###
### 4.5 - Nouvelles variables ###
#################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude




# Création de quelques variables

base_6 <- base_5 %>% 
  mutate(age = floor(time_length(interval(date_naissance, dmy("31/12/2022")), "year")),
         anciennete = floor(time_length(interval(date_activation, dmy("31/12/2022")), "year")),
         duree_engagement = floor(time_length(interval(dmy("31/12/2022"), date_fin_engagement), "month")),
         anciennete_reengagement = floor(time_length(interval(date_dernier_reengagement, dmy("31/12/2022")), "year")),
         vol_appels_6m = rowMeans(pick(starts_with("vol_appels")), na.rm = TRUE)) %>% 
  select(- starts_with("date"))
