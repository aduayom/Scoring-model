#################################
### 4 - CIBLAGE SCORE V1      ###
### 4.1 - Population éligible ###
#################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude




# Hypothèses
# - exclure les mineurs
# - exclure les personnes âgées

base_pop_eligible <- base_2022 %>% 
  mutate(age =  floor(time_length(interval(date_naissance, dmy("31/12/2022")), "years")))




# Analyse de l'âge

freq(base_pop_eligible$age, cum = TRUE)
skim(base_pop_eligible$age)


nrow(base_pop_eligible %>% filter(age < 18))


nrow(base_pop_eligible %>% filter(age >= 80))
nrow(base_pop_eligible %>% filter(age >= 80)) / nrow(base_pop_eligible)

nrow(base_pop_eligible %>% filter(age >= 80 & flag_resiliation == 1)) / nrow(base_pop_eligible %>% filter(age >= 80))
nrow(base_pop_eligible %>% filter(flag_resiliation == 1)) / nrow(base_pop_eligible)




# Commentaires
# - 45 valeurs manquantes (à traiter plus tard)
# - aucun client de moins de 18 ans (donc au final pas de décision à prendre)
# - 118 clients de 80 ans et plus, soit 0,3% de la base




# Décision
# Les clients trop âges sont retirés des campagnes, on fixe le seuil à 80 ans
# (même si leur taux de résiliation est plus élevé que la moyenne : 25,4% vs 18,1%)

base_1 <- base_2022 %>% 
  filter(floor(time_length(interval(date_naissance, dmy("31/12/2022")), "years")) < 80 |
           is.na(date_naissance))


# 44411 clients
