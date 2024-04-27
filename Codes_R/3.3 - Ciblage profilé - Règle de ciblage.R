##############################
### 3 - CIBLAGE PROFILE    ###
### 3.3 - Règle de ciblage ###
##############################


# Identification de la règle de ciblage ----

## Construction des variables sur les bases 2022 et 2023
## (création de nouvelles variables pour le découpage en décile au lieu de remplacements)

var_qt <- base_2022_2 %>%
  select(where(is.numeric)) %>% 
  colnames()

base_2022_ciblage <- base_2022 %>% 
  mutate (age = floor(time_length(interval(date_naissance, dmy("31/12/2022")), "years")),
          anciennete_client = floor(time_length(interval(date_activation, dmy("31/12/2022")), "months")),
          duree_engagement = floor(time_length(interval(dmy("31/12/2022"), date_fin_engagement), "months")),
          anciennete_reengagement = floor(time_length(interval(date_dernier_reengagement, dmy("31/12/2022")), "months"))) %>% 
  mutate(across(all_of(var_qt),
                .fns = list(cl = ~ ntile(.x, 10)),
                .names = "{fn}_{col}"),
         across(starts_with("cl_"), as.factor))

base_2023_ciblage <- base_2023 %>% 
  mutate (age = floor(time_length(interval(date_naissance, dmy("31/03/2023")), "years")),
          anciennete_client = floor(time_length(interval(date_activation, dmy("31/03/2023")), "months")),
          duree_engagement = floor(time_length(interval(dmy("31/03/2023"), date_fin_engagement), "months")),
          anciennete_reengagement = floor(time_length(interval(date_dernier_reengagement, dmy("31/03/2023")), "months"))) %>% 
  mutate(across(all_of(var_qt),
                .fns = list(cl = ~ ntile(.x, 10)),
                .names = "{fn}_{col}"),
         across(starts_with("cl_"), as.factor))




## Construction d'une table contenant les résiliés

base_resil_2022 <- base_2022 %>% 
  filter(flag_resiliation == 1) %>% 
  select(id_client)




## Construction d'une fonction d'évaluation de la performance d'une règle de ciblage
## Le nombre de clients à sélectionner est connu sur 2023 (2000 clients), mais la base de 2022 est différente
## Pour restreindre à un volume comparable aux 2000 clients de la base 2023 j'applique une règle de 3 :
## - 44529 clients dans la base 2022
## - 22528 clients dans la base 2023
## => 2000 clients ciblés en 2023 représentent ( 2000 * 44529 / 22528 ) = 3953 clients ciblés en 2022
## => chaque règle de ciblage sera appliquée sur 2022 en ne prenant que 3953 clients afin d'estimer un taux de résiliation non biaisé
##    et pouvoir ensuite appliquer ce taux de résiliation sur les 2000 clients à cibler en 2023

nb_cible_2022 <- round(2000 * nrow(base_2022_ciblage) / nrow(base_2023_ciblage))


perf <- function(regle) {
  
  cible_2022 <- base_2022_ciblage %>% 
    filter({{regle}}) %>% 
    arrange(desc(date_fin_engagement)) %>% 
    slice(1:nb_cible_2022)
  
  nb_resil_2022 <- nrow(cible_2022 %>% 
                          inner_join(base_resil_2022,
                                     by = "id_client"))
  
  tx_resil_2022 <- nb_resil_2022 / nrow(cible_2022)
  
  nb_clients_2023 <- nrow(base_2023_ciblage %>% 
                            filter({{regle}}))
  
  cible_2023 <- base_2023_ciblage %>% 
    filter({{regle}}) %>% 
    arrange(desc(date_fin_engagement)) %>% 
    slice(1:2000)
  
  nb_resil_2023 <- nrow(cible_2023) * tx_resil_2022
  
  cat("\n")
  cat("Nombre de clients 2023 ciblés   : ", nb_clients_2023, "\n", sep = "")
  cat("Nombre de résiliés 2023 estimés : ", round(nb_resil_2023), " (", round(tx_resil_2022*100,1), "%)", "\n", sep = "")
  cat("\n")
  
}

perf(nb_reengagements == 0) # 10682 / 197 (9,8%)
perf(nb_reengagements == 0 & cl_duree_engagement == 1 & duree_offre == 2) # 1035 / 736 (71,1%)
