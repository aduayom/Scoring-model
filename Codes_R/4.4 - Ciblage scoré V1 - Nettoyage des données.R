###################################
### 4 - CIBLAGE SCORE V1        ###
### 4.4 - Nettoyage des données ###
###################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude




# Traitement global de la base ----

## % de valeurs manquantes total ----

pct_miss(base_1)




## Valeurs manquantes par variables ----

### Audit

skim(base_1)

pct_miss_var(base_1)
gg_miss_var(base_1, show_pct = TRUE)
vis_miss(base_1, warn_large_data = FALSE)

nb_vm <- miss_var_summary(base_1)




### Suppression des variables :
### - ayant plus de 50% de valeurs manquantes
###   => aucune variable concernée
### - n'ayant qu'une seule modalité
###   => aucune variable concernée
### - intégrées dans la variable à expliquer ou postérieure à la date de référence
###   => aucune variable concernée
### - décision métier
###   => code_postal, taille_ville, type_ville et revenu_moyen_ville

### Certaines variables sont à traiter à part :
### - lorsque les NA ne sont pas des anomalies, elles correspondent à un comportement précis
###   donc on peut les remplacer par une valeur sûre / maîtrisée
###   (dans l'idéal on peut le vérifier si elles dépendent de la valeur d'une autre variable)
###   => date_dernier_reengagement
### - lorsqu'elles peuvent servir à remplir des NA de variables conditionnelles
###   (on peut éventuellement les supprimer ensuite)
###   => aucune variable concernée
### - lorsqu'elles peuvent être utilisées pour créer une variable dérivée
###   (on peut éventuellement les supprimer ensuite)
###   => aucune variable concernée

### Liste des variables à traiter à part

var_na_a_part <- c("date_dernier_reengagement")


### Liste des variables à supprimer

var_a_supprimer <- nb_vm %>% 
  filter((pct_miss >= 50 |
            variable %in% c("code_postal", "taille_ville", "type_ville", "revenu_moyen_ville")) &
           ! variable %in% var_na_a_part) %>% 
  pull(variable)


### Suppression des variables

base_2 <- base_1 %>% 
  select(- all_of(var_a_supprimer))




## Valeurs manquantes par individus ----

### Audit (hors identifiant, variable à expliquer et variables avec un traitement spécial)

pct_miss_case(base_2 %>% select(- id_client, flag_resiliation, - all_of(var_na_a_part)))

nb_var <- ncol(base_2 %>% select(- id_client, flag_resiliation, - all_of(var_na_a_part)))

base_3 <- base_2 %>%
  add_n_miss(- id_client, - var_na_a_part) %>% 
  mutate(pct_miss_vars = round(n_miss_vars / nb_var * 100, 2) )

freq(base_3$n_miss_vars)
freq(base_3$pct_miss_vars)

ggplot(data = base_3) +
  aes(x = n_miss_vars) +
  geom_bar()


### Suppression des individus ayant plus de 50% de valeurs manquantes
### => aucun individu  concerné

base_4 <- base_3 %>% 
  filter(pct_miss_vars < 50) %>% 
  select(- n_miss_vars, - pct_miss_vars)






# Traitement variable par variable ----

## Exemple pour une variable quantitative : date_naissance (45 valeurs manquantes) ----

summary(base_4$date_naissance)


### Imputation par la médiane

test <- base_4 %>% 
  mutate(date_naissance = na.aggregate(date_naissance, FUN = median))

summary(base_4$date_naissance)
summary(test$date_naissance)
ggplot(data = base_4) + aes(x = date_naissance) + geom_density()
ggplot(data = test) + aes(x = date_naissance) + geom_density()


### Imputation par la médiane stratifiée par CSP

test <- base_4 %>% 
  mutate(date_naissance = na.aggregate(date_naissance, by = csp, FUN = median))

summary(base_4$date_naissance)
summary(test$date_naissance)
ggplot(data = base_4) + aes(x = date_naissance) + geom_density()
ggplot(data = test) + aes(x = date_naissance) + geom_density()


### Imputation par une estimation calculée à partir des voisins
### - hors identifiant et variable à expliquer
### - variables dates converties temporairement en numériques, puis reconverties en dates

test <- base_4 %>% 
  mutate(across(where(is.Date), as.numeric)) %>% 
  kNN(data = test,
      variable = "date_naissance",
      dist_var = colnames(base_4)[- c(1, 2)],
      k = 5,
      imp_var = FALSE) %>% 
  mutate(across(starts_with("date"), as.Date))

summary(base_4$date_naissance)
summary(test$date_naissance)
ggplot(data = base_4) + aes(x = date_naissance) + geom_density()
ggplot(data = test) + aes(x = date_naissance) + geom_density()


### Imputation par une valeur aléatoire permettant de respecter la distribution

indices_na <- which(is.na(base_4$date_naissance))
valeurs_ok <- base_4$date_naissance[which(! is.na(base_4$date_naissance))]
test <- base_4
set.seed(123)
valeurs_imputees <- sample(x = valeurs_ok,
                           size = length(indices_na),
                           replace = TRUE)
test$date_naissance[indices_na] <- valeurs_imputees

summary(base_4$date_naissance)
summary(test$date_naissance)
ggplot(data = base_4) + aes(x = date_naissance) + geom_density()
ggplot(data = test) + aes(x = date_naissance) + geom_density()




## Exemple pour une variable qualitative : sexe (6 valeurs manquantes) ----

freq(base_4$sexe)


### Imputation par une valeur "Inconnu"

test <- base_4 %>% 
  mutate(sexe = fct_na_value_to_level(sexe, level = "Inconnu"))

freq(base_4$sexe)
freq(test$sexe)
levels(test$sexe)


### Imputation par le mode

test <- base_4
test$sexe[is.na(test$sexe)] <- names(sort(table(base_4$sexe), decreasing = TRUE))[1]

freq(base_4$sexe)
freq(test$sexe)


### Imputation par une estimation calculée à partir des voisins
### - hors identifiant et variable à expliquer

test <- kNN(data = base_4,
            variable = "sexe",
            dist_var = colnames(base_4)[- c(1, 2)],
            k = 5,
            imp_var = FALSE)

freq(base_4$sexe)
freq(test$sexe)


### Imputation par une valeur aléatoire respectant la répartition des modalités

indices_na <- which(is.na(base_4$sexe))
valeurs_ok <- base_4$sexe[which(! is.na(base_4$sexe))]
test <- base_4
set.seed(123)
valeurs_imputees <- sample(x = valeurs_ok,
                           size = length(indices_na),
                           replace = TRUE)
test$sexe[indices_na] <- valeurs_imputees

freq(base_4$sexe)
freq(test$sexe)
