#############################
### 5 - CIBLAGE SCORE V2  ###
### 5.2 - Forêt aléatoire ###
#############################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude


# Forêt étendue

formule_modele_complet <- as.formula(paste("flag_resiliation ~ ",
                                           paste(apprentissage_2 %>%
                                                   select(where(is.factor), - id_client, - flag_resiliation) %>% 
                                                   colnames(),
                                                 collapse = " + ")))

rf <- randomForest(formula = formule_modele_complet,
                   data = apprentissage_2,
                   ntree = 500)




# Optimisation du nombre d'arbres de la forêt : 160

tx_erreur = as.data.frame(rf$err.rate) %>% 
  rownames_to_column(var = "nb_arbres") %>% 
  mutate(nb_arbres = as.numeric(nb_arbres)) %>% 
  rename(tx_erreur = OOB) %>% 
  select(nb_arbres, tx_erreur)

ggplot(data = tx_erreur) +
  aes(x = nb_arbres, y = tx_erreur) +
  geom_line() +
  scale_x_continuous(breaks = seq(0, 500, by = 25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.01),
                     labels = scales::percent) +
  labs(x = "Nombre d'arbres",
       y = "Taux d'erreur") +
  theme_minimal()

tx_erreur %>% 
  arrange(tx_erreur) %>% 
  slice(1)


# Optimisation du nombre de variables sélectionnées à chaque coupure d'arbre : 8

tx_erreur <- NULL

for(nb_var in 1:37) {
  
  set.seed(123)
  
  rf <- randomForest(formula = formule_modele_complet,
                     data = apprentissage_2,
                     ntree = 160,
                     mtry = nb_var)
  
    tx_erreur <- c(tx_erreur, rf$err.rate[160, 1])

}

tx_erreur <- as.data.frame(tx_erreur) %>% 
  rownames_to_column(var = "nb_variables") %>% 
  mutate(nb_variables = as.numeric(nb_variables))

ggplot(data = tx_erreur) +
  aes(x = nb_variables, y = tx_erreur) +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 52, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.01),
                     labels = scales::percent) +
  labs(x = "Nombre de variables",
       y = "Taux d'erreur") +
  theme_minimal()

tx_erreur %>% 
  arrange(tx_erreur) %>% 
  slice(1)




# Forêt optimisée

## Calcul du modèle

rf <- randomForest(formula = formule_modele_complet,
                   data = apprentissage_2,
                   ntree = 160,
                   mtry = 8,
                   importance = TRUE)


## Importance des variables

varImpPlot(rf, type = 1)


## Calcul des probabilités estimées

validation_rf <- validation_2 %>% 
  mutate(p_resiliation = predict(object = rf,
                                 newdata = validation_2,
                                 type = "prob")[, 2]) %>% 
  select(id_client, flag_resiliation, p_resiliation)


## Distribution des probabilités estimées

ggplot(validation_rf) +
  aes(x = p_resiliation) +
  geom_density()


## Performance du modèle (0.9138)

print(roc(validation_rf$flag_resiliation, validation_rf$p_resiliation)$auc)
