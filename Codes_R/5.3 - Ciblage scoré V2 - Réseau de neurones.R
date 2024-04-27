################################
### 5 - CIBLAGE SCORE V2     ###
### 5.3 - Réseau de neurones ###
################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude


# Réseau de neurones avec 1 couche cachée composée de 5 neurones

formule_modele_complet <- as.formula(paste("flag_resiliation ~ ",
                                           paste(apprentissage_2 %>%
                                                   select(where(is.factor), - id_client, - flag_resiliation) %>% 
                                                   colnames(),
                                                 collapse = " + ")))

set.seed(123)

rn <- nnet(formula = formule_modele_complet,
           data = apprentissage_2,
           size = 5,
           maxit = 10000)


# Représentation du réseau

plotnet(rn)


# Importance des variables

garson(rn) + coord_flip()
olden(rn) + coord_flip()


# Calcul des probabilités estimées

validation_rn <- validation_2 %>% 
  mutate(p_resiliation = predict(object = rn,
                                 newdata = validation_2,
                                 type = "raw")[, 1]) %>% 
  select(id_client, flag_resiliation, p_resiliation)


# Distribution des probabilités estimées

ggplot(validation_rn) +
  aes(p_resiliation) +
  geom_density()


# Performance du modèle (0.8831)

print(roc(validation_rn$flag_resiliation, validation_rn$p_resiliation)$auc)
