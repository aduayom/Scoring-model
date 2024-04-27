###############################
### 5 - CIBLAGE SCORE V2    ###
### 5.1 - Arbre de décision ###
###############################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude


# Arbre maximal

## Calcul du modèle

formule_modele_complet <- as.formula(paste("flag_resiliation ~ ",
                                           paste(apprentissage_2 %>%
                                                   select(where(is.factor), - id_client, - flag_resiliation) %>% 
                                                   colnames(),
                                                 collapse = " + ")))

arbre_max <- rpart(formula = formule_modele_complet,
                   data = apprentissage_2,
                   method = "class",
                   cp = -Inf)


## Représentations de l'arbre

arbre_max
prp(x = arbre_max)
rpart.plot(x = arbre_max)




# Elagage de l'arbre

## Représentation du taux de mal classés

plotcp(arbre_max)


## Sélection de la constante avec le taux d'erreur minimum

printcp(arbre_max)


## Construction du sous-arbre optimal en utilisant la constante avec le taux d'erreur minimum

cp_min <- arbre_max$cptable %>% 
  as.data.frame() %>% 
  arrange(xerror) %>% 
  slice(1) %>% 
  select(CP) %>% 
  pull()

arbre <- rpart(formula = formule_modele_complet,
               data = apprentissage_2,
               method = "class",
               cp = cp_min)


## Visualisation de l'arbre

rpart.plot(x = arbre, cex = 0.7)


## Importance des variables

importance <- data.frame(arbre$variable.importance) %>% 
  rownames_to_column(var = "variable") %>% 
  rename(importance = arbre.variable.importance)

ggplot(data = importance) +
  aes(x = importance, y = fct_reorder(variable, importance)) +
  geom_col(fill = "steelblue4") +
  labs(title = "Importance des variables", x = NULL, y = NULL) +
  theme_minimal()


## Calcul des probabilités estimées

validation_arbre <- validation_2 %>% 
  mutate(p_resiliation = predict(object = arbre,
                                 newdata = validation_2,
                                 type = "prob")[, 2]) %>% 
  select(id_client, flag_resiliation, p_resiliation)


## Distribution des probabilités estimées

ggplot(validation_arbre) +
  aes(x = p_resiliation) +
  geom_density()


## Performance du modèle (0.8426)

print(roc(validation_arbre$flag_resiliation, validation_arbre$p_resiliation)$auc)
