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

# Colonnes à inclure dans la formule
colonnes_incluses <- c("flag_migration_baisse","flag_migration_hausse" ,"Upgrade_tel", "Downgrade_tel",
                       "Identif_tel", "Personnalisation","csp","cl_Reengament_type_woe","cl_Variation_offre_ini_woe",
                       "cl_anciennete_woe", "cl_Variabilité_vol_woe","cl_Variabilité_sms_woe","cl_age_woe","cl_duree_offre_woe",
                       "cl_taux_croissance_sms_woe","cl_taux_croissance_appel_woe")


# Générer la formule en utilisant les colonnes incluses
formule_modele_complet <- as.formula(paste("flag_resiliation ~ ",
                                           paste(colonnes_incluses, collapse = " + ")))

# Afficher la formule générée
print(formule_modele_complet)


rf <- randomForest(formula = formule_modele_complet,
                   data = apprentissage_1,
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

tx_erreur %>% 
  arrange(tx_erreur) %>% 
  head(1)



# Optimisation du nombre de variables sélectionnées à chaque coupure d'arbre : 8

tx_erreur <- NULL

for(nb_var in 1:37) {
  
  set.seed(123)
  
  rf <- randomForest(formula = formule_modele_complet,
                     data = apprentissage_1,
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

tx_erreur %>% 
  arrange(tx_erreur) %>% 
  head(1)


# Forêt optimisée

## Calcul du modèle

rf <- randomForest(formula = formule_modele_complet,
                   data = apprentissage_1,
                   ntree = 160,
                   mtry = 24,
                   importance = TRUE)


## Importance des variables

varImpPlot(rf, type = 1)


## Calcul des probabilités estimées

validation_rf <- validation_1 %>% 
  mutate(p_resiliation = predict(object = rf,
                                 newdata = validation_1,
                                 type = "prob")[, 2]) %>% 
  select(id_client, flag_resiliation, p_resiliation)

# Tri par ordre décroissant de la colonne P_resiliation
df_trie <- validation_rf %>% arrange(desc(p_resiliation))
df_trie

## Distribution des probabilités estimées

ggplot(validation_rf) +
  aes(x = p_resiliation) +
  geom_density()


## Performance du modèle (0.9138)

print(roc(validation_rf$flag_resiliation, validation_rf$p_resiliation)$auc)



# Fonction pour trouver les colonnes avec des niveaux de facteur différents entre deux dataframes
find_different_levels <- function(df1, df2) {
  different_columns <- vector("list", length(df1))
  for (i in seq_along(df1)) {
    if (is.factor(df1[[i]]) && is.factor(df2[[i]])) {
      levels1 <- levels(df1[[i]])
      levels2 <- levels(df2[[i]])
      if (!identical(levels1, levels2)) {
        different_columns[[i]] <- colnames(df1)[i]
      }
    }
  }
  different_columns <- Filter(Negate(is.null), different_columns)
  return(different_columns)
}

# Utilisation de la fonction pour trouver les colonnes avec des niveaux de facteur différents
colonnes_differentes <- find_different_levels(apprentissage_1, validation_1)

# Afficher les colonnes avec des niveaux de facteur différents
print(colonnes_differentes)


###

## Calcul des probabilités estimées

validation_rf <- base_telecom_2023 %>% 
  mutate(p_resiliation = predict(object = rf,
                                 newdata = base_telecom_2022,
                                 type = "prob")[, 2]) %>% 
  select(id_client, p_resiliation)

# Tri par ordre décroissant de la colonne P_resiliation
df_trie <- validation_rf %>% arrange(desc(p_resiliation))


# Sélectionner les 2000 premiers identifiants de clients
top_2000_ids <- head(df_trie$id_client, 2000)

# Créer un dataframe avec ces identifiants de clients
top_2000_df <- data.frame(id_client = top_2000_ids)

# Écrire les identifiants de clients dans un fichier texte sans les guillemets
write.table(top_2000_df, file = "C5_Daniel.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)




"Si aujourd'hui ma maison peut servir à permettre à impacter alors ce sera une grande fierté
pour moi

je ferai de mon mieux aussi pour être un bon hote,
"