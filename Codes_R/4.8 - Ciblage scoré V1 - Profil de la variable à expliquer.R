###############################################
### 4 - CIBLAGE SCORE V1                    ###
### 4.8 - Profil de la variable à expliquer ###
###############################################


# Identification des variables qualitatives : toutes les variables facteurs hors identifiant et cible

var_ql <- apprentissage_2 %>%
  select(where(is.factor), - id_client, - flag_resiliation) %>% 
  colnames()




# Mesures d'intensité du lien de chaque variable avec la variable à expliquer ----

## Calcul du T de Tshuprow

tschuprow <- sapply(apprentissage_2[var_ql],
                    function (x) { TschuprowT(x, apprentissage_2$flag_resiliation) })


## Regroupement des mesures

liens_var_ql <- data.frame(tschuprow) %>% 
  rownames_to_column(var = "variable") %>% 
  arrange(desc(tschuprow))


## Représentation graphique du T de Tshuprow

ggplot(liens_var_ql) +
  aes(x = tschuprow, y = reorder(variable, tschuprow)) +
  geom_col(fill = "steelblue4") +
  labs(x = "T de Tshuprow", y = "Variable") +
  theme_minimal()




# Caractérisation fine des résiliés ----

profil_resilies <-  as.data.frame(round(catdes(base_2022_3[, c("flag_resiliation", var_ql_all)],
                                               1)$category$"1",
                                        1)) %>% 
  rownames_to_column(var = "indicateurs")

ggplot(profil_resilies %>% 
         slice(1:30)) +
  aes(y = reorder(indicateurs, v.test), x = v.test) +
  geom_col(fill = "steelblue4") +
  ylab("Indicateurs") +
  xlab("Valeur test") +
  theme_minimal()
