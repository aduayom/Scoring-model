###################################################
### 3 - CIBLAGE PROFILE                         ###
### 3.2 - Généralisation de l'analyse de profil ###
###################################################


# Calcul des mesures de liaison pour toutes les variables quantitatives ----

## Calcul des durées à partir des dates

base_2022_2 <- base_2022 %>% 
  mutate (age = floor(time_length(interval(date_naissance, dmy("31/12/2022")), "years")),
          anciennete_client = floor(time_length(interval(date_activation, dmy("31/12/2022")), "months")),
          duree_engagement = floor(time_length(interval(dmy("31/12/2022"), date_fin_engagement), "months")),
          anciennete_reengagement = floor(time_length(interval(date_dernier_reengagement, dmy("31/12/2022")), "months")))




## Liste des variables à analyser

var_qt <- base_2022_2 %>%
  select(where(is.numeric)) %>% 
  colnames()




## Mesures d'intensité du lien de chaque variable avec la variable à expliquer
## - Statistique du test de Student
## - Statistique du test de Wilcoxon
## - Distance de Kolmogorov-Smirnov
## - Rapport de corrélation


student <- sapply(base_2022_2[var_qt],
                  function(x) { abs(t.test(x ~ base_2022_2$flag_resiliation)$statistic) })

wilcoxon <- sapply(base_2022_2[var_qt],
                   function(x) { wilcox.test(x ~ base_2022_2$flag_resiliation)$statistic })

ks <- sapply(base_2022_2[var_qt],
             function(x) { ks.test(x ~ base_2022_2$flag_resiliation)$statistic })

eta2 <- sapply(base_2022_2[var_qt],
               function(x) { eta2(x, base_2022_2$flag_resiliation) })




## Regroupement des mesures issues de chaque test

liens_var_qt <- data.frame(eta2, ks, wilcoxon, student) %>% 
  rownames_to_column(var = "variable") %>% 
  arrange(desc(ks))




## Représentation graphique de la distance de Kolmogorov-Smirnov

ggplot(liens_var_qt) +
  aes(x = ks, y = reorder(variable, ks)) +
  geom_col(fill = "steelblue4") +
  labs(x = "Distance de Kolmogorov-Smirnov", y = "Variable") +
  theme_minimal()






# Calcul des mesures de liaison pour toutes les variables qualitatives ----

## Liste des variables à analyser

var_ql <- base_2022_2 %>%
  select(where(is.factor), - flag_resiliation) %>% 
  colnames()




## Mesures d'intensité du lien de chaque variable avec la variable à expliquer
## - V de Cramer
## - T de Tshuprow

cramer <- sapply(base_2022_2[var_ql],
                 function(x) { CramerV(x, base_2022_2$flag_resiliation) })

tschuprow <- sapply(base_2022_2[var_ql],
                    function (x) { TschuprowT(x, base_2022_2$flag_resiliation) })




## Regroupement des mesures issues de chaque test

liens_var_ql <- data.frame(cramer, tschuprow) %>% 
  rownames_to_column(var = "variable") %>% 
  arrange(desc(tschuprow))




## Représentation graphique du T de Tshuprow

ggplot(liens_var_ql) +
  aes(x = tschuprow, y = reorder(variable, tschuprow)) +
  geom_col(fill = "steelblue4") +
  labs(x = "T de Tshuprow", y = "Variable") +
  theme_minimal()






# Analyse simultanée des variables quantitatives et qualitatives ----

## Découpage des variables quantitatives en déciles (avec les NA mis dans une modalité à part)

base_2022_3 <- base_2022_2 %>% 
  mutate(across(all_of(var_qt), ~ ntile(.x, n = 10)),
         across(all_of(var_qt), as.factor))

var_ql_all <- base_2022_3 %>%
  select(where(is.factor), - flag_resiliation) %>% 
  colnames()




## Mesures d'intensité du lien de chaque variable avec la variable à expliquer
## - V de Cramer
## - T de Tshuprow

cramer <- sapply(base_2022_3[var_ql_all],
                 function(x) { CramerV(x, base_2022_2$flag_resiliation) })

tschuprow <- sapply(base_2022_3[var_ql_all],
                    function (x) { TschuprowT(x, base_2022_2$flag_resiliation) })




## Regroupement des mesures issues de chaque test

liens_var_ql_all <- data.frame(cramer, tschuprow) %>% 
  rownames_to_column(var = "variable") %>% 
  arrange(desc(tschuprow))




## Représentation graphique du V de Cramer et du T de Tshuprow

ggplot(liens_var_ql_all) +
  aes(x = tschuprow, y = reorder(variable, cramer)) +
  geom_col(fill = "steelblue4") +
  labs(x = "V de Cramer", y = "Variable") +
  theme_minimal()

ggplot(liens_var_ql_all) +
  aes(x = tschuprow, y = reorder(variable, tschuprow)) +
  geom_col(fill = "steelblue4") +
  labs(x = "T de Tshuprow", y = "Variable") +
  theme_minimal()




##  Caractérisation fine des résiliés

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
