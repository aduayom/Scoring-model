###################################
### 4 - CIBLAGE SCORE V1        ###
### 4.9 - Régression logistique ###
###################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude



# Exemple de modélisation par régression logistique

## Modèle 1

### Calcul du modèle

liste_var <- c("cl_age", "cl_csp", "cl_duree_engagement", "flag_reengagement", "cl_duree_offre",
               "cl_nb_migrations", "flag_migration_hausse", "flag_migration_baisse", "cl_vol_appels_6m")

formule <- as.formula(paste("flag_resiliation ~ ", paste(liste_var, collapse = " + ")))

rl <- glm(formula = formule,
          data = apprentissage_2,
          family = "binomial")


### Test de significativité globale du modèle (test du rapport de vraisemblance)

pchisq(q = rl$null.deviance - rl$deviance,
       df = rl$df.null - rl$df.residual,
       lower.tail = FALSE)


### R² de McFadden (<=> R² en régression linéaire)

1 - (rl$deviance/(-2)) / (rl$null.deviance/(-2))


### Simplification du modèle : utilité de chaque variable

Anova(rl, type = 3)




## Modèle 2

### Recalcul du modèle sans les variables non significatives

liste_var <- c("cl_age", "cl_csp", "cl_duree_engagement", "flag_reengagement", "cl_duree_offre",
               "cl_nb_migrations", "flag_migration_baisse", "cl_vol_appels_6m")

formule <- as.formula(paste("flag_resiliation ~ ", paste(liste_var, collapse = " + ")))

rl <- glm(formula = formule,
          data = apprentissage_2,
          family = "binomial")


### Test de significativité globale du modèle (test du rapport de vraisemblance)

pchisq(q = rl$null.deviance - rl$deviance,
       df = rl$df.null - rl$df.residual,
       lower.tail = FALSE)


### R² de McFadden (<=> R² en régression linéaire)

1 - (rl$deviance/(-2)) / (rl$null.deviance/(-2))


### Simplification du modèle : utilité de chaque variable

Anova(rl, type = 3)




## Résultats détaillés du modèle

summary(rl)




## Simplification du modèle : utilité de chaque modalité

### Analyse des modalités de la variable cl_age

pairs(emmeans(object = rl, spec = "cl_age"))


### Analyse des modalités de la variable cl_csp

pairs(emmeans(object = rl, spec = "cl_csp"))


### Analyse des modalités de la variable cl_duree_engagement

pairs(emmeans(object = rl, spec = "cl_duree_engagement"))


### Analyse des modalités de la variable cl_duree_offre

pairs(emmeans(object = rl, spec = "cl_duree_offre"))


### Regroupement des modalités "0.5 + 1 + 2" et "3 + 4"

apprentissage_3 <- apprentissage_2 %>% 
  mutate(cl_duree_offre = fct_collapse(cl_duree_offre, "0.5 + 1 + 2 + 3 + 4" = c("0.5 + 1 + 2", "3 + 4")))


### Relance de la régression logistique

rl <- glm(formula = formule, data = apprentissage_3, family = "binomial")


### Analyse des modalités de la variable cl_nb_migrations

pairs(emmeans(object = rl, spec = "cl_nb_migrations"))


### Regroupement des modalités "1" et "2"

apprentissage_3 <- apprentissage_3 %>% 
  mutate(cl_nb_migrations = fct_collapse(cl_nb_migrations, "1 + 2" = c("1", "2")))


### Relance de la régression logistique

rl <- glm(formula = formule, data = apprentissage_3, family = "binomial")

pairs(emmeans(object = rl, spec = "cl_nb_migrations"))




## Résultats détaillés du modèle

summary(rl)
blr_regress(rl, odd_conf_limit = TRUE)




## Application du modèle sur la base de validation

### Regroupement des modalités

validation_3 <- validation_2 %>% 
  mutate(cl_duree_offre = fct_collapse(cl_duree_offre, "0.5 + 1 + 2 + 3 + 4" = c("0.5 + 1 + 2", "3 + 4")),
         cl_nb_migrations = fct_collapse(cl_nb_migrations, "1 + 2" = c("1", "2")))


### Calcul des probabilités estimées

validation_rl <- validation_3 %>% 
  mutate(p_resiliation = predict(object = rl,
                                 newdata = validation_3,
                                 type = "response")) %>% 
  select(id_client, flag_resiliation, p_resiliation)


### Distribution des probabilités estimées

ggplot(validation_rl) +
  aes(x = p_resiliation) +
  geom_density()


### Affectation de chaque patient à une classe prédite : quel seuil ?

#### Distribution des probabilités estimées pour chaque catégorie

ggplot(validation_rl) +
  aes(x = p_resiliation, fill = flag_resiliation) +
  geom_density(alpha = 0.7)


#### Distributions cumulées

rl %>%
  blr_gains_table() %>%
  blr_ks_chart()


#### Courbe ROC

rl %>%
  blr_gains_table() %>%
  blr_roc_curve()


#### Choix du seuil : taux de Y=1

validation_rl <- validation_rl %>% 
  mutate(flag_resiliation_predit = factor(if_else(p_resiliation >= 0.18, 1, 0)))




## Performance du modèle

### Matrice de confusion

table(validation_rl$flag_resiliation_predit, validation_rl$flag_resiliation)
prop(table(validation_rl$flag_resiliation_predit, validation_rl$flag_resiliation))   # Taux de bien classés = 76,0%
rprop(table(validation_rl$flag_resiliation_predit, validation_rl$flag_resiliation))  # Précision = 41,2%
cprop(table(validation_rl$flag_resiliation_predit, validation_rl$flag_resiliation))  # Sensibilité = 76,7%

confusionMatrix(data = validation_rl$flag_resiliation_predit,
                reference = validation_rl$flag_resiliation,
                positive = "1")




### Courbe ROC (AUC = 0.8402)

roc <- data.frame(tx_faux_positifs = 1 - roc(validation_rl$flag_resiliation, validation_rl$p_resiliation)$specificities,
                  tx_vrais_positifs = roc(validation_rl$flag_resiliation, validation_rl$p_resiliation)$sensitivities) %>% 
  arrange(tx_faux_positifs, tx_vrais_positifs)

auc <- roc(validation_rl$flag_resiliation, validation_rl$p_resiliation)$auc
auc

ggplot() +
  geom_line(data = roc,
            aes(x = tx_faux_positifs, y = tx_vrais_positifs),
            color = "steelblue4",
            linewidth = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "indianred",
               size = 1) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(title = paste("Courbe ROC   -   AUC =", round(auc, 3)),
       x = "% Faux positifs\n(1 - spécificité)",
       y = "% Vrais positifs\n(sensibilité)") +
  theme_minimal()




### Courbe de lift

lift <- validation_rl %>% 
  mutate(decile = ntile(p_resiliation, 10)) %>% 
  group_by(decile) %>% 
  summarise(nb_clients = n(),
            nb_resilies = sum(as.numeric(as.character(flag_resiliation)))) %>% 
  arrange(- decile)

lift <- lift %>% 
  mutate(nb_clients_cum = cumsum(nb_clients),
         nb_resilies_cum = cumsum(nb_resilies),
         tx_resilies = nb_resilies / nb_clients,
         p_clients_cum = nb_clients_cum / nrow(validation_rl),
         p_resilies_cum = nb_resilies_cum / sum(nb_resilies))

lift_parfait <- apprentissage_3 %>% 
  summarise(p = mean(flag_resiliation == "1"))

ggplot() +
  geom_path(data = rbind(c(0, 0, 0), lift %>% select(decile, p_clients_cum, p_resilies_cum)),
            aes(x = p_clients_cum, y = p_resilies_cum),
            color = "steelblue4",
            size = 1
  ) +
  geom_line(data = lift,
            aes(x = p_clients_cum, y = p_resilies_cum),
            color = "steelblue4",
            linewidth = 1) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
               color = "indianred",
               size = 1) +
  geom_segment(data = lift_parfait,
               aes(x = 0, y = 0, xend = p, yend = 1),
               linetype = "dashed") +
  geom_segment(data = lift_parfait,
               aes(x = p, y = 1, xend = 1, yend = 1),
               linetype = "dashed") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(title = "Courbe de lift",
       x = "% Population",
       y = "% cumulé Y=1") +
  theme_minimal()




### Taux de Y=1 par décile

ggplot(data = lift) +
  aes(x = decile, y = tx_resilies) +
  geom_col(fill = "steelblue4") +
  scale_x_continuous(breaks = seq(1, 10, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(x = "Décile",
       y = "% Y=1") +
  theme_minimal()






# Sélections automatiques (critère BIC)

## Paramétrage

formule_modele_vide <- as.formula("flag_resiliation ~ 1")

formule_modele_complet <- as.formula(paste("flag_resiliation ~ ",
                                           paste(apprentissage_2 %>%
                                                   select(where(is.factor), - id_client, - flag_resiliation) %>% 
                                                   colnames(),
                                                 collapse = " + ")))

rl_vide <- glm(formula = formule_modele_vide,
               data = apprentissage_2,
               family = "binomial")

rl_complet<- glm(formula = formule_modele_complet,
                 data = apprentissage_2,
                 family = "binomial")




## Sélection forward (AUC = 0.8718)

rl_forward <- step(scope = list(lower = rl_vide, upper = rl_complet),
                   object = rl_vide,
                   direction = "forward",
                   trace = TRUE,
                   k = log(nrow(apprentissage_2)))

validation_rl_f <- validation_2 %>% 
  mutate(p_resiliation = predict(object = rl_forward,
                           newdata = validation_2,
                           type = "response")) %>% 
  select(id_client, flag_resiliation, p_resiliation)

print(roc(validation_rl_f$flag_resiliation, validation_rl_f$p_resiliation)$auc)
