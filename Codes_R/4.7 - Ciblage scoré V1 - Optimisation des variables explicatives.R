#####################################################
### 4 - CIBLAGE SCORE V1                          ###
### 4.7 - Optimisation des variables explicatives ###
#####################################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude


# Exemples de discrétisation sur une variable quantitative : durée d'engagement ----

base_discretisation <- apprentissage_1 %>% 
  select(id_client, flag_resiliation, duree_engagement)




## Discrétisation manuelle ----

ggplot(base_discretisation) +
  aes(x = duree_engagement, fill = flag_resiliation) +
  geom_density(alpha = 0.7)

base_discretisation <- base_discretisation %>% 
  mutate(vingtiles_duree_engagement = ntile(duree_engagement, 20))

base_discretisation %>% 
  group_by(vingtiles_duree_engagement) %>%
  summarise(nb = n(),
            pct_y = mean(flag_resiliation == 1),
            pct = nb / nrow(base_discretisation)) %>% 
  ggplot +
  geom_col(aes(x = vingtiles_duree_engagement, y = pct_y), fill = "steelblue4") +
  geom_point(aes(x = vingtiles_duree_engagement, y = pct), shape = 18, size = 2, colour = "indianred") +
  labs(x = "Durée d'engagement (vingtiles)", y ="Proportion de résiliés (barres verticales) \n Poids dans la base (points)") +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  geom_text(aes(x = vingtiles_duree_engagement, y = pct_y, label = scales::percent(round(pct_y, 2))), vjust = "top", size = 3, color = "white") +
  theme_minimal()

base_discretisation <- base_discretisation %>% 
  mutate(cl_duree_engagement_manuel = case_when(vingtiles_duree_engagement %in% c(1, 2) ~ 1,
                                                vingtiles_duree_engagement %in% c(3, 4) ~ 2,
                                                vingtiles_duree_engagement %in% c(5, 6, 7) ~ 3,
                                                vingtiles_duree_engagement %in% c(8, 9, 10, 11) ~ 4,
                                                .default = 5))

table(base_discretisation$vingtiles_duree_engagement, base_discretisation$cl_duree_engagement_manuel)
freq(base_discretisation$cl_duree_engagement_manuel)
rprop(table(base_discretisation$cl_duree_engagement_manuel, base_discretisation$flag_resiliation))




## Discrétisation supervisée - woeBinning ----

woe <- woe.binning(df = base_discretisation,
                   target.var = "flag_resiliation",
                   pred.var = "duree_engagement",
                   event.class = "1")

woe.binning.table(woe)
woe.binning.plot(woe)

base_discretisation <- woe.binning.deploy(df = base_discretisation,
                                          binning = woe) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_duree_engagement_woe = duree_engagement.binned)

freq(base_discretisation$cl_duree_engagement_woe)
rprop(table(base_discretisation$cl_duree_engagement_woe, base_discretisation$flag_resiliation))




## Discrétisation supervisée - smbinning ----

base_discretisation <- base_discretisation %>%
  mutate(flag_resiliation_int = if_else(flag_resiliation == 1, 1, 0))

smb <- smbinning(df = base_discretisation,
                 y = "flag_resiliation_int",
                 x = "duree_engagement")

smb$ivtable
smb$bands
smbinning.sql(smb)

base_discretisation <- smbinning.gen(df = base_discretisation,
                                     ivout = smb,
                                     chrname = "cl_duree_engagement_smb")

freq(base_discretisation$cl_duree_engagement_smb)
rprop(table(base_discretisation$cl_duree_engagement_smb, base_discretisation$flag_resiliation))




## Comparaison des discrétisations ----

rprop(table(base_discretisation$cl_duree_engagement_manuel, base_discretisation$flag_resiliation))
rprop(table(base_discretisation$cl_duree_engagement_woe, base_discretisation$flag_resiliation))
rprop(table(base_discretisation$cl_duree_engagement_smb, base_discretisation$flag_resiliation))

CramerV(base_discretisation$cl_duree_engagement_manuel, base_discretisation$flag_resiliation)
CramerV(base_discretisation$cl_duree_engagement_woe, base_discretisation$flag_resiliation)
CramerV(base_discretisation$cl_duree_engagement_smb, base_discretisation$flag_resiliation)

TschuprowT(base_discretisation$cl_duree_engagement_manuel, base_discretisation$flag_resiliation)
TschuprowT(base_discretisation$cl_duree_engagement_woe, base_discretisation$flag_resiliation)
TschuprowT(base_discretisation$cl_duree_engagement_smb, base_discretisation$flag_resiliation)






# Exemples de discrétisation sur une variable qualitative : nombre de services ----

base_discretisation <- apprentissage_1 %>% 
  select(id_client, flag_resiliation, csp)




## Discrétisation manuelle ----

rprop(table(apprentissage_1$csp, apprentissage_1$flag_resiliation))

base_discretisation %>% 
  group_by(csp) %>% 
  summarise(nb = n(),
            pct_y = mean(flag_resiliation == 1),
            pct = nb / nrow(base_discretisation)) %>% 
  ggplot +
  geom_col(aes(x = csp, y = pct_y), fill = "steelblue4") +
  geom_point(aes(x = csp, y = pct), shape = 18, size = 2, colour = "indianred") +
  labs(x = "CSP", y ="Proportion de résiliés (barres verticales) \n Poids dans la base (points)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  geom_text(aes(x = csp, y = pct_y, label = scales::percent(pct_y)), vjust = "top", size = 3, color = "white") +
  theme_minimal()

base_discretisation <- base_discretisation %>% 
  mutate(cl_csp_manuel = case_when(csp %in% c("Commerçant", "Profession libérale") ~ 1,
                                   csp == "Autre" ~ 2,
                                   csp %in% c("Cadre", "Employé", "Fonctionnaire", "Sans emploi") ~ 3,
                                   csp == "Etudiant" ~ 4,
                                   .default = 5))

table(base_discretisation$csp, base_discretisation$cl_csp_manuel)
freq(base_discretisation$cl_csp_manuel)
rprop(table(base_discretisation$cl_csp_manuel, base_discretisation$flag_resiliation))




## Discrétisation supervisée - woeBinning ----

woe <- woe.binning(df = base_discretisation,
                   target.var = "flag_resiliation",
                   pred.var = "csp",
                   event.class = "1")

woe.binning.table(woe)
woe.binning.plot(woe)

base_discretisation <- woe.binning.deploy(df = base_discretisation,
                                          binning = woe) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_csp_woe = csp.binned)

table(base_discretisation$csp, base_discretisation$cl_csp_woe)
freq(base_discretisation$cl_csp_woe)
rprop(table(base_discretisation$cl_csp_woe, base_discretisation$flag_resiliation))




## Discrétisation supervisée - smbinning ----
## => moins utile car ne regroupe pas automatiquement les modalités
##    ne fait que les décrire pour pouvoir ensuite les regrouper manuellement (on obtient quand même les WOE et IV)

base_discretisation <- base_discretisation %>%
  mutate(flag_resiliation_int = if_else(flag_resiliation == 1, 1, 0))

smb <- smbinning.factor(df = base_discretisation,
                        y = "flag_resiliation_int",
                        x = "csp",
                        maxcat = 20)

smb$ivtable




## Comparaison des discrétisations ----

TschuprowT(base_discretisation$cl_csp_manuel, base_discretisation$flag_resiliation)
TschuprowT(base_discretisation$cl_csp_woe, base_discretisation$flag_resiliation)






# Mise en œuvre sur toutes les variables ----

## Liste des variables à discrétiser ou dont les modalités sont à regrouper ---

var_qt <- apprentissage_1 %>%
  select(where(is.numeric)) %>% 
  colnames()

var_ql <- apprentissage_1 %>%
  select(where(is.factor), - starts_with("flag")) %>% 
  colnames()




## Création des "recettes" de discrétisation pour les variables quantitatives ----

decoupage_qt <- woe.binning(df = apprentissage_1,
                            target.var = "flag_resiliation",
                            pred.var = var_qt,
                            event.class = "1")

woe.binning.table(decoupage_qt)




## Regroupement des modalités des variables qualitatives

for(var in var_ql) {
  
  print(apprentissage_1 %>% 
          group_by(.data[[var]]) %>% 
          summarise(
            nb = n(),
            pct_y = mean(flag_resiliation == 1),
            pct = nb / nrow(apprentissage_1)) %>% 
          ggplot +
          geom_col(aes(x = .data[[var]], y = pct_y), fill = "steelblue4") +
          geom_point(aes(x = .data[[var]], y = pct), shape = 18, size = 3, color = "indianred") +
          xlab({{var}}) +
          ylab("Proportion de résiliés (barres verticales) et Poids dans la base (points)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_y_continuous(labels = scales::percent) +
          geom_text(aes(x = .data[[var]], y = pct_y, label = scales::percent(pct_y)), vjust = "top", size = 3, color = "white") +
          theme_minimal())
  
}

regroupement_ql <- woe.binning(df = apprentissage_1,
                               target.var = "flag_resiliation",
                               pred.var = var_ql,
                               event.class = "1")

woe.binning.table(regroupement_ql)




## Application des "recettes" de discrétisation à l'ensemble des variables ----
## (échantillons d'apprentissage et de validation)

apprentissage_2 <- woe.binning.deploy(apprentissage_1, decoupage_qt) %>%
  rename_with(.fn = function(.x) { paste0("cl_", .x) },
              .cols = ends_with("binned")) %>% 
  rename_with(~ str_remove(.x, ".binned")) %>% 
  mutate(cl_anciennete_reengagement = fct_recode(cl_anciennete_reengagement, "Aucun réengagement" = "Missing"),
         cl_sexe = sexe,
         cl_csp = case_when(csp %in% c("Commerçant", "Profession libérale", "Autre") ~ 1,
                            csp == "Autre" ~ 2,
                            csp %in% c("Cadre", "Employé", "Fonctionnaire", "Sans emploi") ~ 3,
                            csp == "Etudiant" ~ 4,
                            .default = 5),
         cl_enseigne = case_when(enseigne == "Internet" ~ "Internet",
                                 enseigne %in% c("Boutique", "Grande distribution") ~ "Boutique + Grande distribution",
                                 .default = "Autre"),
         cl_mode_paiement = case_when(mode_paiement == "Virement" ~ "Virement",
                                      mode_paiement %in% c("Chèque", "TIP") ~ "Chèque + TIP",
                                      .default = "Autre"),
         cl_duree_offre_init = case_when(duree_offre_init %in% c("0.5", "1") ~ "0.5 + 1",
                                         duree_offre_init == "2" ~ "2",
                                         duree_offre_init %in% c("3", "4", "6", "8", "10") ~ "3 et plus",
                                         .default = "Autre"),
         cl_duree_offre = case_when(duree_offre %in% c("0.5", "1", "2") ~ "0.5 + 1 + 2",
                                    duree_offre %in% c("3", "4") ~ "3 + 4",
                                    duree_offre %in% c("6", "8", "10") ~ "6 et plus",
                                    .default = "Autre"),
         cl_nb_migrations = case_when(nb_migrations == "0" ~ "0",
                                      nb_migrations == "1" ~ "1",
                                      nb_migrations == "2" ~ "2",
                                      nb_migrations %in% c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13") ~ "3 et plus",
                                      .default = "Autre"),
         cl_nb_services = case_when(nb_services == "0" ~ "0",
                                    nb_services == "1" ~ "1",
                                    nb_services %in% c("2", "3", "4") ~ "2 à 4",
                                    nb_services %in% c("5", "6", "7", "8", "9", "10", "11", "12", "13") ~ "5 et plus",
                                    .default = "Autre"),
         cl_telephone_init = case_when(telephone_init == "Milieu de gamme" ~ "Milieu de gamme",
                                       telephone_init %in% c("Bas de gamme", "Haut de gamme") ~ "Bas de gamme + Haut de gamme",
                                       telephone_init == "Carte SIM seule" ~ "Carte SIM seule",
                                       .default = "Autre"),
         cl_telephone = telephone,
         flag_reengagement = if_else(nb_reengagements != "0", "1", "0"),
         cl_situation_impayes = situation_impayes,
         cl_segment = case_when(segment == "A" ~ "A",
                                segment %in% c("B", "C") ~ "B + C",
                                .default = "Autre"),
         across(starts_with("flag_"), as.factor),
         across(starts_with("cl_"), as.factor),
         across(where(is.factor), fct_drop)) %>% 
  select(- all_of(var_qt), - all_of(var_ql))


validation_2 <- woe.binning.deploy(validation_1, decoupage_qt) %>%
  rename_with(.fn = function(.x) { paste0("cl_", .x) },
              .cols = ends_with("binned")) %>% 
  rename_with(~ str_remove(.x, ".binned")) %>% 
  mutate(cl_anciennete_reengagement = fct_recode(cl_anciennete_reengagement, "Aucun réengagement" = "Missing"),
         cl_sexe = sexe,
         cl_csp = case_when(csp %in% c("Commerçant", "Profession libérale", "Autre") ~ 1,
                            csp == "Autre" ~ 2,
                            csp %in% c("Cadre", "Employé", "Fonctionnaire", "Sans emploi") ~ 3,
                            csp == "Etudiant" ~ 4,
                            .default = 5),
         cl_enseigne = case_when(enseigne == "Internet" ~ "Internet",
                                 enseigne %in% c("Boutique", "Grande distribution") ~ "Boutique + Grande distribution",
                                 .default = "Autre"),
         cl_mode_paiement = case_when(mode_paiement == "Virement" ~ "Virement",
                                      mode_paiement %in% c("Chèque", "TIP") ~ "Chèque + TIP",
                                      .default = "Autre"),
         cl_duree_offre_init = case_when(duree_offre_init %in% c("0.5", "1") ~ "0.5 + 1",
                                         duree_offre_init == "2" ~ "2",
                                         duree_offre_init %in% c("3", "4", "6", "8", "10") ~ "3 et plus",
                                         .default = "Autre"),
         cl_duree_offre = case_when(duree_offre %in% c("0.5", "1", "2") ~ "0.5 + 1 + 2",
                                    duree_offre %in% c("3", "4") ~ "3 + 4",
                                    duree_offre %in% c("6", "8", "10") ~ "6 et plus",
                                    .default = "Autre"),
         cl_nb_migrations = case_when(nb_migrations == "0" ~ "0",
                                      nb_migrations == "1" ~ "1",
                                      nb_migrations == "2" ~ "2",
                                      nb_migrations %in% c("3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13") ~ "3 et plus",
                                      .default = "Autre"),
         cl_nb_services = case_when(nb_services == "0" ~ "0",
                                    nb_services == "1" ~ "1",
                                    nb_services %in% c("2", "3", "4") ~ "2 à 4",
                                    nb_services %in% c("5", "6", "7", "8", "9", "10", "11", "12", "13") ~ "5 et plus",
                                    .default = "Autre"),
         cl_telephone_init = case_when(telephone_init == "Milieu de gamme" ~ "Milieu de gamme",
                                       telephone_init %in% c("Bas de gamme", "Haut de gamme") ~ "Bas de gamme + Haut de gamme",
                                       telephone_init == "Carte SIM seule" ~ "Carte SIM seule",
                                       .default = "Autre"),
         cl_telephone = telephone,
         flag_reengagement = if_else(nb_reengagements != "0", "1", "0"),
         cl_situation_impayes = situation_impayes,
         cl_segment = case_when(segment == "A" ~ "A",
                                segment %in% c("B", "C") ~ "B + C",
                                .default = "Autre"),
         across(starts_with("flag_"), as.factor),
         across(starts_with("cl_"), as.factor),
         across(where(is.factor), fct_drop)) %>% 
  select(- all_of(var_qt), - all_of(var_ql))




## Description de l'ensemble des variables ----

skim(apprentissage_2)

var_ql <- apprentissage_2 %>%
  select(where(is.factor)) %>% 
  colnames()

for (var in var_ql) {
  cat("\n\n", "VARIABLE : ", var, "\n\n", sep = "")
  print(freq(apprentissage_2[[var]], total = TRUE))
}
