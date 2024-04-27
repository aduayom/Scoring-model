library(readxl)
library(tidyverse)      # Manipulation des données et graphiques
library(skimr)          # Statistiques descriptives
library(questionr)      # Statistiques descriptives
library(ggstats)        # Statistiques descriptives
library(BioStatR)       # Mesures de liaison pour les variables quantitatives (rapport de corrélation)
library(DescTools)      # Mesures de liaison pour les variables qualitatives (Cramer, Tschuprow)
library(FactoMineR)     # Caractérisation des classes
library(UBL)            # Rééquilibrage
library(naniar)         # Gestion des valeurs manquantes
library(zoo)            # Gestion des valeurs manquantes
library(VIM)            # Gestion des valeurs manquantes
library(woeBinning)     # Discrétisation supervisée
library(smbinning)      # Discrétisation supervisée
library(caret)          # Modélisation
library(car)            # Modélisation
library(emmeans)        # Modélisation
library(blorr)          # Modélisation
library(pROC)           # Modélisation
library(broom)          # Modélisation
library(rpart)          # Modélisation
library(rpart.plot)     # Modélisation
library(randomForest)   # Modélisation
library(nnet)           # Modélisation
library(NeuralNetTools) # Modélisation


base_telecom_2022 <- read_excel("C:/Users/Daniel/Desktop/Mastère ENSAI/Scoring/base_telecom_2022.xlsx") %>%
as.data.frame()

View(base_telecom_2022)

names(base_telecom_2022)

# Calculer le taux de croissance
base_telecom_2022 <- base_telecom_2022 %>%
  mutate(taux_croissance_appel = ((vol_appels_m1 - vol_appels_m6) / vol_appels_m6) * 100)

base_telecom_2022 <- base_telecom_2022 %>%
  mutate(taux_croissance_sms = ((nb_sms_m1 - nb_sms_m6) / nb_sms_m6) * 100)

transformer_en_factor <- function(df, vars) {
  for (var in vars) {
    if (var %in% names(df)) {
      df[[var]] <- factor(df[[var]])
    } else {
      warning(paste("La variable", var, "n'existe pas dans le dataframe. Elle sera ignorée."))
    }
  }
  return(df)
}

# Supposons que 'df' est votre dataframe
variables_a_transformer <- c("flag_resiliation", "sexe", "flag_migration_hausse","csp","enseigne","mode_paiement",
                             "telephone_init","telephone","Personnalisation","segment",
                             "flag_migration_baisse", "flag_personnalisation_repondeur", "flag_telechargement_sonnerie",
                             "Upgrade_tel","Downgrade_tel","Identif_tel","Personnalisation","Internationnal","situation_impayes",
                             "flag_appels_vers_international","flag_appels_depuis_international","flag_appels_numeros_speciaux")

base_telecom_2022 <- transformer_en_factor(base_telecom_2022, variables_a_transformer)


# Echantillonnage stratifié sur la variable cible
# - apprentissage : 70%
# - validation 30%

set.seed(123)

index_apprentissage <- as.vector(createDataPartition(base_telecom_2022$flag_resiliation,
                                                     p = 0.7,
                                                     list = FALSE))

# Diviser les données en ensembles d'apprentissage et de validation
apprentissage_1 <- base_telecom_2022[index_apprentissage, ]
validation_1 <- base_telecom_2022[-index_apprentissage, ]



woe_Reengament_type <- woe.binning(df = apprentissage_1,
                   target.var = "flag_resiliation",
                   pred.var = "Reengament_type",
                   event.class = "1")

woe_Dead_line_engagement <- woe.binning(df = apprentissage_1,
                                   target.var = "flag_resiliation",
                                   pred.var = "Dead_line_engagement",
                                   event.class = "1")

woe_duree_engagement <- woe.binning(df = apprentissage_1,
                                    target.var = "flag_resiliation",
                                    pred.var = "duree_engagement",
                                    event.class = "1")


woe_Variation_offre_ini <- woe.binning(df = apprentissage_1,
                                    target.var = "flag_resiliation",
                                    pred.var = "Variation_offre_ini",
                                    event.class = "1")

woe_anciennete <- woe.binning(df = apprentissage_1,
                                       target.var = "flag_resiliation",
                                       pred.var = "anciennete",
                                       event.class = "1")

woe_Variabilité_vol <- woe.binning(df = apprentissage_1,
                              target.var = "flag_resiliation",
                              pred.var = "Variabilité_vol",
                              event.class = "1")

woe_Variabilité_sms <- woe.binning(df = apprentissage_1,
                                   target.var = "flag_resiliation",
                                   pred.var = "Variabilité_sms",
                                   event.class = "1")

woe_nb_reengagements <- woe.binning(df = apprentissage_1,
                                   target.var = "flag_resiliation",
                                   pred.var = "nb_reengagements",
                                   event.class = "1")


woe_duree_engagement <- woe.binning(df = apprentissage_1,
                                    target.var = "flag_resiliation",
                                    pred.var = "duree_engagement",
                                    event.class = "1")

woe_age <- woe.binning(df = apprentissage_1,
                                    target.var = "flag_resiliation",
                                    pred.var = "age",
                                    event.class = "1")

woe_duree_offre <- woe.binning(df = apprentissage_1,
                       target.var = "flag_resiliation",
                       pred.var = "duree_offre",
                       event.class = "1")


woe_taux_croissance_appel <- woe.binning(df = apprentissage_1,
                               target.var = "flag_resiliation",
                               pred.var = "taux_croissance_appel",
                               event.class = "1")

woe_taux_croissance_sms <- woe.binning(df = apprentissage_1,
                               target.var = "flag_resiliation",
                               pred.var = "taux_croissance_sms",
                               event.class = "1")


                                 
apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                          binning = woe_duree_engagement) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_duree_engagement_woe = duree_engagement.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_Variation_offre_ini) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variation_offre_ini_woe = Variation_offre_ini.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_anciennete) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_anciennete_woe = anciennete.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_Variabilité_vol) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variabilité_vol_woe = Variabilité_vol.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_Variabilité_sms) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Variabilité_sms_woe = Variabilité_sms.binned)

apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_age) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_age_woe = age.binned)

apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_duree_offre) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_duree_offre_woe = duree_offre.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_Reengament_type) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Reengament_type_woe = Reengament_type.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_Dead_line_engagement) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_Dead_line_engagement_woe = Dead_line_engagement.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_taux_croissance_appel) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_taux_croissance_appel_woe = taux_croissance_appel.binned)


apprentissage_1 <- woe.binning.deploy(df = apprentissage_1,
                                      binning = woe_taux_croissance_sms) %>% 
  mutate(across(ends_with(".binned"), fct_drop)) %>% 
  rename(cl_taux_croissance_sms_woe = taux_croissance_sms.binned)

