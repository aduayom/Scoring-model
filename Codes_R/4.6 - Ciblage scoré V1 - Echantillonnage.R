#############################
### 4 - CIBLAGE SCORE V1  ###
### 4.6 - Échantillonnage ###
#############################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude


# Echantillonnage stratifié sur la variable cible
# - apprentissage : 70%
# - validation 30%

set.seed(123)

index_apprentissage <- as.vector(createDataPartition(base_6$flag_resiliation,
                                                     p = 0.7,
                                                     list = FALSE))

apprentissage_1 <- base_6 %>%
  slice(index_apprentissage)

validation_1 <- base_6 %>%
  slice(- index_apprentissage)


# Vérification que la proportion de résiliés est la même dans chaque base

freq(apprentissage_1$flag_resiliation, digits = 2, valid = FALSE)
freq(validation_1$flag_resiliation, digits = 2, valid = FALSE)
