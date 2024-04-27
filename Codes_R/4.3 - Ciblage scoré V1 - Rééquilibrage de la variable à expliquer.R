######################################################
### 4 - CIBLAGE SCORE V1                           ###
### 4.3 - Rééquilibrage de la variable à expliquer ###
######################################################


# Remarque :
# Ce code est fourni à titre d'exemple pour aider à la programmation du ciblage
# Le ciblage généré n'est donc pas pertinent pour notre problématique
# Il n'est pas à reproduire pour fournir le fichier de ciblage
# Il est à adapter en fonction de la stratégie d'étude




# Analyse de la variable à expliquer ----
#       n     %
# 0 36399 81.96
# 1  8012 18.04

freq(base_1$flag_resiliation, digits = 2, valid = FALSE)


# 18,04% de résiliés : on ne rééquilibre pas pour cette 1ère version du score






# Exemples de rééquilibrage ----

## Base simplifiée (hors identifiant, dates et valeurs manquantes pour Smote et Tomek)

skim(base_1)

base_reequilibrage <- base_1 %>% 
  select(- id_client, - code_postal, - starts_with("date_"), - sexe,
         - type_ville, - taille_ville, - revenu_moyen_ville, - nb_sms_m3)

skim(base_reequilibrage)




## Sur-échantillonnage ----

### Aléatoire (à ne pas privilégier) ----

#### Nombre de Y=1 multiplié par 4
####       n     %
#### 0 36399 53.18
#### 1 32048 46.82

set.seed(123)

base_reequilibree_1 <- RandOverClassif(flag_resiliation ~ .,
                                       base_reequilibrage,
                                       C.perc = list("0" = 1, "1" = 4))

freq(base_reequilibree_1$flag_resiliation, digits = 2, valid = FALSE)


#### Nombre de Y=1 multiplié par 2
#### (on peut alors demander à ce qu'il n'y ait pas de doublon parmi les enregistrements créés)
####       n     %
#### 0 36399 69.43
#### 1 16024 30.57

set.seed(123)

base_reequilibree_1 <- RandOverClassif(flag_resiliation ~ .,
                                       base_reequilibrage,
                                       C.perc = list("0" = 1, "1" = 2),
                                       repl = FALSE)

freq(base_reequilibree_1$flag_resiliation, digits = 2, valid = FALSE)


#### Nombre de Y=1 égal au nombre de Y=0
####       n  %
#### 0 36399 50
#### 1 36399 50

set.seed(123)

base_reequilibree_1 <- RandOverClassif(flag_resiliation ~ .,
                                       base_reequilibrage,
                                       C.perc = "balance")

freq(base_reequilibree_1$flag_resiliation, digits = 2, valid = FALSE)




### Algorithme Smote ----
### - Ne pas utiliser d'identifiant, ni de variable date, ni de NA
### - La distance euclidienne par défaut s'applique à un ensemble de variables numériques
###   => on peut la changer avec l'option dist
###   => si on a des variables uniquement qualitatives, on met dist = "Overlap"
###   => si on a des variables quantitatives et qualitatives, on met dist = "HEOM"

#### Nombre de Y=1 multiplié par 4
####       n    %
#### 0 36399 53.2
#### 1 32048 46.8

set.seed(123)

base_reequilibree_2 <- SmoteClassif(flag_resiliation ~ .,
                                    base_reequilibrage,
                                    k = 3,
                                    dist = "HEOM",
                                    C.perc = list("0" = 1, "1" = 4))

freq(base_reequilibree_2$flag_resiliation, digits = 2, valid = FALSE)


#### Nombre de Y=1 égal au nombre de Y=0
#### => sur-échantillonnage des Y=1 et sous-échantillonnage des Y=0
####    nb = (nb Y=0 initial + nb Y=1 initial) / 2
####       n  %
#### 0 22206 50
#### 1 22206 50

set.seed(123)

base_reequilibree_2 <- SmoteClassif(flag_resiliation ~ .,
                                    base_reequilibrage,
                                    k = 3,
                                    dist = "HEOM",
                                    C.perc = "balance")

freq(base_reequilibree_2$flag_resiliation, digits = 2, valid = FALSE)




## Sous-échantillonnage ----

### Aléatoire ----

#### Nombre de Y=0 divisé par 4 (environ)
####      n     %
#### 0 9099 53.18
#### 1 8012 46.82

set.seed(123)

base_reequilibree_3 <- RandUnderClassif(flag_resiliation ~ .,
                                        base_reequilibrage,
                                        C.perc = list("0" = 0.25, "1" = 1))

freq(base_reequilibree_3$flag_resiliation, digits = 2, valid = FALSE)


#### Nombre de Y=1 égal au nombre de Y=0
####      n  %
#### 0 8012 50
#### 1 8012 50

set.seed(123)

base_reequilibree_3 <- RandUnderClassif(flag_resiliation ~ .,
                                        base_reequilibrage,
                                        C.perc = "balance")

freq(base_reequilibree_3$flag_resiliation, digits = 2, valid = FALSE)




### Tomek (on arrive rarement à un équilibre) ----
### rem = both : on enlève les 2 individus formant le "lien Tomek"
### rem = maj : on enlève seulement l'individu Y=1 formant le "lien Tomek"
###       n     %
### 0 36027 81.81
### 1  8012 18.19

base_reequilibree_4 <- as.data.frame(TomekClassif(flag_resiliation ~ .,
                                                  base_reequilibrage,
                                                  dist = "HEOM",
                                                  rem = "maj")[1])

freq(base_reequilibree_4$flag_resiliation, digits = 2, valid = FALSE)
