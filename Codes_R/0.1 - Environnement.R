###########################
### 0.1 - ENVIRONNEMENT ###
###########################


# Nettoyage de l'environnement

rm(list=ls())
gc()


# Répertoire de travail (à personnaliser en fonction de l'arborescence)

setwd("~/3. ENSEIGNEMENTS/2. Ensai/2. MS Data Science/1. Scoring/2023 - 2024")


# Packages utiles

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
