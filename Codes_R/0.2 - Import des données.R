################################
### 0.2 - IMPORT DES DONNEES ###
################################


# Import des données 2023

base_2023 <- read.table(file = "Sources/base_telecom_2023_03.txt",
                        fileEncoding = "UTF-8",
                        sep = ";",
                        header = TRUE,
                        na.strings = "")


# Import des données 2022

base_2022 <- read.table(file = "Sources/base_telecom_2022_12.txt",
                        fileEncoding = "UTF-8",
                        sep = ";",
                        header = TRUE,
                        na.strings = "")
