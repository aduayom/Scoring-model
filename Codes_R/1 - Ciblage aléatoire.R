#############################
### 2 - CIBLAGE ALEATOIRE ###
#############################


# Sélection aléatoire de 2000 clients (graine du tirage aléatoire à modifier)

set.seed(12345)
ciblage_aleatoire <- base_2023 %>% 
  slice_sample(n = 2000) %>% 
  select(id_client)


# Export pour le test de campagne (nom à personnaliser)

write.table(x = ciblage_aleatoire,
            file = "Ciblages/C1_Kienner.txt",
            fileEncoding = "UTF-8",
            row.names = FALSE,
            col.names = FALSE,
            quote = FALSE,
            na = "")
