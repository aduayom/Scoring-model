#########################################
### 3 - CIBLAGE PROFILE               ###
### 3.1 - Exemple d'analyse de profil ###
#########################################


# Analyse univariée de la variable à expliquer ----

freq(base_2022$flag_resiliation, total = TRUE)




# Analyse bivariée d'une variable quantitative : vol_appels_m1 ----

## Quartiles et moyennes

tapply(base_2022$vol_appels_m1,
       base_2022$flag_resiliation,
       "summary")


## Intervalles de confiance autour de la moyenne

t.test(base_2022$vol_appels_m1[base_2022$flag_resiliation == 0])$conf.int
t.test(base_2022$vol_appels_m1[base_2022$flag_resiliation == 1])$conf.int


## Boîtes à moustaches

ggplot(base_2022) +
  aes(x = vol_appels_m1, fill = flag_resiliation) +
  geom_boxplot()


## Courbes de densité

ggplot(base_2022) +
  aes(x = vol_appels_m1, fill = flag_resiliation) +
  geom_density(alpha = 0.7)


## Test paramétrique de Student

t.test(base_2022$vol_appels_m1 ~ base_2022$flag_resiliation)


## Test non-paramétrique de Wilcoxon

wilcox.test(base_2022$vol_appels_m1 ~ base_2022$flag_resiliation)


## Test non-paramétrique de Kolmogorov-Smirnov

ks.test(base_2022$vol_appels_m1 ~ base_2022$flag_resiliation)


## Mesures de l'intensité de la liaison : distance de Kolmogorov-Smirnov et rapport de corrélation

ks.test(base_2022$vol_appels_m1 ~ base_2022$flag_resiliation)$statistic
eta2(base_2022$vol_appels_m1, base_2022$flag_resiliation)




# Analyse bivariée d'une variable qualitative : segment ----

## Tableau croisé

table(base_2022$segment, base_2022$flag_resiliation)
prop(table(base_2022$segment, base_2022$flag_resiliation))
cprop(table(base_2022$segment, base_2022$flag_resiliation))
rprop(table(base_2022$segment, base_2022$flag_resiliation))


## Diagramme en bâtons

base_2022 %>% 
  group_by(segment) %>% 
  summarise(
    nb = n(),
    pct_y = mean(flag_resiliation == 1),
    pct = nb / nrow(base_2022)) %>% 
  ggplot +
  geom_col(aes(x = segment, y = pct_y), fill = "steelblue4") +
  geom_point(aes(x = segment, y = pct), shape = 18, size = 3, colour = "indianred") +
  labs(x = "segment", y ="Proportion de résiliés (barres verticales) \n Poids dans la base (points)") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), labels = scales::percent) +
  geom_text(aes(x = segment, y = pct_y, label = scales::percent(pct_y)), vjust = "top", size = 3, color = "white") +
  theme_minimal()


## Test du Khi-Deux

chisq.test(table(base_2022$flag_resiliation, base_2022$segment))


## Mesures de l'intensité de la liaison : V de Cramer et T de Tschuprow

CramerV(base_2022$segment, base_2022$flag_resiliation)
TschuprowT(base_2022$segment, base_2022$flag_resiliation)
