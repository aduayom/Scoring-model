#######################################
### 2 - CIBLAGE METIER              ###
### 2.1 - Description des variables ###
#######################################


# Analyse globale ----

summary(base_2023)
skim(base_2023)




# Variables qualitatives ----

## Exemple ----

### Tableau de répartition

freq(base_2023$segment, total = TRUE)


### Diagramme en bâtons

ggplot(data = base_2023) +
  aes(y = segment) +
  geom_bar()

ggplot(data = base_2023) +
  aes(x = after_stat(prop),
      y = fct_infreq(segment)) +
  geom_bar(stat = "prop",
           fill = "steelblue4") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                     labels = scales::percent) +
  labs(title = "segment",
       x = NULL,
       y = NULL) +
  geom_text(aes(label = after_stat(prop) %>% scales::percent(accuracy = 0.1)),
            stat = "prop",
            hjust = -0.05,
            size = 3) +
  theme_minimal()




## Généralisation à toutes les variables qualitatives ----

var_ql <- base_2023 %>%
  select(where(is.factor)) %>% 
  colnames()


for (var in var_ql) {
  
  cat("\n\n", "VARIABLE : ", var, "\n\n", sep = "")
  print(freq(base_2023[[var]], total = TRUE))
  
  print(ggplot(data = base_2023) +
          aes(x = after_stat(prop),
              y = fct_infreq(.data[[var]])) +
          geom_bar(stat = "prop",
                   fill = "steelblue4") +
          scale_x_continuous(breaks = seq(0, 1, by = 0.1),
                             labels = scales::percent) +
          labs(title = var,
               x = NULL,
               y = NULL) +
          geom_text(aes(label = after_stat(prop) %>% scales::percent(accuracy = 0.1)),
                    stat = "prop",
                    hjust = -0.05,
                    size = 3) +
          theme_minimal())
  
}




# Variables quantitatives ----

## Exemple ----

### Indicateurs de distribution

summary(base_2023$vol_appels_m1)


### Graphiques

ggplot(data = base_2023) +
  aes(x = vol_appels_m1) +
  geom_histogram()

ggplot(data = base_2023) +
  aes(x = vol_appels_m1) +
  geom_density()

ggplot(data = base_2023) +
  aes(x = vol_appels_m1) +
  geom_boxplot()




## Généralisation à toutes les variables quantitatives ----

var_qt <- base_2023 %>%
  select(where(is.numeric)) %>% 
  colnames()


for (var in var_qt) {
  
  cat("\n\n", "VARIABLE : ", var, "\n\n", sep = "")
  print(summary(base_2023[[var]]))
  
  print(ggplot(data = base_2023) +
          aes(x = .data[[var]]) +
          geom_density() +
          labs(title = var) +
          theme_minimal())
  
}
