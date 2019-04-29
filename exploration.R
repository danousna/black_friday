# Exploration des données

# Description
# Ces données sont les ventes provenant d'un seul magasin lors du Black Friday.
# Le magasin veut mieux comprendre le comportement des consommateur sur différents produits.
# Selon Kaggle, le problème principal est un problème de regression. On essaie de prédire la variable
# quantité d'achat à l'aide des autres variables.

# Questions que l'on peut se poser

# Sociologie
# - Age
# - Métier
# - Ancienneté dans la ville
# - Marié, pas mérié
# - Sexe

# Commercial
# - Performance des produit (via ID)
# - Catégorie de produit qui se vendent le mieux

install.packages("tidyverse")
library(tidyverse)

bf <- read.csv("dataset.csv", header=T)
summary(bf)
head(bf)

bf_gender = bf %>% select(User_ID, Gender) %>% group_by(User_ID) %>% distinct()
summary(bf_gender)
ggplot(data = bf_gender) + 
  geom_bar(mapping = aes(x = Gender, y = ..count.., fill = Gender)) + 
  labs(title = 'Gender of Customers') + 
  scale_fill_brewer(palette = 'PuBuGn')