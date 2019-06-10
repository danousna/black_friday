# Conseils de Baptiste
# 1. One hot encoding à faire
# 2. Virer Product_Category_2 et 3 => Kaggle semble confirmer ça

# Si PC1 > 80% => une combinaison linéaire de nos attributs est pertinente.
# Donc on pourra faire regression

# Algo : Reg log / Random Forest / SVM
# Faire avec plusieurs seeds et moyenner


# PREPARE DATASET

bf <- read.csv("dataset.csv", header=T)

bf$Occupation <- factor(bf$Occupation, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
bf$Marital_Status <- factor(bf$Marital_Status, levels = c('0', '1'))
bf$Product_Category_1 <- factor(bf$Product_Category_1, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18'))

# Drop Product_Category_2 and Product_Category_3
bf$Product_Category_2 <- NULL
bf$Product_Category_3 <- NULL

# Lebel encoding
bf$Age <- as.factor(as.numeric(bf$Age))
bf$City_Category <- as.factor(as.numeric(bf$City_Category))
bf$Stay_In_Current_City_Years <- as.factor(as.numeric(bf$Stay_In_Current_City_Years))

bf$Gender <- as.numeric(bf$Gender)
bf[which(bf$Gender==2),]$Gender <- 0
bf$Gender <- as.factor(bf$Gender)

library(randomForest)
library(tidyverse)

set.seed(9864)

bf_subset <- bf[sample(nrow(bf), 100000), ]
bf_subset$User_ID <- NULL
bf_subset$Product_ID <- NULL

ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))

train <- bf_subset[ran,]
test <- bf_subset[-ran,]
model <- randomForest(Gender ~ ., data = train, na.action = na.omit)
pred <- predict(model, newdata = test, type = 'response')
stats <- model_stats(test$Gender, pred)