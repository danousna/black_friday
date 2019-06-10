library(tidyverse)
library(randomForest)
library(ROSE)

set.seed(9864)

# PREPARE DATASET

bf <- read.csv("dataset.csv", header=T)

bf$Occupation <- factor(bf$Occupation, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
bf$Marital_Status <- factor(bf$Marital_Status, levels = c('0', '1'))
bf$Product_Category_1 <- factor(bf$Product_Category_1, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18'))

# Drop User_ID and Product_ID
bf$User_ID <- NULL
bf$Product_ID <- NULL

# Drop Product_Category_2 and Product_Category_3
bf$Product_Category_2 <- NULL
bf$Product_Category_3 <- NULL

# Lebel encoding
bf$Age <- as.factor(as.numeric(bf$Age))
bf$City_Category <- as.factor(as.numeric(bf$City_Category))
bf$Stay_In_Current_City_Years <- as.factor(as.numeric(bf$Stay_In_Current_City_Years))
bf$Gender <- as.numeric(bf$Gender)
# MALE IS 0, FEMALE IS 1
bf[which(bf$Gender==2),]$Gender <- 0
bf$Gender <- as.factor(bf$Gender)

bf <- bf[sample(nrow(bf), 100000), ]

logreg <- function(train, test) {
  model <- glm(Gender ~., family = binomial(link='logit'), data = train)

  pred <- predict(model, newdata = test, type = 'response')
  pred <- ifelse(pred > 0.5,1,0)
  
  out <- NULL
  out <- model_stats(test$Gender, pred)
  out$model <- model
  out$pred <- pred
  
  out
}

randforest <- function(train, test) {
  model <- randomForest(Gender ~ ., data = train, na.action = na.omit)
  
  pred <- predict(model, newdata = test, type = 'response')
  
  out <- NULL
  out <- model_stats(test$Gender, pred)
  out$model <- model
  out$pred <- pred
  
  out
}

ran <- sample(1:nrow(bf), 0.80 * nrow(bf))

train <- bf[ran, ]
test <- bf[-ran, ]

train.over <- ovun.sample(Gender ~ ., data = train, method = "over", p = 0.5)$data
train.under <- ovun.sample(Gender ~ ., data = train, method = "under", p = 0.5)$data
train.both <- ovun.sample(Gender ~ ., data = train, method = "both", p = 0.5)$data
train.rose <- ROSE(Gender ~ ., data = train)$data

reg.none <- logreg(train, test)
reg.over <- logreg(train.over, test)
reg.under <- logreg(train.under, test)
reg.both <- logreg(train.both, test)
reg.rose <- logreg(train.rose, test)

tree.none <- randforest(train, test)
tree.over <- randforest(train.over, test)
tree.under <- randforest(train.under, test)
tree.both <- randforest(train.both, test)
tree.rose <- randforest(train.rose, test)

roc.curve(test$Gender, tree.rose$pred)

# Synthetic females

bf_subset <- bf[sample(nrow(bf), 100000), ]
ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))

train <- as.data.frame(bf_subset[ran,])
train_male <- train[which(train$Gender == 0),]
train_female <- train[which(train$Gender == 1),]
train_female <- SMOTE(train_female, 2, 5)
train <- rbind(train_male, train_female)

test <- bf_subset[-ran,]

synthetic <- logreg(train, test)