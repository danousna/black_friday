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

# APPLY MODEL

library(tidyverse)

set.seed(9864)

bf_subset <- bf[sample(nrow(bf), 10000), ]

ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))
train <- as.data.frame(bf_subset[ran,])
test <- bf_subset[-ran,]

model <- glm(Marital_Status ~., family = binomial(link='logit'), data = train)
summary(model)

fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Marital_Status)
print(paste('Accuracy',1-misClasificError))

confusion_matrix <- table(fitted.results, test$Marital_Status)
