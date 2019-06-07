library(tidyverse)

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
bf[which(bf$Gender==2),]$Gender <- 0
bf$Gender <- as.factor(bf$Gender)

bf_male <- bf[which(bf$Gender == 0),]
bf_female <- bf[which(bf$Gender == 1),]

# MALE IS 0, FEMALE IS 1

logreg <- function(train, test) {
  model <- glm(Gender ~., family = binomial(link='logit'), data = train)

  predictions <- predict(model, newdata = test, type = 'response')
  predictions <- ifelse(predictions > 0.5,1,0)
  
  accuracy <- 1 - mean(predictions != test$Gender)
  
  confusion_matrix <- table(predictions, test$Gender)
  confusion_matrix_melted <- apply(confusion_matrix, 2, function(line){
    line / sum(line)
  })
  
  out <- NULL
  out$model <- model
  out$accuracy <- accuracy
  out$pred <- predictions
  out$confusion_matrix <- confusion_matrix
  out$confusion_matrix_melted <- confusion_matrix_melted
  
  out
}

# Naïve 

bf_subset <- bf[sample(nrow(bf), 100000), ]
ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))

naive <- logreg(train = as.data.frame(bf_subset[ran,]), bf_subset[-ran,])

melted_confusion_matrix <- melt(melted_confusion_matrix)
colnames(melted_confusion_matrix) <- c('Réalité', 'Prédiction', 'values')
confusion_matrix_naive.plot <- ggplot(data = melted_confusion_matrix, aes(x=Réalité, y=Prédiction, fill=values)) + 
  geom_tile(aes(fill = values)) + 
  geom_text(aes(label = round(values, 2)))
ggsave('figures/confusion_matrix_logreg_naive.pdf', plot = confusion_matrix_naive.plot, device='pdf', width = 4, height = 3)
  
# Fifty

bf_subset <- bf_male[sample(nrow(bf_male), 100000), ]
bf_subset <- rbind(bf_subset, bf_female[sample(nrow(bf_female), 100000), ])

ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))

fifty <- logreg(train = as.data.frame(bf_subset[ran,]), bf_subset[-ran,])

# Clones

bf_subset <- bf_male
# We clone female x3 to reach parity with males.
bf_subset <- rbind(bf_subset, bf_female)
bf_subset <- rbind(bf_subset, bf_female)
bf_subset <- rbind(bf_subset, bf_female)

ran <- sample(1:nrow(bf_subset), 0.80 * nrow(bf_subset))

clones <- logreg(train = as.data.frame(bf_subset[ran,]), bf_subset[-ran,])