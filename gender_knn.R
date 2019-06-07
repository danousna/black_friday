library(class)
library(ggplot2)
library(reshape2)

library(devtools)
library(ggbiplot)

source('./load_data.R')

source('./load_user_pc.R')

n_user_pc_df.pca <- prcomp(n_user_pc_df, center = TRUE)
user_cat <- sapply(as.list(rownames(n_user_pc_df)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})
pca.plot <- ggplot(as.data.frame(n_user_pc_df.pca$x), aes(x=PC1, y=PC2, color=user_cat)) +
  geom_point()
ggsave('figures/pca_categories.pdf', plot = pca.plot, device='pdf', width = 4, height = 2.5)

pca.plot.axis <- ggbiplot(n_user_pc_df.pca, alpha=1, groups=user_cat) + coord_equal(ratio = 0.70)
pca.plot.axis$layers <- c(pca.plot.axis$layers[[2]], pca.plot.axis$layers[[3]], pca.plot.axis$layers[[1]])
ggsave('figures/pca_categories_axis.pdf', plot = pca.plot.axis, device='pdf', width = 5, height = 3.5)

scatter3D(n_user_pc_df.pca$x[,1], n_user_pc_df.pca$x[,2], n_user_pc_df.pca$x[,3],
          colvar = NULL, col = c("#1B9E77", "#D95F02"), col.var = as.integer(user_cat),
          theta = 275, phi = 10)

user_gender <- sapply(as.list(rownames(n_user_pc_df)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})

eval_best_k_knn <- function(N=10, Ks=seq(1, 10, 1)){
  res_error <- rep(0, length(Ks))
  res_prod <- rep(0, length(Ks))
  res_MM <- rep(0, length(Ks))
  res_FF <- rep(0, length(Ks))
  i <- 1
  for (k in Ks){
    avg_error <- 0
    avg_MM <- 0
    avg_FF <- 0
    for (n in 1:N){
      ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))
      user_train <- as.data.frame(n_user_pc_df[ran,])
      user_test <- n_user_pc_df[-ran,]
      user_train_cat <- user_gender[ran]
      user_test_cat <- user_gender[-ran]
      user_train_F <- user_train[user_train_cat == 'F',]
      user_train <- rbind(user_train, user_train_F)
      user_train <- rbind(user_train, user_train_F)
      user_train_cat <- factor(c(as.character(user_train_cat), as.character(rep('F', 2*sum(user_train_cat == 'F')))), levels=levels(user_train_cat))
      
      pr <- knn(user_train,user_test,cl=user_train_cat,k=k)
      avg_error <- avg_error + (1-mean(pr == user_test_cat)) / N
      
      confusion_matrix_knn <- table(pr,user_test_cat)
      confusion_matrix_knn_normalised <- t(apply(confusion_matrix_knn, 2, function(line){
        line / sum(line)
      }))
      avg_MM <- avg_MM + confusion_matrix_knn_normalised[2,2] / N
      avg_FF <- avg_FF + confusion_matrix_knn_normalised[1,1] / N
    }
    res_error[i] <- avg_error
    res_MM[i] <- avg_MM
    res_FF[i] <- avg_FF
    i<-i+1
  }
  res <- NULL
  res$error <- res_error
  res$MM <- res_MM
  res$FF <- res_FF
  
  res
}

Ks <- seq(1, 50, 1)
k_errors <- eval_best_k_knn(Ks=Ks, N=25)

k_errors.plot <- ggplot(mapping = aes(x = Ks[1:length(k_errors$MM)])) +
  geom_line(aes(y = k_errors$MM, colour = "MM")) + 
  geom_line(aes(y = k_errors$FF, colour = "FF")) + 
  geom_line(aes(y = k_errors$error, colour = "Error")) +
  xlab('k') +
  ylab('')
print(k_errors.plot)
ggsave('figures/knn_error_large.pdf', plot = k_errors.plot, device='pdf', width = 4, height = 3)

Ks <- seq(1, 100, 2)
k_errors <- eval_best_k_knn(Ks=Ks, N=50)

k_errors.plot <- ggplot(mapping = aes(x = Ks[1:length(k_errors$prod)])) +
  geom_line(aes(y = k_errors$prod, colour = "Product")) + 
  geom_line(aes(y = k_errors$MM, colour = "MM")) + 
  geom_line(aes(y = k_errors$FF, colour = "FF")) + 
  geom_line(aes(y = k_errors$error, colour = "Error")) +
  xlab('k') +
  ylab('')
print(k_errors.plot)
ggsave('figures/knn_error.pdf', plot = k_errors.plot, device='pdf', width = 4, height = 3)

ggplot(mapping = aes(x = Ks[1:length(k_errors$prod)])) +
  geom_line(aes(y = k_errors$error, colour = "Error")) +
  xlab('k') +
  ylab('')

ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))
user_train <- as.data.frame(n_user_pc_df[ran,])
user_test <- n_user_pc_df[-ran,]
user_train_cat <- user_gender[ran]
user_test_cat <- user_gender[-ran]

#user_train_F <- user_train[user_train_cat == 'F',]
#user_train <- rbind(user_train, user_train_F)
#user_train <- rbind(user_train, user_train_F)
#user_train_cat <- factor(c(as.character(user_train_cat), as.character(rep('F', 2*sum(user_train_cat == 'F')))), levels=levels(user_train_cat))

pr <- knn(user_train,user_test,cl=user_train_cat,k=18)
confusion_matrix_knn <- table(pr,user_test_cat)
print(confusion_matrix_knn)

confusion_matrix_knn_normalised <- apply(confusion_matrix_knn, 2, function(line){
  line / sum(line)
})
confusion_matrix_knn_normalised
melted_confusion_matrix <- melt(confusion_matrix_knn_normalised)
colnames(melted_confusion_matrix) <- c('Réalité', 'Prédiction', 'values')

confusion_matrix.plot <- ggplot(data = melted_confusion_matrix, aes(x=Prédiction, y=Réalité, fill=values)) + 
  geom_tile(aes(fill = values)) + 
  geom_text(aes(label = round(values, 2)))
print(confusion_matrix.plot)
ggsave('figures/confusion_matrix_categories.pdf', plot = confusion_matrix.plot, device='pdf', width = 4, height = 3)

inertia <- function (df){
  mu <- apply(df, 2, mean)
  mean(apply(df, 1, function(x){
    dist(rbind(mu, x))
  }))
}
inertia(n_user_pc_df[user_gender == 'M',])
inertia(n_user_pc_df[user_gender == 'F',])

### SMOTE

eval_best_k_knn_SMOTE <- function(user_SMOTE, user_gender_SMOTE,N=10, Ks=seq(1, 10, 1), replication_factor=1){
  res_error <- rep(0, length(Ks))
  res_prod <- rep(0, length(Ks))
  res_MM <- rep(0, length(Ks))
  res_FF <- rep(0, length(Ks))
  i <- 1
  for (n in 1:N){
    ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))
    
    user_train <- as.data.frame(n_user_pc_df[ran,])
    user_train_M <- user_train[user_gender[ran] == 'M',]
    user_train_F <- user_train[user_gender[ran] == 'F',]
    user_train_F_SMOTE <- SMOTE(user_train_F, replication_factor, 5)
    user_train_SMOTE <- rbind(user_train_M, user_train_F_SMOTE)
    user_train_cat <- factor(c(as.character(rep('M', dim(user_train_M)[1])), as.character(rep('F', dim(user_train_F_SMOTE)[1]))), levels=levels(user_train_cat))
    
    user_test <- n_user_pc_df[-ran,]
    user_test_cat <- user_gender[-ran]
    i <- 1
    for (k in Ks){
      avg_error <- 0
      avg_MM <- 0
      avg_FF <- 0
      pr <- knn(user_train_SMOTE ,user_test,cl=user_train_cat,k=k)
      
      confusion_matrix_knn <- table(pr,user_test_cat)
      res_error[i] <- res_error[i] + (mean(pr == user_test_cat)) / N
      confusion_matrix_knn_normalised <- t(apply(confusion_matrix_knn, 2, function(line){
        line / sum(line)
      }))
      res_MM[i] <- res_MM[i] + confusion_matrix_knn_normalised[2,2] / N
      res_FF[i] <- res_FF[i] + confusion_matrix_knn_normalised[1,1] / N
      i<-i+1
    }
  }
  res <- NULL
  res$error <- res_error
  res$MM <- res_MM
  res$FF <- res_FF
  
  res
}

replication_factor <- 2

#user_M <- n_user_pc_df[user_gender == 'M',]
#user_F <- n_user_pc_df[user_gender == 'F',]
#user_F_SMOTE <- SMOTE(user_F, replication_factor, 10)

#user_SMOTE <- rbind(user_M, user_F_SMOTE)
#user_gender_SMOTE <- factor(c(as.character(rep('M', dim(user_M)[1])), as.character(rep('F', dim(user_F_SMOTE)[1]))), levels=levels(user_train_cat))

ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))

user_train <- as.data.frame(n_user_pc_df[ran,])
user_train_M <- user_train[user_gender[ran] == 'M',]
user_train_F <- user_train[user_gender[ran] == 'F',]
user_train_F_SMOTE <- SMOTE(user_train_F, replication_factor, 5)
user_train_SMOTE <- rbind(user_train_M, user_train_F_SMOTE)
user_train_cat <- factor(c(as.character(rep('M', dim(user_train_M)[1])), as.character(rep('F', dim(user_train_F_SMOTE)[1]))), levels=levels(user_train_cat))

user_test <- n_user_pc_df[-ran,]
user_test_cat <- user_gender[-ran]

pr <- knn(user_train_SMOTE ,user_test,cl=user_train_cat,k=80)
confusion_matrix_knn <- table(pr,user_test_cat)
print(confusion_matrix_knn)
confusion_matrix_knn_normalised <- apply(confusion_matrix_knn, 2, function(line){
  line / sum(line)
})
confusion_matrix_knn_normalised
melted_confusion_matrix <- melt(confusion_matrix_knn_normalised)
colnames(melted_confusion_matrix) <- c('Réalité', 'Prédiction', 'values')

confusion_matrix.plot <- ggplot(data = melted_confusion_matrix, aes(x=Prédiction, y=Réalité, fill=values)) + 
  geom_tile(aes(fill = values)) + 
  geom_text(aes(label = round(values, 2)))
print(confusion_matrix.plot)

replication_factor <- 2
Ks <- seq(1, 100, 1)
k_errors <- eval_best_k_knn_SMOTE(user_SMOTE, user_gender_SMOTE,Ks=Ks, N=3, replication_factor=replication_factor)

k_errors.plot <- ggplot(mapping = aes(x = Ks[1:length(k_errors$MM)])) +
  geom_line(aes(y = k_errors$MM, colour = "MM")) + 
  geom_line(aes(y = k_errors$FF, colour = "FF")) + 
  geom_line(aes(y = k_errors$error, colour = "Good")) +
  xlab('k') +
  ylab('')
print(k_errors.plot)
ggsave('figures/k_SMOTE_3.pdf', plot = k_errors.plot, device='pdf', width = 4, height = 3)

