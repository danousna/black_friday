library(class)
library(ggplot2)
library(reshape2)

library(devtools)
library(ggbiplot)

source('./load_data.R')

categories =  c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18')
user_pc <- sapply(unique(bf$User_ID), function(user){
  user_data <- bf[bf$User_ID == user,]
  cat_count <- matrix(0, length(categories), 1, dimnames = list(categories, c(user)))
  #print(as.numeric(rownames(user_data)))
  for (line in as.numeric(rownames(user_data))){
    PC_1 <- as.numeric(bf[line,]$Product_Category_1)
    cat_count[PC_1] <- cat_count[PC_1] + 1
    if (! is.na(bf[line,]$Product_Category_2)){
      PC_2 <- as.numeric(bf[line,]$Product_Category_2)
      cat_count[PC_2] <- cat_count[PC_2] + 1
    }
    if (! is.na(bf[line,]$Product_Category_3)){
      PC_3 <- as.numeric(bf[line,]$Product_Category_3)
      cat_count[PC_3] <- cat_count[PC_3] + 1
    }
  }
  #cat_count[length(categories) + 1] = user
  cat_count
})

n_user_pc <- t(apply(user_pc, 2, function(line){
  squared_line <- sapply(line, function(x){x*x})
  s <- sqrt(sum(squared_line))
  sapply(line, function(x){x/s})
}))
n_user_pc_df <- as.data.frame(n_user_pc)
rownames(n_user_pc_df) <- unique(bf$User_ID)


n_user_pc_df.pca <- prcomp(n_user_pc_df, center = TRUE)
user_cat <- sapply(as.list(rownames(n_user_pc_df)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})
pca.plot <- ggplot(as.data.frame(n_user_pc_df.pca$x), aes(x=PC1, y=PC2, color=user_cat)) +
  geom_point()
ggsave('figures/pca_categories.pdf', plot = pca.plot, device='pdf', width = 4, height = 2.5)

pca.plot.axis <- ggbiplot(n_user_pc_df.pca, alpha=1, groups=user_cat) + coord_equal(ratio = 0.70)
pca.plot.axis$layers <- c(pca.plot.axis$layers[[2]], pca.plot.axis$layers[[3]], pca.plot.axis$layers[[1]])
ggsave('figures/pca_categories_axis.pdf', plot = pca.plot.axis, device='pdf', width = 5, height = 3.5)

user_gender <- sapply(as.list(rownames(n_user_pc_df)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})

eval_best_k_knn <- function(N=10, Ks=seq(1, 10, 1)){
  res_error <- rep(0, length(Ks))
  res_prod <- rep(0, length(Ks))
  res_MM <- rep(0, length(Ks))
  res_FF <- rep(0, length(Ks))
  i <- 1
  for (k in Ks){
    avg_error <- 0
    avg_prod <- 0
    avg_MM <- 0
    avg_FF <- 0
    for (n in 1:N){
      ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))
      user_train <- as.data.frame(n_user_pc_df[ran,])
      user_test <- n_user_pc_df[-ran,]
      user_train_cat <- user_gender[ran]
      user_test_cat <- user_gender[-ran]
      pr <- knn(user_train,user_test,cl=user_train_cat,k=k)
      avg_error <- avg_error + (1-mean(pr == user_test_cat)) / N
      
      confusion_matrix_knn <- table(pr,user_test_cat)
      confusion_matrix_knn_normalised <- t(apply(confusion_matrix_knn, 1, function(line){
        line / sum(line)
      }))
      avg_prod <- avg_prod + confusion_matrix_knn_normalised[1,1] * confusion_matrix_knn_normalised[2,2] / N
      avg_MM <- avg_MM + confusion_matrix_knn_normalised[2,2] / N
      avg_FF <- avg_FF + confusion_matrix_knn_normalised[1,1] / N
    }
    res_prod[i] <- avg_prod
    res_error[i] <- avg_error
    res_MM[i] <- avg_MM
    res_FF[i] <- avg_FF
    i<-i+1
  }
  res <- NULL
  res$prod <- res_prod
  res$error <- res_error
  res$MM <- res_MM
  res$FF <- res_FF
  
  res
}
Ks <- seq(1, 550, 20)
k_errors <- eval_best_k_knn(Ks=Ks, N=15)

k_errors.plot <- ggplot(mapping = aes(x = Ks[1:length(k_errors$prod)])) +
  geom_line(aes(y = k_errors$prod, colour = "Product")) + 
  geom_line(aes(y = k_errors$MM, colour = "MM")) + 
  geom_line(aes(y = k_errors$FF, colour = "FF")) + 
  geom_line(aes(y = k_errors$error, colour = "Error")) +
  xlab('k') +
  ylab('')
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
pr <- knn(user_train,user_test,cl=user_train_cat,k=25)
confusion_matrix_knn <- table(pr,user_test_cat)

confusion_matrix_knn_normalised <- t(apply(confusion_matrix_knn, 1, function(line){
  line / sum(line)
}))
melted_confusion_matrix <- melt(confusion_matrix_knn_normalised)
colnames(melted_confusion_matrix) <- c('Réalité', 'Prédiction', 'values')

confusion_matrix.plot <- ggplot(data = melted_confusion_matrix, aes(x=Réalité, y=Prédiction, fill=values)) + 
  geom_tile(aes(fill = values)) + 
  geom_text(aes(label = round(values, 2)))
print(confusion_matrix.plot)
ggsave('figures/confusion_matrix_categories.pdf', plot = confusion_matrix.plot, device='pdf', width = 4, height = 3)
