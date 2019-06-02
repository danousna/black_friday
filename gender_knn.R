library(class)
library(ggplot2)
library(reshape2)

library(devtools)
install_github("vqv/ggbiplot")
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

ran <- sample(1:nrow(n_user_pc_df), 0.70 * nrow(n_user_pc_df))
user_train <- as.data.frame(n_user_pc_df[ran,])
user_test <- n_user_pc_df[-ran,]
user_train_cat <- sapply(as.list(rownames(user_train)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})
user_test_cat <- sapply(as.list(rownames(user_test)), function(id){purchase_sum[purchase_sum['User_ID'] == id,'Gender']})
pr <- knn(user_train,user_test,cl=user_train_cat,k=9)
confusion_matrix_knn <- table(pr,user_test_cat)

melted_confusion_matrix <- t(apply(confusion_matrix_knn, 1, function(line){
  line / sum(line)
}))
melted_confusion_matrix <- melt(confusion_matrix_knn_normalised)
colnames(melted_confusion_matrix) <- c('Réalité', 'Prédiction', 'values')

confusion_matrix.plot <- ggplot(data = melted_confusion_matrix, aes(x=Réalité, y=Prédiction, fill=values)) + 
  geom_tile(aes(fill = values)) + 
  geom_text(aes(label = round(values, 2)))
ggsave('figures/confusion_matrix_categories.pdf', plot = confusion_matrix.plot, device='pdf', width = 4, height = 3)
