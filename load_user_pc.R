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

n_user_pc_df_with_index <- data.frame(n_user_pc_df)
n_user_pc_df_with_index$User_ID <- rownames(n_user_pc_df)
User_all_info <- merge(n_user_pc_df_with_index, purchase_sum, by="User_ID")
User_all_info$User_ID <- NULL
