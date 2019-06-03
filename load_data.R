bf <- read.csv("dataset.csv", header=T)

bf$Occupation <- factor(bf$Occupation, levels = c('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20'))
bf$Marital_Status <- factor(bf$Marital_Status, levels = c('0', '1'))
bf$Product_Category_1 <- factor(bf$Product_Category_1, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18'))
bf$Product_Category_2 <- factor(bf$Product_Category_2, levels = c('2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18'))
bf$Product_Category_3 <- factor(bf$Product_Category_3, levels = c('3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18'))

purchase_sum = aggregate(
  bf$Purchase,
  list(User_ID = bf$User_ID, Gender = bf$Gender, City_Category = bf$City_Category, Age = bf$Age),
  sum
)
