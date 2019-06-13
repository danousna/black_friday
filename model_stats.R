model_stats <- function(expected, predicted) {
  accuracy <- 1 - mean(predicted != expected)
  
  confusion_matrix <- table(expected, predicted)
  confusion_matrix.scaled <- apply(confusion_matrix, 2, function(line){
    line / sum(line)
  })
  
  out <- NULL
  out$accuracy <- accuracy
  out$confusion_matrix <- confusion_matrix
  out$confusion_matrix.scaled <- confusion_matrix.scaled
  out$precision <- confusion_matrix[2,2] / sum(confusion_matrix[2,])
  out$recall <- confusion_matrix[2,2] / sum(confusion_matrix[,2])
  out$f1_score <- 2 * ((out$precision * out$recall) / (out$precision + out$recall))
  
  out
}