distXY <- function(X, Y) {
  nx <- dim(X)[1]
  ny <- dim(Y)[1]
  h.x <- rowSums(X^2)
  h.y <- rowSums(Y^2)
  ones.x <- rep(1, nx)
  ones.y <- rep(1, ny)
  D2xy <- h.x %*% t(ones.y) - 2 * X %*% t(Y) + ones.x %*% t(h.y)
}

kppv <- function(X, K)
{
  X <- as.matrix(X)
  
  napp <- dim(X)[1]
  p <- dim(X)[2]
  
  # calcul des distances 
  d2 <- distXY(X, X)
  d2sor <- t(apply(d2, 1, order))
  
  d2sor[,2:(K+1)]
}

SMOTE <- function(P, N, k){
  len_P <- nrow(P)
  kppv_points <- kppv(P, k)
  
  plus_decimal <- floor((N - floor(N)) * len_P)
  N <- floor(N)
  
  plus_decimal_elements <- sample(1:len_P, plus_decimal)
  
  print(plus_decimal_elements)
  
  Synthetic <- data.frame(matrix(ncol=dim(P)[2], nrow=0))
  colnames(Synthetic) <- colnames(P)
  new_index <- 1
  for ( index in 1:dim(P)[1]){
    if ( index %in% plus_decimal_elements ){
      n <- N + 1
    } else {
      n <- N
    }
    ran <- sample(1:k, n)
    x <- P[index,]
    sample_points <- P[kppv_points[index ,ran],]
    if (n > 0){
      for (j in 1:n){
        point <- sample_points[j,]
        alpha <- runif(1,0,1)
        for (l in 1:dim(P)[2]){
          Synthetic[new_index, l] <- alpha * as.numeric(x[l]) + (1-alpha) * as.numeric(point[l])
        }
        new_index <- new_index + 1
      }
    }
  }
  Synthetic
}
