scavar<- function(LambdaMatrix, col) {
  i <- 1
  Item_count <- nrow(LambdaMatrix)
  Lambda_scaled <- length(LambdaMatrix)
  LamOld <- length(LambdaMatrix)
  
  for (i in 1:Item_count) {
    LamOld[i] <- ((LambdaMatrix[i, col])^2)
  }
  overall <-sum(LamOld)
  
  l<-1
  for (l in 1:Item_count) {
    Lambda_scaled[l] <- sqrt(LamOld[l] / overall)
    
  }
  cat("Scaling applied according to Schweizer & Troche (2019)")
  Lambda_scaled
  
}
