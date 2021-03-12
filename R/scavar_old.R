scavar_old <- function(LambdaMatrix, col) {
  i <- 1
  Item_count <- nrow(LambdaMatrix)
  LamOld <- length(LambdaMatrix)
  for (i in 1:Item_count) {
    LamOld[i] <- ((LambdaMatrix[i, col])^2)
  }
  overall <-sqrt(sum(LamOld)/Item_count)
  Lambda <- length(Item_count) 
  l<-1
  for (l in 1:Item_count) {
    Lambda[l] <- LamOld[l] / overall
    
  }

  cat("Scaling applied according to Schweizer (2011)")
  Lambda

}

