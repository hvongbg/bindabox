quic <- function(Item_count){
  quad<- length(Item_count)
  
  for (i in 1:Item_count) {
    quad[i] <- (i^2)/(Item_count^2)
  }
  
  quad 
}
