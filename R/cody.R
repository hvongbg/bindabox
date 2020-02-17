cody <- function(my_data, Item = NULL, continuous = NULL, output_matrix = NULL, Item_SD = NULL){
  
  #my_data<-dt
  if (is.data.frame(my_data)==FALSE) {
    stop("candy ERROR: data supplied is not a dataframe")
  }
  if(sum(is.na(my_data))!=0) {
    stop("candy ERROR: missing values found. candy cannot handle missing values")
  }
  
  zahler            <- 1
  
  if(missing(Item)) {
    Item_Anzahl <- length(my_data)
  } else {Item_Anzahl <- Item}
  
  continuous<-continuous
  Teilnehmer_Anzahl <- nrow(my_data) 
  #my_array will contain original datavalues and compared item values
  my_array          <- matrix(NA, ncol = (Item_Anzahl * (Item_Anzahl+1))/2,
                              nrow = Teilnehmer_Anzahl)
  #prob_mat will contain probability values of correct answers
  prob_mat          <- matrix(NA, ncol = Item_Anzahl + continuous,
                              nrow = Item_Anzahl + continuous)
  #for SD
  summer                <-length(my_data)
  
  my_array          <-as.data.frame(my_array)
  prob_mat          <-as.data.frame(prob_mat)
  
  my_array[,1:Item_Anzahl]<-my_data[,1:Item_Anzahl]
  #fills first colums with original dataframe
  
  zahler            <- Item_Anzahl + 1
  
  for (i in 1:(Item_Anzahl-1)){
    for (j in (i+1):Item_Anzahl) {
      my_array[,zahler] <- (my_data[,i] * my_data[,j])
      zahler = zahler + 1
    }
  }
  
  wahr <- colSums(my_array/Teilnehmer_Anzahl)
  #probability of correct answers for each combination
  
  k <- Item_Anzahl + 1
  first <- 1
  second <- 2
  
  #first and second comparison
  #p each additional col in wahr
  for (p in k:length(wahr)) {
    prob_mat[second,first]<- wahr[p] - (wahr[first]* wahr[second])
    second <- second + 1
    if (second==k){
      first <- first + 1
      second <- first + 1
    }
  }
  
  for (i in 1:Item_Anzahl){
    prob_mat[i,i] <- wahr[i]*(1-wahr[i])
  }
  
  for (i in 1:Item_Anzahl){
    summer[i] <- sqrt(wahr[i]*(1-wahr[i]))
  }
  
  #naming output
  
  namen <- paste0("A", 1:length(prob_mat), collapse = NULL)
  
  
  
  prob_mat[is.na(prob_mat)]<-""
  
  rownames(prob_mat)<-namen
  
  out_mat <- matrix(data = NA, nrow = dim(prob_mat)[1], ncol = dim(prob_mat)[2])
  
  rownames(out_mat)<-namen
  colnames(out_mat)<-namen
  for (i in 1:dim(prob_mat)[2]) {
    out_mat[,i] <- c(as.numeric(prob_mat[[i]]))
  }
  
  
  
  
  
  if(missing(output_matrix)) {
    assign("output_matrix", out_mat, envir=globalenv())
  } else {assign(output_matrix[], out_mat, envir=globalenv())
  }
  
  
  
  if(missing(continuous)) {
    break
  }else{ 
    
    r <- 1
    i <- 1
    for (j in 1:continuous) {
      
      for (i in 1:Item_Anzahl) {
        
        prob_mat[r+Item_Anzahl,i]<-cov(my_data[ , i], my_data[ , r+Item_Anzahl])
        
        
      }
      
      
      
      r <- r+1    
    }
    
    
    
    if (continuous>1) {
      
      a <- 1
      for (i in 1:continuous) {
        for (p in 1:a) {
          
          
          prob_mat[i + Item_Anzahl, Item_Anzahl + p] <-cov(my_data[, Item_Anzahl + i], my_data[ , Item_Anzahl + p])
          
          
        } 
        a<-a+1
        
        
      } 
      
    }
    
    
  }
  
  #naming output
  
  namen <- paste0("A", 1:length(prob_mat), collapse = NULL)
  
  
  
  prob_mat[is.na(prob_mat)]<-""
  
  rownames(prob_mat)<-namen
  
  out_mat <- matrix(data = NA, nrow = dim(prob_mat)[1], ncol = dim(prob_mat)[2])
  
  rownames(out_mat)<-namen
  colnames(out_mat)<-namen
  for (i in 1:dim(prob_mat)[2]) {
    out_mat[,i] <- c(as.numeric(prob_mat[[i]]))
  }
  
  
  
  
  if(missing(Item_SD)) {
    assign("Item_SD", summer, envir=globalenv())
  } else {assign(Item_SD[], summer, envir=globalenv())
  }
  
  if(missing(output_matrix)) {
    assign("output_matrix", prob_mat, envir=globalenv())
  } else {assign(output_matrix[], prob_mat, envir=globalenv())}
  
}

