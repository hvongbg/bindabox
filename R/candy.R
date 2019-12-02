candy <- function(my_data, NAME){

#my_data<-dt

  if(sum(is.na(my_data))!=0) {
    stop("missing values found. candy cannot handle missing values")
  }

  if(any(my_data > 1)) {
    stop("binary values must be defined with 1 and 0. no other values permitted.
         entered data cannot contain ID identifiers--see example data bitest")
  }

  zahler            <- 1
  Item_Anzahl       <- length(my_data)
  Teilnehmer_Anzahl <- nrow(my_data)
  #my_array will contain original datavalues and compared item values
  my_array          <- matrix(NA, ncol = (Item_Anzahl * (Item_Anzahl+1))/2,
                              nrow = Teilnehmer_Anzahl)
  #prob_mat will contain probability values of correct answers
  prob_mat          <- matrix(NA, ncol = length(my_data),
                              nrow = length(my_data))
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

  NAME<<- prob_mat
  SD <<-summer

}



