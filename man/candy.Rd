\name{candy}
\alias{candy}

\title{
correlation covariance matrix of binary data
}
\description{
the function calculates for each row, the probability of correct anwsers for each item and the probabilties for correct answers for the combination any of two items. then these probabilties are used to create the correltaion and covariance matrix --  necessary to calcualte fixed-link models in SEM
}
\usage{
candy(my_data, NAME)
}

\arguments{
  \item{my_data}{
my_data is the dataframe entered, containing only the binary data and no ID numbers. Must be a df or matrix
}
  \item{NAME}{
the matrix created is called NAME
}
}
\details{

}
\value{
 NAME -- matrix containing calcualted values
}
\references{

}
\author{

}
\note{
goal is to create a toolbox for handling binary data
}



\seealso{

}
\examples{


a <-c(1,0,0,1)
b <-c(1,1,0,0)
c <-c(1,1,1,1)
data <- rbind(a, b, c) #example data

candy(data) #call function

NAME #matrix with saved values

StandD #SD for each item
}
