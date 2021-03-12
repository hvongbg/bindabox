\name{cody}
\alias{cody}

\title{
correlation covariance matrix of binary data AND continous data
}
\description{
calculation of variance covariance matrix for continous data, covariance for continous and binary data, and covarniance of binary data
}
\usage{
cody(my_data, Item = NULL, continuous = NULL, output_matrix = NULL, Item_SD = NULL)
}

\arguments{
  \item{my_data}{
my_data is the dataframe entered, first the variables with binary data, then continous variables and no ID numbers. Must be a df or matrix
}
\item{Item}{
number of binary Items in dataframe
}

\item{continuous}{
number of continuous variables in dataframe
}
  \item{output_matrix}{
the matrix created with default name output_matrix unless other name is provided
}
   \item{Item_SD}{
object is a list of SD of the propability of answering any two items correct
  }
}
\details{
dataframe provided has to start with binary items followed by continous variables. No other variables are allowed 
}


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}

