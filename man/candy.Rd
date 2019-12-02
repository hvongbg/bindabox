\name{candy}
\alias{candy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
correlation covariance matrix of binary data
}
\description{
the function calculates for each row, the probability of correct anwsers for each item and the probabilties for correct answers for the combination any of two items. then these probabilties are used to create the correltaion and covariance matrix --  necessary to calcualte fixed-link models in SEM
}
\usage{
candy(my_data, NAME)
}
%-
\arguments{
  \item{my_data}{
my_data is the dataframe entered, containing only the binary data and no ID numbers
}
  \item{NAME}{
the matrix created is called NAME
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 NAME -- matrix containing calcualted values
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{


a <-c(1,0,0,1)
b <-c(1,1,0,0)
c <-c(1,1,1,1)
data <- rbind(a, b, c) #example data

candy(data) #call function

NAME #matrix with saved values

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
