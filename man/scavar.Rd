\name{scavar}
\alias{scavar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Scaling Item Variances
}
\description{
Function scales variances of each item in model.
To get scaled variances for latent variables, run the original model again, but fix each item with the scaled variance provided by this function.
}
\usage{
scavar(LambdaMatrix, col)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LambdaMatrix}{
get LambdaMatrix directly from your lavaan object (fit_model <- sem(model, ...)).

LambdaMatrix <- lavInspect(fit_model, what = "std.all")

}
  \item{col}{
select column for which latent variable the variances should be scaled.

scavar(LambdaMatrixlambda, 2)
}
}
\details{
Scaling variances makes values comparable.
}
\value{
List of the scaled varaince for each item is returned.
}
\references{
Schweizer, K., & Troche, S. (2019). The EV scaling method for variances of latent variables. Methodology.
}
\author{
Helene M. von Gugelberg
}
\note{
%%  ~~further notes~~
}

\examples{

function (LambdaMatrix, col) 
}
