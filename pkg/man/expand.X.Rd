\name{expand.X}
\alias{expand.X}
\title{Expand the Design Matrix}
\description{Expand the design matrix for structured component- and
  transition-wise multi state model regression.}
\usage{
expand.X(dat, id.index, trans.indicators.index, covariate.index)
}
\arguments{
\item{dat}{ a data set.}  
\item{id.index}{ an integer giving the column of the id variable in
  \code{dat}.}
\item{trans.indicators.index}{ an integer vector giving the columns of the
  transition indicators in \code{dat}.}
\item{covariate.index}{ an integer vector giving the columns of the
  cavariate variables in \code{dat}.}
}
\details{This function expands the covariate variables in a suitable way
  to get a matrix that contains products of transition indicators and
  covariate variables, similar to parts of a design matrix for varying coefficients.}
\value{
A data set
\item{expanded.dat}{ a data set. }
}
\author{Holger Reulen}
\examples{
\dontrun{expand.X(data, 1, 2:5, 6:7)}
}
