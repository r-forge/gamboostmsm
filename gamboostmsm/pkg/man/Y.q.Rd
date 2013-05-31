\name{Y.q}
\alias{Y.q}
\title{At Risk Indicators}
\description{Calculate at risk indicators.}
\usage{
Y.q(dat, tra)
}
\arguments{
\item{dat}{ a data set including variables \code{entry} (\bold{left truncation time}),
  \code{exit} (\bold{event time}), \code{from} (\bold{initial state}), and \code{to} (\bold{final state}).}  
\item{tra}{ a logical matrix containing the possible transitions. note
  that \code{dimnames} have to be equal \code{as.character} versions of
  the unique values in \code{dat$from} and \code{dat$to}.}
}
\details{This function calculates the values of the transition and
  individual specific risk indicators: These have to get calculated once
before the boosting estimation and are used in each of the boosting
steps for calculating the current loss and working response, respectively.}
\value{
A list of length \emph{Q} of lists of length \emph{2} including elements
\item{nominator}{ an indicator matrix }
\item{denominator}{ a list of length \emph{n} of vectors containing
  indexes addressing the respective individuals at risk. Note that
  \code{denominator} will be only needed for calculation of working
  response values. }
}
\author{Holger Reulen}
\examples{
\dontrun{Y.q(data, transitionmatrix)}
}
