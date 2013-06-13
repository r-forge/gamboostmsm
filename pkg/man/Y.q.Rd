\name{Y.q}
\alias{Y.q}
\title{At Risk Indicators}
\description{Calculate at risk indicators used for working response and partial likelihood calculation.}
\usage{
Y.q(dat, tra)
}
\arguments{
\item{dat}{ a data set including mandatory variables \code{entry} (\bold{left truncation times}),
  \code{exit} (\bold{event or censoring times}), \code{from} (\bold{initial state}), and \code{to} (\bold{final state}).}  
\item{tra}{ a logical matrix containing the possible transitions. Note
  that \code{dimnames} have to be equal \code{as.character} versions of
  the unique values in \code{dat$from} and \code{dat$to}. Different 
  values on \code{unique(dat$from)} are seen as denoting right censoring.}
}
\details{This function calculates the values of the transition and
  individual specific risk indicators: These have to get calculated once
  before the boosting estimation and are used in each of the boosting
  steps for calculation of current loss and working response, respectively.}
\value{
A list of length \emph{Q} of lists of length \emph{2} including elements
\item{nominator}{ zero one \emph{n}-by-\emph{n} square matrices indicating whether 
  individual \emph{i} is at risk for event \emph{q} at exit time of 
  individual \emph{j}.}
\item{denominator}{ a vector of length \emph{n} containing the 
  respective event types of all events. Right censored observations 
  will be coded as as \code{NA}, but treated adequately in \code{multistate}. 
  Note that \code{denominator} will be only needed for calculation of working
  response values.}
}
\author{Holger Reulen}
\examples{
\dontrun{Y.q(data, transitionmatrix)}
}
