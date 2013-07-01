\name{breslow}
\alias{breslow}
\title{Breslow estimator for cumulative baseline hazard rate}
\description{This function calculates the Breslow estimator for the
  cumulative hazard rate, given a fitted multi state model.}
\usage{
breslow(model, m, d, from.q, to.q)
}
\arguments{
\item{model}{ a fitted multi state model using \code{gamboost(..., family=multistate(...))}.}  
\item{m}{ an integer defing at which boosting iteration to evaluate.}
\item{d}{ a data frame including variables \code{entry} (\bold{left truncation time}),
  \code{exit} (\bold{event time}), \code{from} (\bold{initial state}),
  and \code{to} (\bold{final state}),}
\item{from.q}{ denoting the initial state of transition/event q,}
\item{to.q}{ denoting the initial state of transition/event q.}
}
\details{This function calculates the Breslow estimator for the
  cumulative hazard rate, given a fitted multi state model and a
  given transition q.}
\value{ 
A list of length 2 including elements
\item{times}{ a vector of observed event times,}
\item{A}{ a vector of calculated cumulative hazard rate values.}
}
\author{Holger Reulen}
\examples{
\dontrun{breslow(model, m, data, from.q, to.q)}
}
