\name{multistate}
\alias{multistate}
\title{Family for Multi State Models}
\description{This function implements a family for
  fitting multi state models in mboost.}
\usage{
multistate(numerator, denominator, hi.for.log.pl)
}
\arguments{
\item{numerator}{ an indicator matrix.}  
\item{denominator}{ a list of length \emph{n} of vectors containing
  indexes addressing the respective individuals at risk.}
\item{hi.for.log.pl}{ a matrix with two columns taking containing the
  values of the initial states and the final states for all \emph{Q} possible transitions.}
}
\details{This function implements a family for multi state models and
  will only be used inside the gamboost function.}
\value{ Functions to be used inside gamboost.
}
\author{Holger Reulen}
