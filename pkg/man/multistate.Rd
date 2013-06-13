\name{multistate}
\alias{multistate}
\title{Family for Multi State Models}
\description{This function implements a family for
  fitting multi state models using \code{mboost}.}
\usage{
multistate(numerator, denominator, hi.for.log.pl)
}
\arguments{
\item{numerator}{ a list of length \emph{Q} containing elements: 
  zero one \emph{n}-by-\emph{n} square matrices indicating whether 
  individual \emph{i} is at risk for event \emph{q} at exit time of 
  individual \emph{j}.}  
\item{denominator}{ a list of length \emph{Q} containing elements: 
  a list of length \emph{n} of vectors containing indices addressing 
  the respective individuals at risk for calculating the denominator
  in the working response formula.}
\item{hi.for.log.pl}{ a vector of length \emph{n} containing the 
  respective event types of all events. Right censored observations 
  have to be stored as \code{NA}.}
}
\details{This function implements a family for multi state models and
  will be used inside the \code{gamboost} or \code{glmboost} function.}
\value{ Functions to be used inside gamboost.
}
\author{Holger Reulen}
