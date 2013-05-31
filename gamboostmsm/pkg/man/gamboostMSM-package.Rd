\name{gamboostMSM-package}
\alias{gamboostMSM-package}
\docType{package}
\title{Component-wise Functional Gradient Descent Boosting of Multi State Models}
\description{Gradient boosting for Cox-type multi state models: optimizing negative partial log-likelihood using component- and transition-wise base-learners.}
\details{This package provides functions to fit Cox-type multi state models using the functional gradient descent boosting algorithm as implemented in the package mboost. Therefor, a Family() for fitting these models is given, including negative log partial likelihood of a Cox-type multi state model as loss function and its negative first partial derivative with respect to the linear predictor as working response.}
\author{Holger Reulen}
\references{Andersen, P.K., Borgan, O., Gill, R.D., and Keiding, N. (1993) Statistical Models Based on Counting Processes. Springer Series in Statistics, Springer-Verlag New York. 
Buehlmann, P. and Hothorn, T. (2007) Boosting Algorithms: Regularization, Prediction and Model Fitting (with Discussion). Statistical Science, 22(4), p. 477--505.
Hothorn, T., Buehlmann, P., Kneib, T., Schmid, M., and Hofner, B. (2012) mboost: Model-Based Boosting, R package version 2.2-0. http://CRAN.R-project.org/package=mboost.
Kneib, T., Hothorn, T., and Tutz, G. (2009) Variable Selection and Model Choice in Geoadditive Regression Models. BIOMETRICS 65, p. 626-634.
Ridgeway, G. (1999) The state of boosting. Computing Science and Statistics 31, p. 172-181.}
%% ~~ Optionally other standard keywords, one per line, from file KEYWORDS in the %% R documentation directory ~~
\keyword{ package }
\seealso{\code{\link{mboost}}}
