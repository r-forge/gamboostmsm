Y.q <- function(dat, tra){
  ## step 1/2: temporal relations only.
  risk.sets <- .cpp_helpfun_risk_sets_wrt_times(timeslR = dat$entry, 
                                                timesrR = dat$exit)
  ## step 2/2: dependencies in terms of states.
  from <- dat$from
  to <- dat$to
  possible.trans.from.and.to <- cbind(rep(dimnames(tra)[[1]], each=ncol(tra))[which(tra)], 
                                      rep(dimnames(tra)[[2]], ncol(tra))[which(tra)])
  possible.trans.from.and.to <- apply(possible.trans.from.and.to, MARGIN=2, 
                                      FUN=as.numeric)
  K <- nrow(possible.trans.from.and.to)
  numerator <- denominator <- vector("list", K)
  for(hi in 1:K){
    numerator.denominator <- .cpp_helpfun_risk_sets_wrt_times_and_states(risksetsR = risk.sets, 
                                   fromR = from, 
                                   toR = to, 
                                   fromqR = possible.trans.from.and.to[hi, 1], 
                                   toqR = possible.trans.from.and.to[hi, 2])
    numerator[[hi]] <- numerator.denominator$numerator
    denominator[[hi]] <- numerator.denominator$denominator
    rm(numerator.denominator)
  }
  return(list(numerator=numerator, denominator=denominator))
}