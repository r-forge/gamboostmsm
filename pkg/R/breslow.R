breslow <- function(model, m, d, from.q, to.q){
  rs.time.only <- .cpp_helpfun_risk_sets_wrt_times(timeslR = d$entry, timesrR = d$exit)
  rs <- .cpp_helpfun_breslow_risk_sets(risksetsR = rs.time.only, fromR = d$from, fromqR = from.q)
  etahat <- predict(model, aggregate="cumsum")
  etahat <- etahat[, m]
  ho1 <- .cpp_helpfun_breslow(etahat = etahat, numerator = rs[[1]])
  hi1 <- which((ho1 > 0) & (d$to == to.q))
  A <- rep(0, length(hi1))
  times <- d$exit[hi1]
  for(hi2 in 1:length(hi1)){
    time <- d$exit[hi1[hi2]]
    hi3 <- which(times <= time)
    A[hi2] <- sum(1/ho1[hi1[hi3]])
  }
  return(list(times = sort(times), A = A[order(times)]))
}
