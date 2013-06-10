multistate <- function(numerator = numerator, denominator = denominator, 
                       hi.for.log.pl = hi.for.log.pl){
  ############################################
  ## negative gradient of the loss function ##
  ############################################
  multistate.ngradient <- function(y, f, w=NULL){
    numerator.list <- numerator
    denominator.list <- denominator
    K <- length(numerator.list)
    if (length(f) == 1){f <- rep(f, length(y))}
    zi.list <- vector("list", K)
    for(hi in 1:K){
      zi.list[[hi]] <- .cpp_helpfun_z_i(etahat = f, 
                                        numerator = as.matrix(numerator.list[[hi]]), 
                                        denominator = denominator.list[[hi]])}
    rm(hi); rm(K)
    zi <- do.call(cbind, zi.list)
    rm(zi.list)
    zi <- apply(zi, MARGIN=1, FUN=sum)
    zi <- 1-zi
    zi <- zi - mean(zi)
    return(zi)}
  #########################################################################
  ## loss a.k.a. negative log likelihood of a cox type multi state model ##
  #########################################################################
  multistate.loss <- function(y, f, w=NULL){
    numerator.list <- numerator
    hi.for.log.pl <- hi.for.log.pl
    n <- length(hi.for.log.pl)
    K <- length(numerator.list)
    log.pl.list <- vector("list", K)
    for(hi in 1:K){
      log.pl.list[[hi]] <- .cpp_helpfun_log_pl(etahat = f, 
                                               numerator = numerator.list[[hi]])}
    log.pl.now <- NULL
    for(j in 1:n){
      log.pl.now <- c(log.pl.now, log.pl.list[[hi.for.log.pl[j]]][j])}
    return(-(log.pl.now))}
  #########################################
  ## construct new family using Family() ##
  #########################################
  Family(
    ngradient = multistate.ngradient,
    loss = multistate.loss,
    offset = function(y, w) 0,
    name = "family for multi state models.")}
