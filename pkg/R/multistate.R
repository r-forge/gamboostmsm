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
    zi <- do.call(cbind, zi.list)
    zi <- apply(zi, MARGIN=1, FUN=sum)
    event <- as.numeric(!is.na(hi.for.log.pl))    
    zi <- event - zi
    return(zi)}
  #############################################################
  ## negative log likelihood of a cox type multi state model ##
  #############################################################
  multistate.nlpl <- function(y, f, w=NULL){
    numerator.list <- numerator
    n <- length(hi.for.log.pl)
    K <- length(numerator.list)
    lpl.list <- vector("list", K)
    for(hi in 1:K){
      lpl.list[[hi]] <- .cpp_helpfun_log_pl(etahat = f, 
                                            numerator = numerator.list[[hi]])}
    lpl <- rep(0, n)
    event <- !is.na(hi.for.log.pl)
    for(hi in 1:n){
      if(event[hi]){
        lpl[hi] <- lpl.list[[hi.for.log.pl[hi]]][hi]
        }
      }
    return(-1*lpl)}
  ###################################
  ## construct new boosting family ##
  ###################################
  Family(
    ngradient = multistate.ngradient,
    risk = function(y, f, w) sum(multistate.nlpl(y, f, w)),
    offset = function(y, w) 0,
    name = "Family for multi state models.")}
