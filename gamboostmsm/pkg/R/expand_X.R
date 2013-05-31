expand.X <- function(dat, id.index, trans.indicators.index, covariate.index){
  id <- dat[, id.index]
  trans.indicators <- dat[, trans.indicators.index]
  covariates <- as.data.frame(dat[, covariate.index])
  if(ncol(covariates) < 1.5){
    all.combinations <- expand.grid(names(dat)[covariate.index], names(trans.indicators))}
  if(ncol(covariates) > 1.5){
    all.combinations <- expand.grid(names(covariates), names(trans.indicators))}
  first <- as.character(all.combinations[, 1])
  last <- as.character(all.combinations[, 2])
  covariate.names <- paste(first, last, sep=":")
  expanded.dat <- dat[, -covariate.index]
  for(i in 1:nrow(all.combinations)){
    index.first <- which(names(dat)==first[i])
    index.last <- which(names(dat)==last[i])
    expanded.dat <- cbind(expanded.dat, dat[, index.first]*dat[, index.last])}
  names(expanded.dat)[(ncol(expanded.dat)-length(covariate.names)+1):ncol(expanded.dat)] <- covariate.names
  return(expanded.dat)}