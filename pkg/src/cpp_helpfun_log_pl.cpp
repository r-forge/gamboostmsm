#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_log_pl")]]
NumericVector cpp_helpfun_log_pl(NumericVector etahat, NumericMatrix numerator) {
  NumericVector eta(etahat);
  NumericVector expeta = exp(eta);
  NumericMatrix indicatormatrix(numerator);
  int n = eta.size();
  NumericVector logpli(n);
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++){
      logpli[i] = logpli[i]+(expeta[j]*indicatormatrix(i,j));
    }
  }
  logpli = log(0.000001+logpli);
  logpli = eta-logpli;
  return(wrap(logpli));
}
