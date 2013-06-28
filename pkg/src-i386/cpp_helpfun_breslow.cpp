#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_breslow")]]
NumericVector cpp_helpfun_breslow(NumericVector etahat, NumericMatrix numerator){
  NumericVector eta(etahat);
  NumericVector expeta = exp(eta);
  NumericMatrix indikatormatrix(numerator);
  int n = eta.size();
  NumericVector denominatorvector(n);
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++){
      denominatorvector[i] = denominatorvector[i]+(expeta[j]*indikatormatrix(i,j));
    }
  }
  return(wrap(denominatorvector));
}
