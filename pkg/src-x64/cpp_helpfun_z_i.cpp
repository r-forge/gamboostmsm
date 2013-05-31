#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_z_i")]]
NumericVector cpp_helpfun_z_i(NumericVector etahat, NumericMatrix numerator, List denominator){
  NumericVector eta(etahat);
  NumericVector expeta = exp(eta);
  NumericMatrix indikatormatrix(numerator);
  List denominatorlistouter(denominator);
  int n = eta.size();
  NumericVector zi(n);
  NumericMatrix denominatoreta(n,n);
  NumericMatrix numeratoreta(n,n);
  std::fill(numeratoreta.begin(), numeratoreta.end(), 0.0);
  std::fill(denominatoreta.begin(), denominatoreta.end(), 0.0001);
  for(int i=0; i<n; i++){
    List denominatorlistinner = wrap(denominatorlistouter[i]);
    for(int j=0; j<n; j++){
      numeratoreta(i,j) = expeta(i)*indikatormatrix(j,i);
      IntegerVector denominatorindex = denominatorlistinner[j];
      if((denominatorindex(0)-1)<n){
	denominatoreta(i,j) = 0.0;
	for(int k=0; k<denominatorindex.size(); k++){
	  denominatoreta(i,j) = denominatoreta(i,j) + expeta(denominatorindex(k));
        }
      }
      if((denominatorindex(0)-1)>n){
	numeratoreta(i,j) = 0;
      }
    }
    double ratio = sum(numeratoreta(i,_)/(denominatoreta(i,_)));
    zi(i) = ratio;
  }
  return(wrap(zi));
}
