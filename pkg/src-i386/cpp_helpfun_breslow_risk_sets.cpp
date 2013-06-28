#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_breslow_risk_sets")]]
List cpp_helpfun_breslow_risk_sets(List risksetsR, NumericVector fromR, NumericVector fromqR){
  List risksets(risksetsR);
  NumericVector from(fromR);
  int fromq = as<int>(fromqR);
  //
  int n = from.size();
  //
  NumericMatrix breslowriskmat(n,n);
  NumericVector currentriskset;
  //
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++){
      currentriskset = risksets[j];
      if(is_true(any(currentriskset == (i+1)))){
	if(from[i] == fromq){
	  if(from[j] == fromq){
	    breslowriskmat(i,j) = 1;
	  }
	}
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("breslowriskmat") = breslowriskmat);}
