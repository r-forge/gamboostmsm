#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_risk_sets_wrt_times_and_states")]]
List cpp_helpfun_risk_sets_wrt_times_and_states(List risksetsR, NumericVector fromR, NumericVector toR, NumericVector fromqR, NumericVector toqR){
  List risksets(risksetsR);
  NumericVector from(fromR);
  NumericVector to(toR);
  int fromq = as<int>(fromqR);
  int toq = as<int>(toqR);
  //
  int n = from.size();
  int countindex = 0;
  //
  NumericMatrix numerator(n,n);
  NumericVector currentriskset;
  //
  NumericVector helpvec(n, 0.0);
  NumericVector nullvec;
  NumericVector denominatorhelpvec = nullvec;
  //
  List denominatorlistouter = List(helpvec);
  List denominatorlistinner = List(helpvec);
  //
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++){
      currentriskset = risksets[j];
      if(is_true(any(currentriskset == (i+1)))){
	if(from[i] == fromq){
	  if(from[j] == fromq){
	    if(to[j] == toq){
	      numerator(i,j) = 1;
	      for(int k=0; k<n; k++){
		if(is_true(any(currentriskset == (k+1)))){
		  if(from[k] == fromq){
		    denominatorhelpvec.push_back(1);
		    denominatorhelpvec[countindex] = k;
		    countindex = countindex+1;
		  }
		}
	      }
	    }
	  }
	}
      }
      if(denominatorhelpvec.size() < 1){
	denominatorhelpvec.push_back(1);
	denominatorhelpvec[0] = 2*n;
	denominatorhelpvec.push_back(1);
	denominatorhelpvec[1] = 2*n;
      }
      denominatorlistinner[j] = denominatorhelpvec;
      countindex = 0;
      denominatorhelpvec = nullvec;
    }
    denominatorlistouter[i] = denominatorlistinner;
    denominatorlistinner = List(helpvec);
  }
  return Rcpp::List::create(Rcpp::Named("numerator") = numerator, 
			    Rcpp::Named("denominator") = denominatorlistouter);}
