#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export(".cpp_helpfun_risk_sets_wrt_times")]]
List cpp_helpfun_risk_sets_wrt_times(NumericVector timeslR, NumericVector timesrR){
  NumericVector timesl(timeslR);
  NumericVector timesr(timesrR);
  //
  int n = timesl.size();
  int countindex = 0;
  NumericVector helpvec(n, 0.0);
  NumericVector nullvec;
  NumericVector atriskatti = nullvec;
  List temporallyatrisk = List(helpvec);
  //
  for(int i=0; i<n; i++){
    for(int j=0; j<n; j++){
      if(timesl[j] <= timesr[i]){
	if(timesr[i] <= timesr[j]){
	  atriskatti.push_back(1);
	  atriskatti[countindex] = j+1; // +1 because of R index syntax.
	  countindex = countindex+1;
	}
      }
    }
    temporallyatrisk[i] = atriskatti;
    countindex = 0;
    atriskatti = nullvec;
  }
  return(wrap(temporallyatrisk));
}
