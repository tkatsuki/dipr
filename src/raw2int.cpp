#include <Rcpp.h>
using namespace Rcpp;
//' 16 bit raw to integer conversion
//'
//' @param vec A raw vector.
//' @export
// [[Rcpp::export]]

IntegerVector raw2intC(CharacterVector vec){
  int n = vec.size();
  int m;
  Rcpp::IntegerVector x(n/2);

  if (n % 2 != 0)
    throw std::invalid_argument("Length of the vector must be an even number.");

  for (int i = 0; i < n/2; i++) {
    std::string h1 = Rcpp::as<std::string>(vec[i*2]);
    std::string h2 = Rcpp::as<std::string>(vec[i*2 + 1]);
    h2 += h1;
    std::stringstream ss;
    ss << std::hex << h2;
    ss >> m;
    x[i] = m;
  }
  return(x);
}

