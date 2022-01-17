//--------------------------------------------------------------
// Header (header)
//--------------------------------------------------------------
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;


//--------------------------------------------------------------------------
// Functions for estimating scatter matrices (Fun_estimate) 
//--------------------------------------------------------------------------
// Get Tyler's M estimator under general structure
// [[Rcpp::export()]]
Rcpp::List getGenTyler(
    arma::mat X, 
    double tol = 1e-5, 
    int niter = 1000){
  
  int n = X.n_rows;
  int p = X.n_cols;
  int iter = 1;
  bool stop_rule = false;
  mat S_old = eye(p, p), S(p, p);
  
  cube xxT(p, p, n);
  for(int ii = 0; ii < n; ii++){
    xxT.slice(ii) =  X.row(ii).t() * X.row(ii);
  }
  
  while(1){
    S = zeros(p, p);
    for(int ii = 0; ii < n; ii++){
      S += xxT.slice(ii) / accu(inv(S_old) % xxT.slice(ii));
    }
    S = S * p / n * p / accu(S.diag());
    
    stop_rule = accu(square(S - S_old)) < tol * accu(square(S_old));
    if(stop_rule){
      break;
    }
    if(iter > niter){
      Rcout << "Maximum iteration reached without convergence\n";
      break;
    }
    iter += 1;
    S_old = S;
  }
  return List::create(
    Named("S") = S,
    Named("iter") = iter
  );
}

// Get Kendall's tau estimator
// [[Rcpp::export()]]
Rcpp::List getKendall(arma::mat X){
  
  int n = X.n_rows;
  mat D = arma::sign(kron(X, ones(n, 1)) - kron(ones(n, 1), X));
  mat S = (D.t() * D) / (n * (n - 1.0));
  return List::create(
    Named("S") = S
  );
}
