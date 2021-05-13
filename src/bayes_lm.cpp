#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


//' Gibbs Sample for Linear Regression
//'
//' Gibbs sampler for linear regression
//'
//' @param y a vector of response values
//' @param x a matrix of explanatory variables
//' @param steps number of samples to obtain
//' @export
// [[Rcpp::export]]
List gibbs_lm (
    NumericVector y,
    NumericMatrix x,
    int steps = 1000
)
{
    // y and x correct dimensions
    int n = x.rows (),
        k = x.cols () + 1;
    if (y.size() != n) stop("X must have same number of rows as length of y");
    arma::colvec Y = y;

    // Center X matrix for stability
    arma::colvec xm = colMeans(x);
    arma::mat X (n, k);
    X.col (0) = arma::colvec (n, arma::fill::ones);
    for (int j=0; j<k-1; ++j) X.col (j + 1) = arma::colvec (x.column (j) - xm.at (j));

    // Computations outside the loop:
    int df = n - k;                     // deg. freedom
    arma::mat Sigma = arma::pinv (X.t () * X);   // Normal posterior cov matrix
    arma::vec mu = Sigma * X.t () * Y;
    arma::colvec r = Y - X * mu;
    double a = df * 0.5,
           b = 0.5 * arma::as_scalar (r.t () * r);

    // Posterior objects:
    arma::mat beta (steps, k);
    arma::vec sigma2 (steps);
    int Sk = Sigma.n_cols;
    arma::mat Yn;
    for (int i=0; i<steps; ++i) {
        sigma2.at (i) = R::rgamma ( a, 1 / b);

        // simulate from MVNormal using cholesky decomp
        beta.row (i) =
            arma::repmat (mu, 1, 1).t () +
                arma::randn (1, Sk) * arma::chol (Sigma * sigma2.at (i));
    }

    // transform intercept
    beta.col (0) = beta.col (0) - beta.cols (1, k-1) * xm;

    List z = List::create (
        _["posterior"] = DataFrame::create (
            _["beta"] = beta,
            _["sigma2"] = sigma2
        ),
        _["mcmc_info"] = List::create (
            _["thin"] = 1,
            _["burnin"] = 0,
            _["iter"] = steps,
            _["samples"] = steps
        )
    );
    return z;
}


/*
Benchmark:

y <- iris$Sepal.Length
x <- poly(iris$Sepal.Width, 3)
res <- microbenchmark::microbenchmark(
    gibbs_lm(y, x, 10000),
    gibbs_lm_R(y, x, 10000),
    times = 20
)

*/
