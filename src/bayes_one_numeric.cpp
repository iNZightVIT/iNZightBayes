#include <Rcpp.h>
using namespace Rcpp;

//' Gibbs Sample for Single Mean and Variance
//'
//' Gibbs sampler to sample mean and variance of one numeric variable
//'
//' @details
//' Assumes conjugate Normal-Inverse-Gamma priors on mean and variance:
//' \deqn{y \sim \mathrm{Normal}(\mu, \sigma^2)}
//'
//' @param y a vector of values
//' @param steps number of iterations to run Gibbs sampler for
//' @param burnin number of burn-in iterations to discard before proper steps
//' @param thin thinning factor (default 1)
//' @param mu.0 prior mean for mu (default 0)
//' @param sigma2.0 prior variance for mu (default 1e6)
//' @param alpha prior shape parameter for sigma2 (default 1e-3)
//' @param beta prior scale parameter for sigma2 (default 1e-3)
// [[Rcpp::export]]
List gibbs_one_numeric_cpp (
    NumericVector y,
    int steps,
    int burnin,
    int thin = 1,
    double mu_0 = 0.0,
     double sigma2_0 = 1e6,
    double alpha = 1e-3,
    double beta = 1e-3
)
{
    // Create vectors to store posterior samples
    int Nkeep = floor (steps / thin);
    NumericVector mu (Nkeep);
    NumericVector sigma2 (Nkeep);

    // Run MCMC
    double sigma2_n, mu_n, alpha_n, beta_n;

    int n = y.size ();
    double sum_y = std::accumulate(y.begin(), y.end(), 0.0);

    alpha_n = alpha + n / 2.0;

    // Set starting values
    double mu_i = mean (y),
           sigma2_i = var (y);

    int k=0;
    int N (burnin + Nkeep * thin);
    double beta_s, ys;
    for (int i=0; i<N; ++i)
    {
        /*
         * Step 1: Sample from full conditional of mu | sigma2
         *******************************************************/

        // Compute posterior parameters for mu | sigma2
        sigma2_n = 1.0 / (1.0 / sigma2_0 + n / sigma2_i);
        mu_n = sigma2_n * ( mu_0 / sigma2_0 + sum_y / sigma2_i);

        // Sample mu[i + 1] | sigma2[i] using Normal conjugate
        mu_i = R::rnorm ( mu_n, sqrt (sigma2_n) );

        /*
         * Step 2: Sample from full conditional of sigma2 | mu
         *******************************************************/

        // Compute posterior parameters for sigma2 | mu
        // std::pow is *very* slow ...
        beta_s = 0.0;
        for (auto yi : y)
        {
            ys = yi - mu_i;
            beta_s += ys * ys;
        }
        beta_n = beta + 0.5 * beta_s;

        // Sample sigma2[i + 1] given mu[i + 1] using Inverse-Gamma conjugate
        sigma2_i = 1.0 / R::rgamma ( alpha_n, 1.0 / beta_n );

        if (i >= burnin && (i - burnin) % thin == 0 )
        {
            mu.at (k) = mu_i;
            sigma2.at (k) = sigma2_i;
            k++;
            if (k > Nkeep) break;
        }
    }

    List z = List::create ( _["mu"] = mu, _["sigma2"] = sigma2 );
    return z;
}

/*
Benchmark:

y <- rnorm(1000, 10, sqrt(4))
res <- microbenchmark::microbenchmark(
    gibbs_one_numeric(y, 20000, 5000, thin = 20, verbose = FALSE),
    gibbs_one_numeric_cpp(y, 20000, 5000, thin = 20),
    times = 20
)

*/
