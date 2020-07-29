//
// This Stan program defines a Bayesian model for the damage/healing
// model using character-level data from Critical Role
//

//
// Author: Trent Henderson, 29 July 2020
//

// Data specification
data {
  int<lower=1> N;       // number of observations/rows
  vector[N] damage;    // log-transformed and centred damage vector
  vector[N] healing;     // log-transformed healing vector
}

// Parameters
parameters {
  vector[2] beta;           // coefficients
  real<lower=0> sigma;  // error SD for Gaussian likelihood
}

// Model specification
model {
  // Log-likelihood
  target += normal_lpdf(healing | beta[1] + beta[2] * damage, sigma);

  // Log-priors
  target += normal_lpdf(sigma | 0, 1)
          + normal_lpdf(beta | 0, 1);
}

// Quantities
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] healing_rep; // replications from posterior predictive dist

  for (n in 1:N) {
    real healing_hat_n = beta[1] + beta[2] * damage[n];
    log_lik[n] = normal_lpdf(healing[n] | healing_hat_n, sigma);
    healing_rep[n] = normal_rng(healing_hat_n, sigma);
  }
}
