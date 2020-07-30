//
// This Stan program defines a Bayesian model for the damage/healing
// model with an episode interaction term using character-level data from Critical Role
//

//
// Author: Trent Henderson, 29 July 2020
//

// Data specification
data {
  int<lower=1> N;      // number of observations/rows
  vector[N] damage;    // log-transformed and centred damage vector
  vector[N] healing;   // log-transformed healing vector
  int<lower=0,upper=1>episode;   // binary episode indicator
}

// Parameters
parameters {
  vector[2] b0;       // coefficient for damage
  vector[3] b1;       // coefficient for episode indicator
  vector[4] b2;       // coefficient for damage*episode interaction indicator
  real<lower=0> sigma;  // error SD for Gaussian likelihood
}

// Model specification
model {
  // Log-likelihood
  target += normal_lpdf(healing | b0[1] + b0[2] * damage + b1[3] * episode + b2[4] * (damage * episode), sigma);

  // Log-priors
  target += normal_lpdf(sigma | 0, 1)
          + normal_lpdf(b0 | 0, 1);
          + bernoulli_lpmf(b1 | 0.5);
          + normal_lpdf(b2 | 0, 1);
}

// Quantities
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] healing_rep; // replications from posterior predictive dist

  for (n in 1:N) {
    real healing_hat_n = b0[1] + b0[2] * damage[n] + b1[3] * episode[n] + b2[4] * (damage[n] * episode[n]);
    log_lik[n] = normal_lpdf(healing[n] | healing_hat_n, sigma);
    healing_rep[n] = normal_rng(healing_hat_n, sigma);
  }
}
