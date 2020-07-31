//
// This Stan program defines a Bayesian model for the damage/healing
// model with an proficiency interaction term using character-level data from Critical Role
//

//
// Author: Trent Henderson, 29 July 2020
//

// Data specification
data {
  int<lower=1> N;      // number of observations/rows
  vector[N] damage;    // log-transformed and centred damage vector
  vector[N] healing;   // log-transformed healing vector
  vector[N] proficiency; // proficiency vector
  //int<lower=0,upper=1>proficiency;   // binary proficiency bonus indicator
}

// Parameters
parameters {
  vector[4] beta;       // coefficients for damage
  real<lower=0> sigma;  // error SD for Gaussian likelihood
}

// Model specification
model {
  // Log-likelihood
  target += normal_lpdf(healing | beta[1] + beta[2] * damage + beta[3] * proficiency + beta[4] * damage * proficiency, sigma);

  // Log-priors
  target += normal_lpdf(sigma | 0, 1)
          + normal_lpdf(beta | 0, 1);
}

// Quantities
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] healing_rep; // replications from posterior predictive dist

  for (n in 1:N) {
    real healing_hat_n = beta[1] + beta[2] * damage[n] + beta[3] * proficiency[n] + beta[4] * damage[n] * proficiency[n];
    log_lik[n] = normal_lpdf(healing[n] | healing_hat_n, sigma);
    healing_rep[n] = normal_rng(healing_hat_n, sigma);
  }
}