//
// This Stan program defines a state space model for healing and damage data
//

//
// Author: Trent Henderson, 12 August 2020
//

data {
  int<lower=0> TT;
  int<lower=0> n_pos; // number of non-NA values
  int<lower=0> indx_pos[n_pos]; // index of the non-NA values
  vector[n_pos] y;
}
parameters {
  real x0;
  real u;
  vector[TT] pro_dev;
  real<lower=0> sd_q;
  real<lower=0> sd_r;
}
transformed parameters {
  vector[TT] x;
  x[1] = x0 + u + pro_dev[1];
  for(i in 2:TT) {
    x[i] = x[i-1] + u + pro_dev[i];
  }
}
model {
  x0 ~ normal(y[1],10);
  u ~ normal(0,2);
  sd_q ~ cauchy(0,5);
  sd_r ~ cauchy(0,5);
  pro_dev ~ normal(0, sd_q);
  for(i in 1:n_pos){
    y[i] ~ normal(x[indx_pos[i]], sd_r);
  }
}
generated quantities {
  vector[n_pos] log_lik;
  for (i in 1:n_pos) log_lik[i] = normal_lpdf(y[i] | x[indx_pos[i]], sd_r);
}
