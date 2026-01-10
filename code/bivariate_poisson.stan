data {
  int<lower=0> N;
  int<lower=0> Nt;
  int<lower=0> Np;
  array[N] int<lower=0> s1;
  array[N] int<lower=0> s2;
  array[N] int<lower=0> ht;
  array[N] int<lower=0> at;
  vector<lower=0>[N] hg;
  array[Np] int<lower=0> htp;
  array[Np] int<lower=0> atp;
}

parameters {
  real home;
  vector[Nt] att;
  vector[Nt] def;
  vector[Nt] rho;
  real kappa;
  real mu;
}

transformed parameters {
  vector[N] theta_1;
  vector[N] theta_2;
  vector[N] theta_3;
  for (i in 1:N){
  theta_1[i] = exp(mu + (home * hg[i]) + att[ht[i]] - def[at[i]]);
  theta_2[i] = exp(mu + att[at[i]] - def[ht[i]]);
  theta_3[i] = exp(rho[ht[i]] + rho[at[i]]);
  }
  
}

model {
  // priors.
  home ~ normal(0, 0.1);
  att ~ normal(0, 0.1);
  def ~ normal(0, 0.1);
  rho ~ normal(0, 0.01);
  kappa ~ normal(0, 2);

  s1 ~ poisson(theta_1 + theta_3);
  s2 ~ poisson(theta_2 + theta_3);
}



generated quantities {
  vector[Np] theta1p; 
  vector[Np] theta2p; 
  vector[Np] theta3p;
  array[Np] int s1p; 
  array[Np] int s2p; 
  vector[N] log_lik;
  
  for (i in 1:Np){
  theta1p[i] = exp(mu + (home * hg[i]) + att[ht[i]] - def[at[i]]);
  theta2p[i] = exp(mu + att[at[i]] - def[ht[i]]);
  theta3p[i] = exp(rho[ht[i]] + rho[at[i]]);
  }
  
  s1p = poisson_rng(theta1p + theta3p);
  s2p = poisson_rng(theta2p + theta3p);

  for (i in 1:N){
    log_lik[i] = poisson_lpmf(s1[i] | theta_1[i] + theta_3[i]) + poisson_lpmf(s2[i] | theta_2[i] + theta_3[i]);
   }

}
