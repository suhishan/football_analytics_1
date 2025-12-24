data {
  int<lower=0> nt; // number of teams (20)
  int<lower=0> ng; // number of games (370)
  int<lower=0> np; // number of games to predict (final 10 games)
  array[ng] int<lower=1> ht; //home team index
  array[ng] int<lower=1> at; // away team index
  array[ng] int<lower=0> s1; // home team score
  array[ng] int<lower=0> s2; // away team score
  
  array[np] int<lower=0> htp; // home team index for prediction
  array[np] int<lower=0> atp; //away team index for prediction
}

parameters{
  real home;
  vector[nt] att;
  vector[nt] def;
}

transformed parameters{
  vector[ng] theta1; // poisson lambda for home team
  vector[ng] theta2; //  poisson lambda for away team
  
  theta1 = exp(home + att[ht] - def[at]);
  theta2 = exp(att[at] - def[ht]);
}

model{
  //priors
  att ~ normal(0, 1);
  def ~ normal(0, 1);
  home ~ normal(0, 1);
  
  // likelihood
  s1 ~ poisson(theta1);
  s2 ~ poisson(theta2);
}

generated quantities {
  vector[np] theta1p; // home team predicted score
  vector[np] theta2p; // away team predicted score.
  
  vector[ng] log_lik; // a vector of log-likelihood per observation
  
  array[np] int s1p; // score prediction for home using theta1p.
  array[np] int s2p; // score prediction for away using theta2p.
  
  theta1p = exp(home + att[htp] - def[atp]);
  theta2p = exp(att[atp] - def[htp]);
  
  s1p = poisson_rng(theta1p);
  s2p = poisson_rng(theta2p);
  
  
  // TODO generate a log-likelihood for WAIC loo-PSIS.
  
  for (i in 1:ng) {
    log_lik[i] = poisson_lpmf(s1[i] | theta1[i]) + poisson_lpmf(s2[i] | theta2[i]);
  }
  
}


