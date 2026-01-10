data {
    int<lower=0> N;
    int<lower=0> Nt;
    int<lower=0> Np;
    array[N] int<lower=0> s1;
    array[N] int<lower=0> s2;
    array[N] int<lower=0> ht;
    array[N] int<lower=0> at;

    //predictions
    array[Np] int<lower=0> htp; // home team index for prediction
    array[Np] int<lower=0> atp; //away team index for prediction
}
parameters{
    real home;
    vector[Nt] att;
    vector[Nt] def;

    // for correlated features.
    corr_matrix[2] Rho;
    vector<lower=0>[2] sigma_team;
    real att_bar;
    real def_bar;
}
transformed parameters {
   vector[N] theta_1;
   vector[N] theta_2;

   theta_1 = exp(home + att[ht] - def[at]);
   theta_2 = exp(att[at] - def[ht]);
}
model{
    home ~ normal(0, 0.1);
    att_bar ~ normal(0, 0.1);
    def_bar ~ normal(0, 0.1);
    sigma_team ~ exponential(1);
    Rho ~ lkj_corr(2);

    {
    vector[2] MU;
    array[20] vector[2] YY;
    MU = [att_bar, def_bar]';
    for (j in 1:Nt) YY[j] = [att[j], def[j]]';
    YY ~ multi_normal(MU, quad_form_diag(Rho, sigma_team));
    }

    s1 ~ poisson(theta_1);
    s2 ~ poisson(theta_2);
}

generated quantities {
  vector[Np] theta1p; // home team predicted score
  vector[Np] theta2p; // away team predicted score.
  array[Np] int s1p; // score prediction for home using theta1p.
  array[Np] int s2p; // score prediction for away using theta2p.
  vector[N] log_lik;
  
  theta1p = exp(home + att[htp] - def[atp]);
  theta2p = exp(att[atp] - def[htp]);
  
  s1p = poisson_rng(theta1p);
  s2p = poisson_rng(theta2p);

   
   for (i in 1:N){
    log_lik[i] = poisson_lpmf(s1[i] | theta_1[i]) + poisson_lpmf(s2[i] | theta_2[i]);
   }
   
}

