functions {
    real bipois_loglik(int aX, int aY, real log_l1, real log_l2, real log_l3) {
        int mn;
        real f;
        real init; //initial value
        real cons;

        init = poisson_log_lpmf(aX | log_l1) + poisson_log_lpmf(aY |log_l2) - exp(log_l3);
        mn = min(aX, aY);
        if (mn > 0) {
            cons = -log_l1 - log_l2 + log_l3;
            f = init;
            for (i in 1:mn){
                f = f + log(aX-i+1) + log(aY-i+1) + cons - log(i);
                init = log_sum_exp(init, f);
            }
        }
        return(init);
    }
}

data {
  int<lower=0> N;
  int<lower=0> Nt;
  int<lower=0> Np;
  array[N] int<lower=0> s1;
  array[N] int<lower=0> s2;
  array[N] int<lower=0> ht;
  array[N] int<lower=0> at;
  array[Np] int<lower=0> htp;
  array[Np] int<lower=0> atp;
}

parameters {
  real home;
  vector[Nt] att;
  vector[Nt] def;
  real<lower=0> l3;

    // for correlated features.
    corr_matrix[2] Rho;
    vector<lower=0>[2] sigma_team;
    real att_bar;
    real def_bar;
}

transformed parameters {
  vector[N] log_l1;
  vector[N] log_l2;
  real log_l3;

  // l1 = log(theta_1) = log(exp(...)) == ...
  log_l1 = home + att[ht] - def[at]; 
  log_l2 = att[at] - def[ht];
  log_l3 = log(l3);
  
}

model {
    home ~ normal(0, 0.1);
    att_bar ~ normal(0, 0.1);
    def_bar ~ normal(0, 0.1);
    sigma_team ~ exponential(1);
    Rho ~ lkj_corr(2);
    l3 ~ exponential(1);

    {
    vector[2] MU;
    array[20] vector[2] YY;
    MU = [att_bar, def_bar]';
    for (j in 1:Nt) YY[j] = [att[j], def[j]]';
    YY ~ multi_normal(MU, quad_form_diag(Rho, sigma_team));
    }

    for (i in 1:N){
    target += bipois_loglik(s1[i], s2[i], log_l1[i], log_l2[i], log_l3);
    }
}
