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
    int n;
    array[n] int<lower=0> x;
    array[n] int<lower=0> y;
}
parameters {
    real<lower=0> l1;
    real<lower=0> l2;
    real<lower=0> l3;
}

transformed parameters {
   real log_l1; real log_l2; real log_l3;
   log_l1 = log(l1);
   log_l2 = log(l2);
   log_l3 = log(l3);
}
model{
    //priors.
    l1 ~ exponential(1);
    l2 ~ exponential(1);
    l3 ~ exponential(1);

    // model
    for (j in 1:n){
    target += bipois_loglik(x[j], y[j], log_l1, log_l2, log_l3);
    }
    

}
