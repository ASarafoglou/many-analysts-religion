//H4: null model
data{
    int<lower=1> N;
    int y[N];
}
parameters{
    real ap;
    real al;
}
model{
    vector[N] lambda;
    vector[N] p;
    al ~ normal( 0 , 10 );
    ap ~ normal( 0 , 1 );
    for ( i in 1:N ) {
        lambda[i] = al;
        lambda[i] = exp(lambda[i]);
    }
    for ( i in 1:N ) {
        p[i] = ap;
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:N ) {
    if (y[i] == 0)
      target += (log_sum_exp(bernoulli_lpmf(1|p[i]),
        bernoulli_lpmf(0|p[i]) + poisson_lpmf(y[i]|lambda[i])));
    else
      target += (bernoulli_lpmf(0|p[i]) + poisson_lpmf(y[i]|lambda[i]));
    }//i 
}
generated quantities{
    vector[N] lambda;
    vector[N] p;
    for ( i in 1:N ) {
        lambda[i] = al;
        lambda[i] = exp(lambda[i]);
    }
    for ( i in 1:N ) {
        p[i] = ap;
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:N ) {
    } 
}
