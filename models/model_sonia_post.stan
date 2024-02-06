data {
  int<lower=1> I;                // # Participants
  int<lower=0> N;                // # Observations/Rows in dataset
  int<lower=0,upper=1> y[N];     // # Data 0,1
  vector[N] cperc;               // # Centered Percentages
  int<lower=1,upper=I> sub[N];   // # Participant vector
  real mu_b_mean;
  real scaling;
}

parameters {
  real mu_b;                     // Means of betas
  real<lower=0> sigma_mu_b;      // variance of betas
  real mu_a;                     // Means of alphas
  real<lower=0> sigma_mu_a;      // variance of alphas
  vector[I] b;                   // vectors of betas
  vector<lower=0>[I] a;          // vectors of alphas
  vector<lower=0,upper=1>[I] g;  //vector of gammas
}

transformed parameters {
  real<lower=0> sigma;
  real<lower=0> sigmaalpha;
  sigma      = sqrt(sigma_mu_b);
  sigmaalpha = sqrt(sigma_mu_a);
}

model {
  vector[N] mu;
  vector[N] p;
  
  mu_b ~ normal(0, 5);
  mu_a ~ normal(0, 5);
  
  sigma_mu_a ~ inv_gamma(2, .2);
  sigma_mu_b ~ inv_gamma(2, .2);
  
  for (i in 1:I)
    b[i] ~ normal(mu_b, sigma);
  for (i in 1:I)
    a[i] ~ lognormal(mu_a, sigmaalpha);
  for (i in 1:I)
    g[i] ~ beta(2, 20);
  for (n in 1:N)
    mu[n] = (cperc[n] - b[sub[n]]) / a[sub[n]]; 
  for (n in 1:N)
    p[n] = g[sub[n]] + (1 - 2 * g[sub[n]]) * inv_logit(mu[n]);
  y ~ bernoulli(p);
}

