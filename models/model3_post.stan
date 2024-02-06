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
  vector<lower=-6, upper=0>[I] log_a;
  vector<lower=-0.5, upper=0.5>[I] b;
  vector<lower=0, upper=0.5>[I] g;
  real<lower=-6, upper=0> mu_log_a;
  real<lower=0, upper=0.5> mu_g;
  real<lower=-0.5, upper=0.5> mu_b; 
  real<lower=0> sigma_mu_log_a;
  real<lower=0> sigma_mu_g;
  real<lower=0> sigma_mu_b;
}

transformed parameters {
  
  vector<lower=0, upper=1>[I] a;
    
  for (i in 1:I) a[i] = exp(log_a[i]);
  
}

model {
  vector[N] mu;
  vector[N] p;

  // individual level parameters
  for (i in 1:I) log_a[i] ~ normal(mu_log_a, sigma_mu_log_a);
  for (i in 1:I) g[i]     ~ normal(mu_g, sigma_mu_g);
  for (i in 1:I) b[i]     ~ normal(mu_b, sigma_mu_b);
  
  // hyperparameters: means and standard deviations of mutlivariate distributions
  mu_log_a ~ normal(-3 , 1*scaling);
  mu_b     ~ normal(mu_b_mean, 0.1*scaling); 
  mu_g     ~ normal(0.17, 0.05*scaling); 
  
  sigma_mu_log_a ~ inv_gamma(2*scaling, 1);
  sigma_mu_g ~ inv_gamma(12*scaling, 1);
  sigma_mu_b ~ inv_gamma(6*scaling, 1);
  
  for (n in 1:N) mu[n] = (cperc[n] - b[sub[n]]) / a[sub[n]];
  for (n in 1:N) p[n]  = g[sub[n]] + (1 - 2 * g[sub[n]]) * (1 - exp(-fmax(mu[n], .001)));
  y ~ bernoulli(p);
  
}

