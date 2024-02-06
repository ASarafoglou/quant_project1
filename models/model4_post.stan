data {
  int<lower=1> D;                // # Dimensions of the model: Nr. of quantifiers
  int<lower=1> I;                // # Participants
  int<lower=0> N;                // # Observations/Rows in dataset
  int<lower=0,upper=1> y[N];     // # Data 0,1
  vector[N] cperc;               // # Centered Percentages
  int<lower=1,upper=I> sub[N];   // # Participant vector
  real mu_b_mean;
  real scaling;
}

parameters {
  vector<lower=-0.5, upper=0.5>[I] b;
  vector<lower=0, upper=0.5>[I] g;
  real<lower=0, upper=0.5> mu_g;
  real<lower=-0.5, upper=0.5> mu_b; 
  real<lower=0> sigma_mu_g;
  real<lower=0> sigma_mu_b;
}

model {
  vector[N] mu;
  vector[N] p;

  // individual level parameters
  for (i in 1:I) g[i]     ~ normal(mu_g, sigma_mu_g);
  for (i in 1:I) b[i]     ~ normal(mu_b, sigma_mu_b);
  
  // hyperparameters: means and standard deviations of mutlivariate distributions
  mu_b     ~ normal(mu_b_mean, 0.1*scaling); 
  mu_g     ~ normal(0.17, 0.05*scaling); 
  
  sigma_mu_g ~ inv_gamma(12*scaling, 1);
  sigma_mu_b ~ inv_gamma(6*scaling, 1);
  
  for (n in 1:N) mu[n] = (cperc[n] - b[sub[n]])/.0025;
  for (n in 1:N) p[n]  = g[sub[n]] + (1 - 2 * g[sub[n]]) * (1 - exp(-fmax(mu[n], .001)));
  y ~ bernoulli(p);
  
}

// generated quantities{
//   vector[N] mu_pred;
//   vector[N] p_pred;
//   int y_pred[N];
// 
//   for (n in 1:N) mu_pred[n] = (cperc[n] - b[sub[n]])/.0025;
//   for (n in 1:N) p_pred[n]  = g[sub[n]] + (1 - 2 * g[sub[n]]) * (1 - exp(-fmax(mu_pred[n], .001)));
//   y_pred = bernoulli_rng(p_pred);
// }

