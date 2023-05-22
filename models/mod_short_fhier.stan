data {
  int<lower=1> D;                     // # Dimensions of the model: nr. of quantifiers
  int<lower=0> N;                     // # Observations: rows in dataset
  int<lower=1> I;                     // # Participants
  int<lower=0,upper=1> y[N];          // # Data 0,1
  vector[N] cperc;                    // # Centered Percentages
  int<lower=1,upper=I> sub[N];        // # Participant vector
  int<lower=0,upper=1> fewer[N];      // # Fewer than half
  int<lower=0,upper=1> more[N];       // # More than half
}

parameters {
  vector<lower=0>[D] alpha[I];         // # Vectors of alphas
  vector[D] beta[I];                   // # Vectors of betas
  vector<lower=0,upper=1>[D] gamma[I]; // # Vector of gammas
  real delta[D];                       // # Means of betas
  real<lower=0> sigma[D];              // # Standard deviation of betas
  real nu[D];                          // # Means of alphas
  real<lower=0> sigmaalpha[D];         // # Standard deviation of alphas
  real<lower=0, upper=1> mug[D];       // # Mode of gammas
  real<lower=0> eta[D];                // # Concentration of gammas (prior sample size a + b)
}

transformed parameters {
  real<lower=0> a[D];
  real<lower=0> b[D];
  
  for (d in 1:D)
    a[d] = eta[d] * mug[d];
  for (d in 1:D)
    b[d] = eta[d] * (1 - mug[d]);
}

model {
  vector[N] mu;
  vector[N] p;
  delta       ~ normal(0, 5);
  sigma       ~ student_t(4, 0, 0.3);
  nu          ~ normal(-4, 1);
  sigmaalpha  ~ student_t(4, 0, 1);
  mug         ~ beta(2, 20);
  eta         ~ exponential(0.045);
  
  for (i in 1:I)
    beta[i] ~ normal(delta, sigma);
  for (i in 1:I)
    alpha[i] ~ lognormal(nu, sigmaalpha);
  for (i in 1:I)
    gamma[i] ~ beta(a, b);
  for (n in 1:N)
    mu[n] = fewer[n] * (cperc[n] - beta[sub[n], 1]) / alpha[sub[n], 1] + 
            more[n] * (cperc[n] - beta[sub[n], 2]) / alpha[sub[n], 2];
  for (n in 1:N)
    p[n] = fewer[n] * gamma[sub[n], 1] + 
    more[n] * gamma[sub[n], 2] + 
    (1 - 2 * (fewer[n] * gamma[sub[n], 1] + 
    more[n] * gamma[sub[n], 2])) *
  inv_logit(mu[n]);
  y ~ bernoulli(p);
}

generated quantities {
  int y_postpred[N];
  vector[N] mu_postpred;
  vector[N] p_postpred;
  
  for (n in 1:N)
    mu_postpred[n] = fewer[n] * (cperc[n] - beta[sub[n], 1]) / alpha[sub[n], 1] + 
            more[n] * (cperc[n] - beta[sub[n], 2]) / alpha[sub[n], 2];
  for (n in 1:N)
    p_postpred[n] = fewer[n] * gamma[sub[n], 1] + 
    more[n] * gamma[sub[n], 2] + 
    (1 - 2 * (fewer[n] * gamma[sub[n], 1] + 
    more[n] * gamma[sub[n], 2])) *
  inv_logit(mu_postpred[n]);
  y_postpred =  bernoulli_rng(p_postpred);
}
