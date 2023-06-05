data {
  int<lower=1> D;                      // # Dimensions of the model: Nr. of quantifiers
  int<lower=1> I;                      // # Participants
  int<lower=1> T;                      // # Timepoints
  int<lower=0> N_t1;                   // # Timepoint 1: Observations/Rows in dataset
  int<lower=0> N_t2;                   // # Timepoint 2
  int<lower=0,upper=1> y_t1[N_t1];     // # Data 0,1
  int<lower=0,upper=1> y_t2[N_t2];   
  vector[N_t1] cperc_t1;               // # Centered Percentages
  vector[N_t2] cperc_t2; 
  int<lower=1,upper=I> sub_t1[N_t1];   // # Participant vector
  int<lower=1,upper=I> sub_t2[N_t2];  
  real mu_b_mean;
}

parameters {
  vector<lower=-6, upper=0>[T] log_a[I];
  vector<lower=-6, upper=0>[T] mu_log_a;
  vector<lower=-0.5, upper=0.5>[T] b[I];
  vector<lower=-0.5, upper=0.5>[T] mu_b; 
  vector<lower=0, upper=0.5>[T] g[I];
  vector<lower=0, upper=0.5>[T] mu_g;
  real<lower=0, upper=1> rho_raw_a;
  real<lower=0, upper=1> rho_raw_b;
  real<lower=0, upper=1> rho_raw_g;
  real<lower=0> sigma_mu_log_a[T];
  real<lower=0> sigma_mu_g[T];
  real<lower=0> sigma_mu_b[T];
}

transformed parameters {
  vector<lower=0, upper=1>[T] a[I];
  real<lower=-1, upper=1> rho_a;
  real<lower=-1, upper=1> rho_b;
  real<lower=-1, upper=1> rho_g;
  cov_matrix[T] Sigma_a;
  cov_matrix[T] Sigma_g;
  cov_matrix[T] Sigma_b;
  cholesky_factor_cov[T] L_Sigma_a;
  cholesky_factor_cov[T] L_Sigma_b;
  cholesky_factor_cov[T] L_Sigma_g;
  
  for (t in 1:T)
    for (i in 1:I)
      a[i,t] = exp(log_a[i,t]);
  
  rho_a = (2 * rho_raw_a) - 1;
  rho_g = (2 * rho_raw_g) - 1;
  rho_b = (2 * rho_raw_b) - 1;
  
  Sigma_a[1, 1] = sigma_mu_log_a[1]^2;
  Sigma_a[2, 2] = sigma_mu_log_a[2]^2;
  Sigma_a[1, 2] = rho_a * sigma_mu_log_a[1] * sigma_mu_log_a[2];
  Sigma_a[2, 1] =  Sigma_a[1, 2];
  L_Sigma_a = cholesky_decompose(Sigma_a);
  
  Sigma_g[1, 1] = sigma_mu_g[1]^2;
  Sigma_g[2, 2] = sigma_mu_g[2]^2;
  Sigma_g[1, 2] = rho_g * sigma_mu_g[1] * sigma_mu_g[2];
  Sigma_g[2, 1] = Sigma_g[1, 2];
  L_Sigma_g = cholesky_decompose(Sigma_g);
  
  Sigma_b[1, 1] = sigma_mu_b[1]^2;
  Sigma_b[2, 2] = sigma_mu_b[2]^2;
  Sigma_b[1, 2] = rho_b * sigma_mu_b[1] * sigma_mu_b[2];
  Sigma_b[2, 1] = Sigma_b[1, 2];
  L_Sigma_b = cholesky_decompose(Sigma_b);
  
}

model {
  vector[N_t1] mu_t1;
  vector[N_t2] mu_t2;
  vector[N_t1] p_t1;
  vector[N_t2] p_t2;

  // individual level parameters
  for (i in 1:I) log_a[i] ~ multi_normal_cholesky(mu_log_a, L_Sigma_a);
  for (i in 1:I) g[i]     ~ multi_normal_cholesky(mu_g, L_Sigma_g);
  for (i in 1:I) b[i]     ~ multi_normal_cholesky(mu_b, L_Sigma_b);
  
  // hyperparameters: means of mutlivariate distributions
  for (t in 1:T) mu_log_a[t] ~ normal(-4 , 1);
  for (t in 1:T) mu_g[t]     ~ normal(0, 0.2); 
  for (t in 1:T) mu_b[t]     ~ normal(mu_b_mean, 0.2); 
  
  // hyperparameters: correlations between timepoints
  rho_raw_a ~ beta(4,4);
  rho_raw_g ~ beta(5,3);
  rho_raw_b ~ beta(5,3);
  
  for (t in 1:T) sigma_mu_log_a[t] ~ inv_gamma(2, 1);
  for (t in 1:T) sigma_mu_g[t] ~ inv_gamma(5, 1);
  for (t in 1:T) sigma_mu_b[t] ~ inv_gamma(5, 1);
  
  // timepoint 1
  for (n in 1:N_t1) mu_t1[n] = (cperc_t1[n] - b[sub_t1[n], 1]) / a[sub_t1[n], 1];
  for (n in 1:N_t1) p_t1[n]  = g[sub_t1[n], 1] + (1 - 2 * g[sub_t1[n], 1]) * (1 - exp(-fmax(mu_t1[n], .001)));
  y_t1 ~ bernoulli(p_t1);

  // timepoint 2
  for (n in 1:N_t2) mu_t2[n] = (cperc_t2[n] - b[sub_t2[n], 2]) / a[sub_t2[n], 2];
  for (n in 1:N_t2) p_t2[n]  = g[sub_t2[n], 2] + (1 - 2 * g[sub_t2[n], 2]) * (1 - exp(-fmax(mu_t2[n], .001)));
  y_t2 ~ bernoulli(p_t2);
  
}

