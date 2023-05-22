data {
  int<lower=1> D;                      // # Dimensions of the model: Nr. of quantifiers
  int<lower=1> I;                      // # Participants
  int<lower=1> T;                      // # Timepoints
  int<lower=0> N_t1;                   // # Timepoint 1: Observations/Rows in dataset
  int<lower=0> N_t2;                   // # Timepoint 2
  vector[N_t1] cperc_t1;               // # Centered Percentages
  vector[N_t2] cperc_t2; 
  int<lower=1,upper=I> sub_t1[N_t1];   // # Participant vector
  int<lower=1,upper=I> sub_t2[N_t2];  
  real b_min;
  real b_max;
  real mu_b_sd;
  real invgamma_mub_b;
}

parameters {
  vector[T] log_a[I];
  vector[T] mu_log_a;
  vector<lower=b_min, upper=b_max>[T] b[I];
  vector<lower=b_min, upper=b_max>[T] mu_b; 
  vector<lower=0, upper=0.5>[T] g[I];
  vector<lower=0, upper=0.5>[T] mu_g;
  // real<lower=0, upper=1> rho_raw_a;
  // real<lower=0, upper=1> rho_raw_b;
  // real<lower=0, upper=1> rho_raw_g;
  // real<lower=0> sigma_mu_a[T];
  // real<lower=0> sigma_mu_g[T];
  // real<lower=0> sigma_mu_b[T];
  vector<lower=0>[T] sigma_mu_a;
  vector<lower=0>[T] sigma_mu_g;
  vector<lower=0>[T] sigma_mu_b;
  cholesky_factor_corr[T] L_a;
  cholesky_factor_corr[T] L_b;
  cholesky_factor_corr[T] L_g;
}

transformed parameters {
  vector<lower=0>[T] a[I];
  // real<lower=-1, upper=1> rho_a;
  // real<lower=-1, upper=1> rho_b;
  // real<lower=-1, upper=1> rho_g;
  cov_matrix[T] Sigma_a;
  cov_matrix[T] Sigma_g;
  cov_matrix[T] Sigma_b;
  corr_matrix[T] R_a;
  corr_matrix[T] R_g;
  corr_matrix[T] R_b;
  
  for (t in 1:T)
    for (i in 1:I)
      a[i,t] = exp(log_a[i,t]);
  
  R_a = multiply_lower_tri_self_transpose(L_a);
  R_g = multiply_lower_tri_self_transpose(L_b);
  R_b = multiply_lower_tri_self_transpose(L_g);
  
  Sigma_a = quad_form_diag(R_a, sigma_mu_a);
  Sigma_b = quad_form_diag(R_g, sigma_mu_b);
  Sigma_g = quad_form_diag(R_b, sigma_mu_g);
  
  // rho_a = (2 * rho_raw_a) - 1;
  // rho_g = (2 * rho_raw_g) - 1;
  // rho_b = (2 * rho_raw_b) - 1;
  // 
  // Sigma_a[1, 1] = sigma_mu_a[1]^2;
  // Sigma_a[2, 2] = sigma_mu_a[2]^2;
  // Sigma_a[1, 2] = rho_a * sigma_mu_a[1] * sigma_mu_a[2];
  // Sigma_a[2, 1] = rho_a * sigma_mu_a[1] * sigma_mu_a[2];
  // L_Sigma_a = cholesky_decompose(Sigma_a);

  // Sigma_g[1, 1] = sigma_mu_g[1]^2;
  // Sigma_g[2, 2] = sigma_mu_g[2]^2;
  // Sigma_g[1, 2] = rho_g * sigma_mu_g[1] * sigma_mu_g[2];
  // Sigma_g[2, 1] = rho_g * sigma_mu_g[1] * sigma_mu_g[2];
  // L_Sigma_g = cholesky_decompose(Sigma_g);
  // 
  // Sigma_b[1, 1] = sigma_mu_b[1]^2;
  // Sigma_b[2, 2] = sigma_mu_b[2]^2;
  // Sigma_b[1, 2] = rho_b * sigma_mu_b[1] * sigma_mu_b[2];
  // Sigma_b[2, 1] = rho_b * sigma_mu_b[1] * sigma_mu_b[2];
  // L_Sigma_b = cholesky_decompose(Sigma_b);
  
}

model {

  // individual level parameters
  // for (i in 1:I) log_a[i] ~ multi_normal_cholesky(mu_log_a, L_Sigma_a);
  // for (i in 1:I) g[i]     ~ multi_normal_cholesky(mu_g, L_Sigma_g);
  // for (i in 1:I) b[i]     ~ multi_normal_cholesky(mu_b, L_Sigma_b);
  for (i in 1:I) log_a[i] ~ multi_normal(mu_log_a, Sigma_a);
  for (i in 1:I) g[i]     ~ multi_normal(mu_g    , Sigma_g);
  for (i in 1:I) b[i]     ~ multi_normal(mu_b    , Sigma_b);
  // Omega = L_Omega * L_Omega'
  
  // hyperparameters: means of mutlivariate distributions
  for (t in 1:T) mu_log_a[t] ~ normal(-4 , 1);
  for (t in 1:T) mu_g[t]     ~ normal(0, 0.1); 
  for (t in 1:T) mu_b[t]     ~ normal(0, mu_b_sd); 
  
  // hyperparameters: correlations between timepoints
  L_a ~ lkj_corr_cholesky(2.0);
  L_b ~ lkj_corr_cholesky(2.0);
  L_g ~ lkj_corr_cholesky(2.0);
  // rho_raw_a ~ beta(8,8);
  // rho_raw_g ~ beta(10,6);
  // rho_raw_b ~ beta(10,6);
  
  // for (t in 1:T) sigma_mu_a[t] ~ inv_gamma(5, 0.5);
  // for (t in 1:T) sigma_mu_g[t] ~ inv_gamma(5, 0.5);
  // for (t in 1:T) sigma_mu_b[t] ~ inv_gamma(5, invgamma_mub_b);
  sigma_mu_a ~ cauchy(0, 5); // prior for sigma
  sigma_mu_b ~ cauchy(0, 5); // prior for sigma
  sigma_mu_g ~ cauchy(0, 5); // prior for sigma
  // sigma_mu_a ~ inv_gamma(5, 0.5);
  // sigma_mu_g ~ inv_gamma(5, 0.5);
  // sigma_mu_b ~ inv_gamma(5, invgamma_mub_b);
  
}

generated quantities {
  
  vector[N_t1] mu_t1_pred;
  vector[N_t2] mu_t2_pred;
  vector[N_t1] p_t1_pred;
  vector[N_t2] p_t2_pred;
  int y_pred_t1[N_t1];
  int y_pred_t2[N_t1];
  
  // Prior Predictive: Timepoint 1
  for (n in 1:N_t1) mu_t1_pred[n] = (cperc_t1[n] - b[sub_t1[n], 1]) / a[sub_t1[n], 1];
  for (n in 1:N_t1) p_t1_pred[n]  = g[sub_t1[n], 1] + (1 - 2 * g[sub_t1[n], 1]) * (1 - exp(-fmax(mu_t1_pred[n], .001)));
  y_pred_t1 = bernoulli_rng(p_t1_pred);

  // Prior Predictive: Timepoint 2
  for (n in 1:N_t2) mu_t2_pred[n] = (cperc_t2[n] - b[sub_t2[n], 2]) / a[sub_t2[n], 2];
  for (n in 1:N_t2) p_t2_pred[n]  = g[sub_t2[n], 2] + (1 - 2 * g[sub_t2[n], 2]) * (1 - exp(-fmax(mu_t2_pred[n], .001)));
  y_pred_t2 = bernoulli_rng(p_t2_pred);
  
}

