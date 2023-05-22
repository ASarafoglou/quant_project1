data {
  int<lower=1> D;                     // # Dimensions of the model: Nr. of quantifiers
  int<lower=1> I;                     // # Participants
  int<lower=1> T;                     // # Timepoints
  int<lower=0> N_t1;                  // # Timepoint 1: Observations/Rows in dataset
  int<lower=0,upper=1> y_t1[N_t1];    // # Data 0,1
  vector[N_t1] cperc_t1;              // # Centered Percentages
  int<lower=1,upper=I> sub_t1[N_t1];  // # Participant vector
  int<lower=0,upper=1> fewer_t1[N_t1];// # Quantifier
  int<lower=0,upper=1> more_t1[N_t1]; // # More than half
  int<lower=0,upper=1> few_t1[N_t1];  // # Few
  int<lower=0,upper=1> many_t1[N_t1]; // # Many
  int<lower=0,upper=1> most_t1[N_t1]; // # Most
  int<lower=0> N_t2;                  // # Timepoint 2
  int<lower=0,upper=1> y_t2[N_t2];       
  vector[N_t2] cperc_t2;                
  int<lower=1,upper=I> sub_t2[N_t2];    
  int<lower=0,upper=1> fewer_t2[N_t2];  
  int<lower=0,upper=1> more_t2[N_t2];    
  int<lower=0,upper=1> few_t2[N_t2];     
  int<lower=0,upper=1> many_t2[N_t2];    
  int<lower=0,upper=1> most_t2[N_t2];    
}

parameters {
  // parameters
  real<lower=0> log_a_fewer_t1[I];
  real<lower=0> log_a_fewer_t2[I];
  real<lower=0> log_a_more_t1[I];
  real<lower=0> log_a_more_t2[I];
  real<lower=0> log_a_few_t1[I];
  real<lower=0> log_a_few_t2[I];
  real<lower=0> log_a_many_t1[I];
  real<lower=0> log_a_many_t2[I];  
  real<lower=0> log_a_most_t1[I];
  real<lower=0> log_a_most_t2[I];
  real<lower=0, upper=0.5> g_fewer_t1[I];
  real<lower=0, upper=0.5> g_fewer_t2[I];
  real<lower=0, upper=0.5> g_more_t1[I];
  real<lower=0, upper=0.5> g_more_t2[I];
  real<lower=0, upper=0.5> g_few_t1[I];
  real<lower=0, upper=0.5> g_few_t2[I];
  real<lower=0, upper=0.5> g_many_t1[I];
  real<lower=0, upper=0.5> g_many_t2[I];  
  real<lower=0, upper=0.5> g_most_t1[I];
  real<lower=0, upper=0.5> g_most_t2[I];
  real<lower=-0.5, upper=0> b_fewer_t1[I];
  real<lower=0, upper=0.5> b_more_t1[I];
  real<lower=-0.5, upper=0> b_few_t1[I];
  real<lower=-0.5, upper=0.5> b_many_t1[I];
  real<lower=0, upper=0.5> b_most_t1[I];
  real<lower=-0.5, upper=0> b_fewer_t2[I];
  real<lower=0, upper=0.5> b_more_t2[I];
  real<lower=-0.5, upper=0> b_few_t2[I];
  real<lower=-0.5, upper=0.5> b_many_t2[I];
  real<lower=0, upper=0.5> b_most_t2[I];
  // group-level means
  real<lower=0, upper=0.5> muhat_log_a_fewer_t1;
  real<lower=0, upper=0.5> muhat_log_a_fewer_t2;
  real<lower=0, upper=0.5> muhat_log_a_more_t1;
  real<lower=0, upper=0.5> muhat_log_a_more_t2;
  real<lower=0, upper=0.5> muhat_log_a_few_t1; 
  real<lower=0, upper=0.5> muhat_log_a_few_t2; 
  real<lower=0, upper=0.5> muhat_log_a_many_t1;
  real<lower=0, upper=0.5> muhat_log_a_many_t2;
  real<lower=0, upper=0.5> muhat_log_a_most_t1;
  real<lower=0, upper=0.5> muhat_log_a_most_t2;
  real<lower=0, upper=0.5> muhat_g_fewer_t1;
  real<lower=0, upper=0.5> muhat_g_fewer_t2;
  real<lower=0, upper=0.5> muhat_g_more_t1;
  real<lower=0, upper=0.5> muhat_g_more_t2;
  real<lower=0, upper=0.5> muhat_g_few_t1; 
  real<lower=0, upper=0.5> muhat_g_few_t2; 
  real<lower=0, upper=0.5> muhat_g_many_t1;
  real<lower=0, upper=0.5> muhat_g_many_t2;
  real<lower=0, upper=0.5> muhat_g_most_t1;
  real<lower=0, upper=0.5> muhat_g_most_t2;
  real<lower=-0.5, upper=0> muhat_b_fewer_t1; 
  real<lower=-0.5, upper=0> muhat_b_fewer_t2; 
  real<lower=0, upper=0.5> muhat_b_more_t1;  
  real<lower=0, upper=0.5> muhat_b_more_t2; 
  real<lower=-0.5, upper=0> muhat_b_few_t1; 
  real<lower=-0.5, upper=0> muhat_b_few_t2;
  real<lower=-0.5, upper=0.5> muhat_b_many_t1; 
  real<lower=-0.5, upper=0.5> muhat_b_many_t2;
  real<lower=0, upper=0.5> muhat_b_most_t1;   
  real<lower=0, upper=0.5> muhat_b_most_t2;   
  // group-level variance
  matrix[I, T]dhat_tilde_a_fewer;
  matrix[I, T]dhat_tilde_a_more;
  matrix[I, T]dhat_tilde_a_few;
  matrix[I, T]dhat_tilde_a_many;
  matrix[I, T]dhat_tilde_a_most;
  matrix[I, T]dhat_tilde_g_fewer;
  matrix[I, T]dhat_tilde_g_more;
  matrix[I, T]dhat_tilde_g_few;
  matrix[I, T]dhat_tilde_g_many;
  matrix[I, T]dhat_tilde_g_most;
  matrix[I, T]dhat_tilde_b_fewer;
  matrix[I, T]dhat_tilde_b_more;
  matrix[I, T]dhat_tilde_b_few;
  matrix[I, T]dhat_tilde_b_many;
  matrix[I, T]dhat_tilde_b_most;
  cholesky_factor_corr[T] L_Omega_a_fewer;
  cholesky_factor_corr[T] L_Omega_a_more;
  cholesky_factor_corr[T] L_Omega_a_few;
  cholesky_factor_corr[T] L_Omega_a_many;
  cholesky_factor_corr[T] L_Omega_a_most;
  cholesky_factor_corr[T] L_Omega_g_fewer;
  cholesky_factor_corr[T] L_Omega_g_more;
  cholesky_factor_corr[T] L_Omega_g_few;
  cholesky_factor_corr[T] L_Omega_g_many;
  cholesky_factor_corr[T] L_Omega_g_most;
  cholesky_factor_corr[T] L_Omega_b_fewer;
  cholesky_factor_corr[T] L_Omega_b_more;
  cholesky_factor_corr[T] L_Omega_b_few;
  cholesky_factor_corr[T] L_Omega_b_many;
  cholesky_factor_corr[T] L_Omega_b_most;
  vector<lower=0>[T] sigma_mu_a_fewer;
  vector<lower=0>[T] sigma_mu_a_more;
  vector<lower=0>[T] sigma_mu_a_few;
  vector<lower=0>[T] sigma_mu_a_many;
  vector<lower=0>[T] sigma_mu_a_most;
  vector<lower=0>[T] sigma_mu_g_fewer;
  vector<lower=0>[T] sigma_mu_g_more;
  vector<lower=0>[T] sigma_mu_g_few;
  vector<lower=0>[T] sigma_mu_g_many;
  vector<lower=0>[T] sigma_mu_g_most;
  vector<lower=0>[T] sigma_mu_b_fewer;
  vector<lower=0>[T] sigma_mu_b_more;
  vector<lower=0>[T] sigma_mu_b_few;
  vector<lower=0>[T] sigma_mu_b_many;
  vector<lower=0>[T] sigma_mu_b_most;
}

transformed parameters {
  matrix[I, T] dhat_a_fewer;
  matrix[I, T] dhat_a_more;
  matrix[I, T] dhat_a_few;
  matrix[I, T] dhat_a_many;
  matrix[I, T] dhat_a_most;
  matrix[I, T] dhat_g_fewer;
  matrix[I, T] dhat_g_more;
  matrix[I, T] dhat_g_few;
  matrix[I, T] dhat_g_many;
  matrix[I, T] dhat_g_most;
  matrix[I, T] dhat_b_fewer;
  matrix[I, T] dhat_b_more;
  matrix[I, T] dhat_b_few;
  matrix[I, T] dhat_b_many;
  matrix[I, T] dhat_b_most;
  vector[I] dhat_a_fewer_t1;
  vector[I] dhat_a_fewer_t2;
  vector[I] dhat_a_more_t1;
  vector[I] dhat_a_more_t2;
  vector[I] dhat_a_few_t1;
  vector[I] dhat_a_few_t2;
  vector[I] dhat_a_many_t1;
  vector[I] dhat_a_many_t2;
  vector[I] dhat_a_most_t1;
  vector[I] dhat_a_most_t2;
  vector[I] dhat_g_fewer_t1;
  vector[I] dhat_g_fewer_t2;
  vector[I] dhat_g_more_t1;
  vector[I] dhat_g_more_t2;
  vector[I] dhat_g_few_t1;
  vector[I] dhat_g_few_t2;
  vector[I] dhat_g_many_t1;
  vector[I] dhat_g_many_t2;
  vector[I] dhat_g_most_t1;
  vector[I] dhat_g_most_t2;
  vector[I] dhat_b_fewer_t1;
  vector[I] dhat_b_fewer_t2;
  vector[I] dhat_b_more_t1;
  vector[I] dhat_b_more_t2;
  vector[I] dhat_b_few_t1;
  vector[I] dhat_b_few_t2;
  vector[I] dhat_b_many_t1;
  vector[I] dhat_b_many_t2;
  vector[I] dhat_b_most_t1;
  vector[I] dhat_b_most_t2; 
  vector[I] mu_a_fewer_t1;
  vector[I] mu_a_fewer_t2;
  vector[I] mu_a_more_t1;
  vector[I] mu_a_more_t2;
  vector[I] mu_a_few_t1;
  vector[I] mu_a_few_t2;
  vector[I] mu_a_many_t1;
  vector[I] mu_a_many_t2;
  vector[I] mu_a_most_t1;
  vector[I] mu_a_most_t2;
  vector[I] mu_log_a_fewer_t1;
  vector[I] mu_log_a_fewer_t2;
  vector[I] mu_log_a_more_t1;
  vector[I] mu_log_a_more_t2;
  vector[I] mu_log_a_few_t1;
  vector[I] mu_log_a_few_t2;
  vector[I] mu_log_a_many_t1;
  vector[I] mu_log_a_many_t2;
  vector[I] mu_log_a_most_t1;
  vector[I] mu_log_a_most_t2;
  vector[I] mu_g_fewer_t1;
  vector[I] mu_g_fewer_t2;
  vector[I] mu_g_more_t1;
  vector[I] mu_g_more_t2;
  vector[I] mu_g_few_t1;
  vector[I] mu_g_few_t2;
  vector[I] mu_g_many_t1;
  vector[I] mu_g_many_t2;
  vector[I] mu_g_most_t1;
  vector[I] mu_g_most_t2;
  vector[I] mu_b_fewer_t1;
  vector[I] mu_b_fewer_t2;
  vector[I] mu_b_more_t1;
  vector[I] mu_b_more_t2;
  vector[I] mu_b_few_t1;
  vector[I] mu_b_few_t2;
  vector[I] mu_b_many_t1;
  vector[I] mu_b_many_t2;
  vector[I] mu_b_most_t1;
  vector[I] mu_b_most_t2; 
  
  mu_a_fewer_t1 = exp(mu_log_a_fewer_t1);
  mu_a_more_t1  = exp(mu_log_a_more_t1);
  mu_a_few_t1   = exp(mu_log_a_few_t1);
  mu_a_many_t1  = exp(mu_log_a_many_t1);
  mu_a_most_t1  = exp(mu_log_a_most_t1);
  mu_a_fewer_t2 = exp(mu_log_a_fewer_t2);
  mu_a_more_t2  = exp(mu_log_a_more_t2);
  mu_a_few_t2   = exp(mu_log_a_few_t2);
  mu_a_many_t2  = exp(mu_log_a_many_t2);
  mu_a_most_t2  = exp(mu_log_a_most_t2);
  
 dhat_a_fewer = (diag_pre_multiply(sigma_mu_a_fewer, L_Omega_a_fewer) * dhat_tilde_a_fewer)';
 dhat_a_more  = (diag_pre_multiply(sigma_mu_a_more , L_Omega_a_more)  * dhat_tilde_a_more)';
 dhat_a_few   = (diag_pre_multiply(sigma_mu_a_few  , L_Omega_a_few)   * dhat_tilde_a_few)';
 dhat_a_many  = (diag_pre_multiply(sigma_mu_a_many , L_Omega_a_many)  * dhat_tilde_a_many)';
 dhat_a_most  = (diag_pre_multiply(sigma_mu_a_most , L_Omega_a_most)  * dhat_tilde_a_most)';
  for (i in 1:I) {
    dhat_a_fewer_t1[i] = dhat_a_fewer[i, 1];
    dhat_a_fewer_t2[i] = dhat_a_fewer[i, 2];
    dhat_a_more_t1[i]  = dhat_a_more[i, 1];
    dhat_a_more_t2[i]  = dhat_a_more[i, 2];
    dhat_a_few_t1[i]   = dhat_a_few[i, 1];
    dhat_a_few_t2[i]   = dhat_a_few[i, 2];
    dhat_a_many_t1[i]  = dhat_a_many[i, 1];
    dhat_a_many_t2[i]  = dhat_a_many[i, 2];
    dhat_a_most_t1[i]  = dhat_a_most[i, 1];
    dhat_a_most_t2[i]  = dhat_a_most[i, 2];
    mu_log_a_fewer_t1[i] = muhat_log_a_fewer_t1 + dhat_a_fewer_t1[i];
    mu_log_a_fewer_t2[i] = muhat_log_a_fewer_t2 + dhat_a_fewer_t2[i];
    mu_log_a_more_t1[i]  = muhat_log_a_more_t1  + dhat_a_more_t1[i];
    mu_log_a_more_t2[i]  = muhat_log_a_more_t2  + dhat_a_more_t2[i];
    mu_log_a_few_t1[i]   = muhat_log_a_few_t1   + dhat_a_few_t1[i];
    mu_log_a_few_t2[i]   = muhat_log_a_few_t2   + dhat_a_few_t2[i];
    mu_log_a_many_t1[i]  = muhat_log_a_many_t1  + dhat_a_many_t1[i];
    mu_log_a_many_t2[i]  = muhat_log_a_many_t2  + dhat_a_many_t2[i];
    mu_log_a_most_t1[i]  = muhat_log_a_most_t1  + dhat_a_most_t1[i];
    mu_log_a_most_t2[i]  = muhat_log_a_most_t2  + dhat_a_most_t2[i];
  }

 dhat_g_fewer = (diag_pre_multiply(sigma_mu_g_fewer, L_Omega_g_fewer) * dhat_tilde_g_fewer)';
 dhat_g_more  = (diag_pre_multiply(sigma_mu_g_more , L_Omega_g_more)  * dhat_tilde_g_more)';
 dhat_g_few   = (diag_pre_multiply(sigma_mu_g_few  , L_Omega_g_few)   * dhat_tilde_g_few)';
 dhat_g_many  = (diag_pre_multiply(sigma_mu_g_many , L_Omega_g_many)  * dhat_tilde_g_many)';
 dhat_g_most  = (diag_pre_multiply(sigma_mu_g_most , L_Omega_g_most)  * dhat_tilde_g_most)';
  for (i in 1:I) {
    dhat_g_fewer_t1[i] = dhat_g_fewer[i, 1];
    dhat_g_fewer_t2[i] = dhat_g_fewer[i, 2];
    dhat_g_more_t1[i]  = dhat_g_more[i, 1];
    dhat_g_more_t2[i]  = dhat_g_more[i, 2];
    dhat_g_few_t1[i]   = dhat_g_few[i, 1];
    dhat_g_few_t2[i]   = dhat_g_few[i, 2];
    dhat_g_many_t1[i]  = dhat_g_many[i, 1];
    dhat_g_many_t2[i]  = dhat_g_many[i, 2];
    dhat_g_most_t1[i]  = dhat_g_most[i, 1];
    dhat_g_most_t2[i]  = dhat_g_most[i, 2];
    mu_g_fewer_t1[i]   = muhat_g_fewer_t1 + dhat_g_fewer_t1[i];
    mu_g_fewer_t2[i]   = muhat_g_fewer_t2 + dhat_g_fewer_t2[i];
    mu_g_more_t1[i]    = muhat_g_more_t1  + dhat_g_more_t1[i];
    mu_g_more_t2[i]    = muhat_g_more_t2  + dhat_g_more_t2[i];
    mu_g_few_t1[i]     = muhat_g_few_t1   + dhat_g_few_t1[i];
    mu_g_few_t2[i]     = muhat_g_few_t2   + dhat_g_few_t2[i];
    mu_g_many_t1[i]    = muhat_g_many_t1  + dhat_g_many_t1[i];
    mu_g_many_t2[i]    = muhat_g_many_t2  + dhat_g_many_t2[i];
    mu_g_most_t1[i]    = muhat_g_most_t1  + dhat_g_most_t1[i];
    mu_g_most_t2[i]    = muhat_g_most_t2  + dhat_g_most_t2[i];
  }

 dhat_b_fewer = (diag_pre_multiply(sigma_mu_b_fewer, L_Omega_b_fewer) * dhat_tilde_b_fewer)';
 dhat_b_more  = (diag_pre_multiply(sigma_mu_b_more , L_Omega_b_more)  * dhat_tilde_b_more)';
 dhat_b_few   = (diag_pre_multiply(sigma_mu_b_few  , L_Omega_b_few)   * dhat_tilde_b_few)';
 dhat_b_many  = (diag_pre_multiply(sigma_mu_b_many , L_Omega_b_many)  * dhat_tilde_b_many)';
 dhat_b_most  = (diag_pre_multiply(sigma_mu_b_most , L_Omega_b_most)  * dhat_tilde_b_most)';
  for (i in 1:I) {
    dhat_b_fewer_t1[i] = dhat_b_fewer[i, 1];
    dhat_b_fewer_t2[i] = dhat_b_fewer[i, 2];
    dhat_b_more_t1[i]  = dhat_b_more[i, 1];
    dhat_b_more_t2[i]  = dhat_b_more[i, 2];
    dhat_b_few_t1[i]   = dhat_b_few[i, 1];
    dhat_b_few_t2[i]   = dhat_b_few[i, 2];
    dhat_b_many_t1[i]  = dhat_b_many[i, 1];
    dhat_b_many_t2[i]  = dhat_b_many[i, 2];
    dhat_b_most_t1[i]  = dhat_b_most[i, 1];
    dhat_b_most_t2[i]  = dhat_b_most[i, 2];
    mu_b_fewer_t1[i]   = muhat_b_fewer_t1 + dhat_b_fewer_t1[i];
    mu_b_fewer_t2[i]   = muhat_b_fewer_t2 + dhat_b_fewer_t2[i];
    mu_b_more_t1[i]    = muhat_b_more_t1  + dhat_b_more_t1[i];
    mu_b_more_t2[i]    = muhat_b_more_t2  + dhat_b_more_t2[i];
    mu_b_few_t1[i]     = muhat_b_few_t1   + dhat_b_few_t1[i];
    mu_b_few_t2[i]     = muhat_b_few_t2   + dhat_b_few_t2[i];
    mu_b_many_t1[i]    = muhat_b_many_t1  + dhat_b_many_t1[i];
    mu_b_many_t2[i]    = muhat_b_many_t2  + dhat_b_many_t2[i];
    mu_b_most_t1[i]    = muhat_b_most_t1  + dhat_b_most_t1[i];
    mu_b_most_t2[i]    = muhat_b_most_t2  + dhat_b_most_t2[i];
  }

  
}

model {
  vector[N_t1] mu_t1;
  vector[N_t2] mu_t2;
  vector[N_t1] p_t1;
  vector[N_t2] p_t2;
  
  // hyperparameters: means of mutlivariate distributions
    mu_log_a_fewer_t1 ~ normal(-4, 0.1);  
    mu_log_a_fewer_t2 ~ normal(-4, 0.1);  
    mu_log_a_more_t1  ~ normal(-4, 0.1);  
    mu_log_a_more_t2  ~ normal(-4, 0.1);  
    mu_log_a_few_t1   ~ normal(-4, 0.1);  
    mu_log_a_few_t2   ~ normal(-4, 0.1);  
    mu_log_a_many_t1  ~ normal(-4, 0.1);  
    mu_log_a_many_t2  ~ normal(-4, 0.1);  
    mu_log_a_most_t1  ~ normal(-4, 0.1);  
    mu_log_a_most_t2  ~ normal(-4, 0.1);
    mu_g_fewer_t1 ~ normal(0.1, 0.1);  
    mu_g_fewer_t2 ~ normal(0.1, 0.1);  
    mu_g_more_t1  ~ normal(0.1, 0.1);  
    mu_g_more_t2  ~ normal(0.1, 0.1);  
    mu_g_few_t1   ~ normal(0.1, 0.1);  
    mu_g_few_t2   ~ normal(0.1, 0.1);  
    mu_g_many_t1  ~ normal(0.1, 0.1);  
    mu_g_many_t2  ~ normal(0.1, 0.1);  
    mu_g_most_t1  ~ normal(0.1, 0.1);  
    mu_g_most_t2  ~ normal(0.1, 0.1);
    mu_b_fewer_t1 ~ normal(0, 0.1);  
    mu_b_fewer_t2 ~ normal(0, 0.1);  
    mu_b_more_t1  ~ normal(0, 0.1);  
    mu_b_more_t2  ~ normal(0, 0.1);  
    mu_b_few_t1   ~ normal(0, 0.1);  
    mu_b_few_t2   ~ normal(0, 0.1);  
    mu_b_many_t1  ~ normal(0, 0.1);  
    mu_b_many_t2  ~ normal(0, 0.1);  
    mu_b_most_t1  ~ normal(0, 0.1);  
    mu_b_most_t2  ~ normal(0, 0.1);  
  
  
  L_Omega_a_fewer ~ lkj_corr_cholesky(4);
  L_Omega_a_more  ~ lkj_corr_cholesky(4); 
  L_Omega_a_few   ~ lkj_corr_cholesky(4);
  L_Omega_a_many  ~ lkj_corr_cholesky(4); 
  L_Omega_a_most  ~ lkj_corr_cholesky(4); 
  L_Omega_g_fewer ~ lkj_corr_cholesky(4);
  L_Omega_g_more  ~ lkj_corr_cholesky(4); 
  L_Omega_g_few   ~ lkj_corr_cholesky(4);
  L_Omega_g_many  ~ lkj_corr_cholesky(4); 
  L_Omega_g_most  ~ lkj_corr_cholesky(4); 
  L_Omega_b_fewer ~ lkj_corr_cholesky(4);
  L_Omega_b_more  ~ lkj_corr_cholesky(4); 
  L_Omega_b_few   ~ lkj_corr_cholesky(4);
  L_Omega_b_many  ~ lkj_corr_cholesky(4); 
  L_Omega_b_most  ~ lkj_corr_cholesky(4); 
  
  sigma ~ gamma(1, 4);
  to_vector(dhat_tilde_b_fewer) ~ std_normal;
  to_vector(dhat_tilde_g) ~ std_normal;
  
  // parameters: thresholds
  for (i in 1:I) b_fewer_t1[i] ~ normal(mu_b_fewer[1], 0.1);
  for (i in 1:I) b_more_t1[i]  ~ normal(mu_b_more[1], 0.1);
  for (i in 1:I) b_few_t1[i]   ~ normal(mu_b_few[1], 0.5);
  for (i in 1:I) b_many_t1[i]  ~ normal(mu_b_many[1], 0.5);
  for (i in 1:I) b_most_t1[i]  ~ normal(mu_b_most[1], 0.5);
  for (i in 1:I) b_fewer_t2[i] ~ normal(mu_b_fewer[2], 0.1);
  for (i in 1:I) b_more_t2[i]  ~ normal(mu_b_more[2], 0.1);
  for (i in 1:I) b_few_t2[i]   ~ normal(mu_b_few[2], 0.5);
  for (i in 1:I) b_many_t2[i]  ~ normal(mu_b_many[2], 0.5);
  for (i in 1:I) b_most_t2[i]  ~ normal(mu_b_most[2], 0.5);
  // vagueness  
  for (i in 1:I) log_a_fewer_t1[i] ~ normal(mu_a_fewer[1], 0.1);
  for (i in 1:I) log_a_more_t1[i]  ~ normal(mu_a_more[1], 0.1);
  for (i in 1:I) log_a_few_t1[i]   ~ normal(mu_a_few[1], 0.5);
  for (i in 1:I) log_a_many_t1[i]  ~ normal(mu_a_many[1], 0.5);
  for (i in 1:I) log_a_most_t1[i]  ~ normal(mu_a_most[1], 0.5);
  for (i in 1:I) log_a_fewer_t2[i] ~ normal(mu_a_fewer[2], 0.1);
  for (i in 1:I) log_a_more_t2[i]  ~ normal(mu_a_more[2], 0.1);
  for (i in 1:I) log_a_few_t2[i]   ~ normal(mu_a_few[2], 0.5);
  for (i in 1:I) log_a_many_t2[i]  ~ normal(mu_a_many[2], 0.5);
  for (i in 1:I) log_a_most_t2[i]  ~ normal(mu_a_most[2], 0.5);
  // guessing
  for (i in 1:I)
    g_t1[i] ~ normal(mu_g[1], 0.1);
  for (i in 1:I)
    g_t2[i] ~ normal(mu_g[2], 0.1);

  // timepoint 1
  for (n in 1:N_t1)
    mu_t1[n] = fewer_t1[n] * (cperc_t1[n] - b_fewer_t1[sub_t1[n]]) / a_t1[sub_t1[n], 1] + 
            more_t1[n] * (cperc_t1[n] - b_fewer_t1[sub_t1[n]]) / a_t1[sub_t1[n], 2] + 
            few_t1[n] * (cperc_t1[n] - b_few_t1[sub_t1[n]]) / a_t1[sub_t1[n], 3] + 
            many_t1[n] * (cperc_t1[n] - b_many_t1[sub_t1[n]]) / a_t1[sub_t1[n], 4] + 
            most_t1[n] * (cperc_t1[n] - b_most_t1[sub_t1[n]]) / a_t1[sub_t1[n], 5];
  for (n in 1:N_t1)
    p_t1[n] = fewer_t1[n] * g_t1[sub_t1[n], 1] + 
    more_t1[n] * g_t1[sub_t1[n], 2] + 
    few_t1[n] * g_t1[sub_t1[n], 3] + 
    many_t1[n] * g_t1[sub_t1[n], 4] + 
    most_t1[n] * g_t1[sub_t1[n], 5] + 
    (1 - 2 * (fewer_t1[n] * g_t1[sub_t1[n], 1] + 
    more_t1[n] * g_t1[sub_t1[n], 2] + 
    few_t1[n] * g_t1[sub_t1[n], 3] +
    most_t1[n] * g_t1[sub_t1[n], 4] +
    most_t1[n] * g_t1[sub_t1[n], 5])) *
  (1 - exp(-fmax(mu_t1[n], .001))); 
  y_t1 ~ bernoulli(p_t1);         
            
  // timepoint 2
  for (n in 1:N_t2)
    mu_t2[n] = fewer_t2[n] * (cperc_t2[n] - b_fewer_t2[sub_t2[n]]) / a_t2[sub_t2[n], 1] + 
            more_t2[n] * (cperc_t2[n] - b_fewer_t2[sub_t2[n]]) / a_t2[sub_t2[n], 2] + 
            few_t2[n] * (cperc_t2[n] - b_few_t2[sub_t2[n]]) / a_t2[sub_t2[n], 3] + 
            many_t2[n] * (cperc_t2[n] - b_many_t2[sub_t2[n]]) / a_t2[sub_t2[n], 4] + 
            most_t2[n] * (cperc_t2[n] - b_most_t2[sub_t2[n]]) / a_t2[sub_t2[n], 5];
  for (n in 1:N_t2)
    p_t2[n] = fewer_t2[n] * g_t2[sub_t2[n], 1] + 
    more_t2[n] * g_t2[sub_t2[n], 2] + 
    few_t2[n] * g_t2[sub_t2[n], 3] + 
    many_t2[n] * g_t2[sub_t2[n], 4] + 
    most_t2[n] * g_t2[sub_t2[n], 5] + 
    (1 - 2 * (fewer_t2[n] * g_t2[sub_t2[n], 1] + 
    more_t2[n] * g_t2[sub_t2[n], 2] + 
    few_t2[n] * g_t2[sub_t2[n], 3] +
    most_t2[n] * g_t2[sub_t2[n], 4] +
    most_t2[n] * g_t2[sub_t2[n], 5])) *
  (1 - exp(-fmax(mu_t2[n], .001))); 
  y_t2 ~ bernoulli(p_t2); 
}


