data {
  int<lower=1> D;                     // # Dimensions of the model: nr. of quantifiers
  int<lower=0> N;                     // # Observations: rows in dataset
  int<lower=1> I;                     // # Participants
  vector[N] cperc;                    // # Centered Percentages
  int<lower=1,upper=I> sub[N];        // # Participant vector
  int<lower=0,upper=1> fewer[N];      // # Fewer than half
  int<lower=0,upper=1> more[N];       // # More than half
}

parameters {
  vector<lower=0>[D] alphaprior[I];         // # Vectors of alphas
  vector[D] betaprior[I];                   // # Vectors of betas
  vector<lower=0,upper=1>[D] gammaprior[I]; // # Vector of gammas
  real deltaprior[D];                       // # Means of betas
  real<lower=0> sigmaprior[D];              // # Standard deviation of betas
  real nuprior[D];                          // # Means of alphas
  real<lower=0> sigmaalphaprior[D];         // # Standard deviation of alphas
  real<lower=0, upper=1> mugprior[D];       // # Mode of gammas
  real<lower=0> etaprior[D];                // # Concentration of gammas (prior sample size a + b)
}

transformed parameters {
  real<lower=0> aprior[D];
  real<lower=0> bprior[D];

  for (d in 1:D)
    aprior[d] = etaprior[d] * mugprior[d];
  for (d in 1:D)
    bprior[d] = etaprior[d] * (1 - mugprior[d]);
}

model {
  deltaprior         ~ normal(0, 5);
  sigmaprior         ~ student_t(4, 0, 0.3);
  nuprior            ~ normal(-4, 1);
  sigmaalphaprior    ~ student_t(4, 0, 1);
  mugprior           ~ beta(6, 14);
  etaprior           ~ exponential(0.05);
  
  for (i in 1:I)
    betaprior[i] ~ normal(deltaprior, sigmaprior);
  for (i in 1:I)
    alphaprior[i] ~ lognormal(nuprior, sigmaalphaprior);
  for (i in 1:I)
    gammaprior[i] ~ beta(aprior, bprior);
}

generated quantities {
  vector[N] mu_pred;
  vector[N] p_pred;
  int y_pred[N];
  
  // Prior Predictive
  for (n in 1:N)
  mu_pred[n] = fewer[n] * (cperc[n] - betaprior[sub[n], 1]) / alphaprior[sub[n], 1] + 
          more[n] * (cperc[n] - betaprior[sub[n], 2]) / alphaprior[sub[n], 2];
  for (n in 1:N)
    p_pred[n] = fewer[n] * gammaprior[sub[n], 1] + 
    more[n] * gammaprior[sub[n], 2] + 
    (1 - 2 * (fewer[n] * gammaprior[sub[n], 1] + 
    more[n] * gammaprior[sub[n], 2])) *
    (1 - exp(-fmax(mu_pred[n], .001))); 
   y_pred =  bernoulli_rng(p_pred);
}
