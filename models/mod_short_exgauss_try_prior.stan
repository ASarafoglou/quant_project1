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
  vector<lower=0>[D] alphaprior[I];             // # Vectors of alphas
  vector<lower=0,upper=1>[D] betarawprior[I];   // # Vectors of betas
  vector<lower=0,upper=1>[D] gammarawprior[I];  // # Vector of gammas
  real nuprior[D];                              // # Means of alphas
  real<lower=0> sigmaalphaprior[D];             // # Standard deviation of alphas
  real<lower=0, upper=1> mugprior[D];          // # Mode of gammas
  real<lower=0> etagprior[D];                  // # Concentration of gammas (prior sample size a + b)
  real<lower=0, upper=1> mubprior[D];          // # Mode of betas
  real<lower=0> etabprior[D];                  // # Concentration of betas (prior sample size a + b)
}

transformed parameters {
  real<lower=0> agprior[D];
  real<lower=0> abprior[D];
  real<lower=0> bgprior[D];
  real<lower=0> bbprior[D];
  
  vector<lower=-0.5,upper=0.5>[D] betaprior[I]; 
  vector<lower=0,upper=0.5>[D] gammaprior[I];   

  for (d in 1:D)
    agprior[d] = etagprior[d] * mugprior[d];
  for (d in 1:D)
    bgprior[d] = etagprior[d] * (1 - mugprior[d]);
  for (d in 1:D)
    abprior[d] = etabprior[d] * mubprior[d];
  for (d in 1:D)
    bbprior[d] = etabprior[d] * (1 - mubprior[d]);
  for (i in 1:I)
    betaprior[i,1] = betarawprior[i,1] - 0.5; // shift beta distribution [-0.5, 0.5]
  for (i in 1:I)
    betaprior[i,2] = betarawprior[i,2] - 0.5; // shift beta distribution [-0.5, 0.5]
  for (i in 1:I)
    gammaprior[i,1] = gammarawprior[i,1] * 0.5; // shift beta distribution [0, 0.5]
  for (i in 1:I)
    gammaprior[i,2] = gammarawprior[i,2] * 0.5; // shift beta distribution [0, 0.5]
}

model {
  nuprior           ~ normal(-4, 1);
  sigmaalphaprior   ~ student_t(4, 0, 1);
  mugprior          ~ beta(2, 20);
  etagprior         ~ exponential(0.045);
  mubprior          ~ beta(5, 5);
  etabprior         ~ exponential(0.1);
  
  for (i in 1:I)
    betarawprior[i] ~ beta(abprior, bbprior);
  for (i in 1:I)
    alphaprior[i] ~ lognormal(nuprior, sigmaalphaprior);
  for (i in 1:I)
    gammarawprior[i] ~ beta(agprior, bgprior);
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
