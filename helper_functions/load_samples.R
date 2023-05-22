# load samples and create necessary objects

load(paste0('../output/', filename))
  

list_of_draws  <- rstan::extract(logmodfit)
array_of_draws <- as.array(logmodfit)
posterior2     <- rstan::extract(logmodfit, inc_warmup = FALSE, permuted = FALSE)
sampler_params <- rstan::get_sampler_params(logmodfit, inc_warmup = FALSE)
