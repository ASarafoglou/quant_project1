rm(list=ls())
setwd("~/GitProjects/quant_project1/dev")
library("dplyr")
library("MCMCpack")
library("LaplacesDemon")
library("rstan")
library("bayesplot")
library("plotrix")
library("RColorBrewer")
# data preprocessing
library("papaja")

######################
## load predictions ##
######################



load("../output/out_prior_predictions_exgauss_priors.rda")
list_of_draws  <- rstan::extract(logmodfit)
array_of_draws <- as.array(logmodfit)
posterior2 <- extract(logmodfit, inc_warmup = FALSE, permuted = FALSE)
dat <- read.csv("../data/exp1-preprocessed.csv")

###############
## functions ##
###############

drawDistribution <- function(pid, quantifier = 'Fewer than half', parameter = 'alpha', 
                             draws=list_of_draws, correct = 0.90){
  
  colid           <- ifelse(quantifier == 'Fewer than half', 1, 2)
  samples         <- list_of_draws[[parameter]][, pid, colid]
  median_samples  <- printnum(median(samples))
  samples         <- samples[samples < quantile(samples, correct)] # corrected samples
  density_samples <- density(samples)
  hist_samples    <- hist(samples, plot = FALSE)
  yhigh           <- max(max(density_samples$y), max(hist_samples$density))
  
  hist(samples,
       las = 1,
       freq = FALSE,
       ylim = c(0, yhigh),
       main = paste('median', parameter, '=', median_samples),
       ylab = 'Density',
       xlab = paste(quantifier, pid), col = 'lightgrey')
  lines(density(samples), lwd = 2)
}

drawIndOverallEffect <- function(quantifier = 'Fewer than half', parameter = 'alpha', 
                                 draws=list_of_draws){
  
  colid           <- ifelse(quantifier == 'Fewer than half', 1, 2)
  samples         <- list_of_draws[[parameter]][, , colid]
  overall_effect  <- quantile(samples, c(0.025, 0.5, 0.975))
  ind_effect      <- t(apply(samples, 2, quantile, probs=c(0.025, 0.5, 0.975)))
  ind_effect      <- ind_effect[order(ind_effect[,2]),]
  colnames(ind_effect) <- c('lower', 'median', 'upper')
  
  ymin  <- min(ind_effect[, 'lower'])
  yhigh <- max(ind_effect[, 'upper'])
  
  plot(1:nrow(ind_effect), ind_effect[, 2],
       ylim = c(ymin, yhigh),
       ylab = paste(parameter, quantifier, sep = ': '), 
       xlab = 'Participant',
       axes = FALSE, type = 'n')
  axis(side = 1)
  axis(side = 2, las = 1)
  arrows(1:nrow(ind_effect), round(ind_effect[, 'lower'],4), 
         1:nrow(ind_effect), round(ind_effect[, 'upper'],4), 
         code = 3, angle = 90, length = 0.1)
  points(1:nrow(ind_effect), ind_effect[, 2], pch = 21, bg = 'grey', cex = 1.5)
  abline(h=overall_effect[2], lty = 3, lwd = 2, col = 'red')
  
}

#######################
## check convergence ##
#######################

hist(summary(logmodfit)$summary[,"Rhat"]
     , breaks = 100
     , main = ""
     , xlab =  "Rhat")
quantile(summary(logmodfit)$summary[,"Rhat"], c(0.025, 0.5, 0.975))

# Both bulk-ESS and tail-ESS should be at least 100 
#(approximately) per Markov Chain in order to be reliable 
# and indicate that estimates of respective posterior quantiles are reliable.
hist(summary(logmodfit)$summary[,"n_eff"]
     , breaks = 100
     , main = ""
     , xlab =  "Number of effective samples")
quantile(summary(logmodfit)$summary[,"n_eff"], c(0.025, 0.5, 0.975))

sampler_params <- rstan::get_sampler_params(logmodfit, inc_warmup = FALSE)
sampler_params_chain1 <- sampler_params[[1]]
colnames(sampler_params_chain1)
code <- get_stancode(logmodfit)
cat(code)

# the initial values are not the problem
inits <- get_inits(logmodfit)
inits_chain1 <- inits[[1]]
inits_chain2 <- inits[[2]]
inits_chain3 <- inits[[3]]
print(inits_chain1$betaprior)
print(inits_chain2$betaprior)
print(inits_chain3$betaprior)
# or maybe they are?
inits_chain1[c('sigma2', 'sigma2alpha', 'sigma2prior', 'sigma2alphaprior')]
inits_chain2[c('sigma2', 'sigma2alpha', 'sigma2prior', 'sigma2alphaprior')]
inits_chain3[c('sigma2', 'sigma2alpha', 'sigma2prior', 'sigma2alphaprior')]

inits_chain1[c('alphaprior', 'betaprior')]
inits_chain2[c('alphaprior', 'betaprior')]
inits_chain3[c('alphaprior', 'betaprior')]

print(get_elapsed_time(logmodfit))

##########################
## inspect correlations ##
##########################

# all hyperparameters: nu, delta, sigma2, sigma2alpha
sigma <- dimnames(array_of_draws)$parameters[grepl('sigma', dimnames(array_of_draws)$parameters)][1:4]
nu <- dimnames(array_of_draws)$parameters[grepl('nu', dimnames(array_of_draws)$parameters)][1:2]
delta <- dimnames(array_of_draws)$parameters[grepl('delta', dimnames(array_of_draws)$parameters)][1:2]
hyperparameters <- c(sigma, nu, delta)

M <- cor(array_of_draws[,1,hyperparameters])
corrplot::corrplot(M, method = 'color', order = 'alphabet')

cor.test(array_of_draws[,3,"sigmaalpha[2]"],
         array_of_draws[,3,"nu[2]"])$estimate

estimate <- cor.test(array_of_draws[,3,"sigmaalpha[2]"],
         array_of_draws[,3,"nu[2]"])$estimate
plot(array_of_draws[,3,"sigmaalpha[2]"],
     array_of_draws[,3,"nu[2]"], pch = 21, bg = '#FDC086', 
     xlab = 'sigmaalpha[2]', ylab = 'nu[2]',
     bty = 'n', ylim = c(-9, -3),
     main = paste('corr =', round(estimate, 2)))

#######################
## caterpillar plots ##
#######################

# pars_to_plot <- c("sigma2[1]", "sigma2[2]") # looks good
# pars_to_plot <- c("sigma2prior[1]", "sigma2prior[2]") # looks okay
# pars_to_plot <- c("sigma2alpha[1]", "sigma2alpha[2]") # looks good
# pars_to_plot <- c("sigma2alphaprior[1]", "sigma2alphaprior[2]") # looks okay


pars_to_plot <- c("alphaprior[20,1]", "alphaprior[20,2]") # alphaprior has some weird values in it
pars_to_plot <- c("betaprior[20,1]", "betaprior[20,2]") 
pars_to_plot <- c("gammaprior[10,1]", "gammaprior[10,2]") 
pars_to_plot <- c("beta[20,1]", "beta[20,2]") 
# pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('betaprior', dimnames(array_of_draws)$parameters)]
# Relevant plots: convergence of hyperprior parameters
pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('nu', dimnames(array_of_draws)$parameters)][1:2]
pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('delta', dimnames(array_of_draws)$parameters)][1:2]
pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('nuprior', dimnames(array_of_draws)$parameters)]
pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('deltaprior', dimnames(array_of_draws)$parameters)]
pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('sigma', dimnames(array_of_draws)$parameters)]
# pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('eta', dimnames(array_of_draws)$parameters)]
# pars_to_plot <- dimnames(array_of_draws)$parameters[grepl('mug', dimnames(array_of_draws)$parameters)]

bayesplot::mcmc_trace(posterior2, facet_args = list(nrow = 2),
                      pars = pars_to_plot) + 
  facet_text(size = 15)

bayesplot::mcmc_scatter(
  as.matrix(logmodfit),
  pars = pars_to_plot, 
  np = nuts_params(logmodfit), 
  np_style = scatter_style_np(div_color = "green", div_alpha = 0.5)
)

bayesplot::mcmc_areas(
  array_of_draws, 
  pars = pars_to_plot,
  prob = 0.95, # 95% intervals
  prob_outer = 0.99, # 99%
  point_est = "median"
)

# Relevant plots: differences in quantifiers for the alpha and beta parameters
drawDistribution(20, quantifier = 'More than half', parameter = 'alphaprior')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'alphaprior')
drawDistribution(20, quantifier = 'More than half', parameter = 'betaprior')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'betaprior')
drawDistribution(20, quantifier = 'More than half', parameter = 'gammaprior')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'gammaprior')
drawDistribution(20, quantifier = 'More than half', parameter = 'sigmaprior')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'sigmaprior')

# now the posterior distributions
drawDistribution(20, quantifier = 'More than half', parameter = 'alpha')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'alpha')
drawDistribution(20, quantifier = 'More than half', parameter = 'beta')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'beta')
drawDistribution(20, quantifier = 'More than half', parameter = 'gamma')
drawDistribution(20, quantifier = 'Fewer than half', parameter = 'gamma')

########################
## inspect parameters ##
########################

N <- 20

# p_pred (probability to give a 'true' response)
# mu_pred (parameter for logistic link)
# y_pred (0 & 1 responses)
print(names(list_of_draws))

## check alpha
  dim(list_of_draws$alpha)
  round(colMeans(list_of_draws[['alpha']][, , 1:2]), 4)
  round(colMeans(list_of_draws[['alphaprior']][, , 1:2]), 4)
  apply(list_of_draws$alpha[,, 1], 2, median) 
  apply(list_of_draws$alpha[,, 2], 2, median)
  apply(list_of_draws$alphaprior[,, 1], 2, median)
  apply(list_of_draws$alphaprior[,, 2], 2, median)
  # round(colMeans(list_of_draws$alphaprior[, , 1:2]), 4)
  # one quantifier worked out, the other did not: what went wrong?
  median(list_of_draws$alpha[, , 1])
  median(list_of_draws$alpha[, , 2])
  median(list_of_draws$alphaprior[, , 1])
  median(list_of_draws$alphaprior[, , 2])
  
  apply(list_of_draws$alphaprior[,20,], 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
  
  par(mfrow = c(3, 2))
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'alphaprior')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'alphaprior')
  }
  
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'alpha')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'alpha')
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'alphaprior')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'alphaprior')

## check beta
  dim(list_of_draws$beta)
  round(colMeans(list_of_draws$beta[, , 1:2]), 4)
  round(colMeans(list_of_draws$betaprior[, , 1:2]), 4)
  # one quantifier worked out, the other did not: what went wrong?
  median(list_of_draws$beta[,, 1])
  median(list_of_draws$beta[,, 2])
  median(list_of_draws$betaprior[,, 1])
  median(list_of_draws$betaprior[,, 2])
  
  apply(list_of_draws$betaprior[,20,], 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
  
  par(mfrow = c(3, 2))
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'betaprior')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'betaprior')
  }
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'beta')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'beta')
  }
  
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'beta')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'beta')
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'betaprior')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'betaprior')
  

## check gamma
  dim(list_of_draws$gamma)
  round(colMeans(list_of_draws$gamma[, , 1:2]), 4)
  round(colMeans(list_of_draws$gammaprior[, , 1:2]), 4)
  # one quantifier worked out, the other did not: what went wrong?
  median(list_of_draws$gamma[, , 1])
  median(list_of_draws$gamma[, , 2])
  median(list_of_draws$gammaprior[, , 1])
  median(list_of_draws$gammaprior[, , 2])
  
  apply(list_of_draws$gammaprior[,20,], 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
  
  par(mfrow = c(3, 2))
  for(i in 1:N){
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'gamma')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'gammaprior')
  }
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'gamma')
    drawDistribution(i, quantifier = 'More than half', parameter = 'gammaprior')
  }
  
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'gamma')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'gamma')
  drawIndOverallEffect(quantifier = 'Fewer than half', parameter = 'gammaprior')
  drawIndOverallEffect(quantifier = 'More than half' , parameter = 'gammaprior')
  

## check hyperpriors
  apply(list_of_draws$sigma2, 2, median)
  apply(list_of_draws$sigma2prior, 2, median)
  apply(list_of_draws$sigma2alpha, 2, median)
  apply(list_of_draws$sigma2alphaprior, 2, median)
  apply(list_of_draws$nu, 2, median)
  apply(list_of_draws$nuprior, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))
  apply(list_of_draws$deltaprior, 2, function(x) quantile(x, c(0.025, 0.5, 0.975)))

############################
## inspect the likelihood ##
############################

  fewer <- predat$fewer
  more  <- predat$more
  cperc <- predat$cperc
  N     <- length(predat$y[- c(4059:4154)])
  betaprior <- list_of_draws$betaprior[1000,,]
  alphaprior <- list_of_draws$alphaprior[1000,,]
  sub <- predat$sub
  
mu_pred <- NULL
for (n in 1:N){
  mu_pred[n] = fewer[n] * (cperc[n] - betaprior[sub[n], 1]) / alphaprior[sub[n], 1] + 
    more[n] * (cperc[n] - betaprior[sub[n], 2]) / alphaprior[sub[n], 2]
}
  
  
###############################
## inspect response patterns ##
###############################

pred_names <- dimnames(array_of_draws)$parameters[grepl('y_pred', dimnames(array_of_draws)$parameters)]
head(pred_names); tail(pred_names)
length(rstan::get_inits(logmodfit)[[1]]$y_pred) # 4168
load('../output/list_prior.rda')
length(predat_prior$cperc) # 4154
predat_prior$cperc <- predat_prior$cperc * 100 + 50
predat_prior %>% mutate(percent_cat=cut(cperc, 
                         breaks=c(0, 20, 40, 60, 80, 100), 
                         labels=c("1-20","21-40","41-60", "61-80", "81-100")))



# visualize the pattern for each person: fewer than half
prop         <- tapply(newdat$resp, list(newdat$percent_cat, newdat$workerid, newdat$qq), mean, na.rm = T)
successes    <- tapply(newdat$resp, list(newdat$percent_cat, newdat$workerid, newdat$qq), sum, na.rm = T)
observations <- tapply(newdat$resp, list(newdat$percent_cat, newdat$workerid, newdat$qq), length)
matplot(percentages, prop[,,1]
        , pch = 19, col = qcols[1], type='l', lwd = 2
        , xlab = "Percent", ylab = "Proportion 'true' responses"
        , frame.plot = F)
abline(v = 50, lwd = 1.5, col = "darkgrey")
abline(h = .50, lwd = 1.5, col = "darkgrey")


# visualize the pattern for each person: more than half
matplot(percentages, prop[,,2]
        , pch = 19, col = qcols[2], type='l', lwd = 2
        , xlab = "Percent", ylab = "Proportion 'true' responses"
        , frame.plot = F)
abline(v = 50, lwd = 1.5, col = "darkgrey")
abline(h = .50, lwd = 1.5, col = "darkgrey")

