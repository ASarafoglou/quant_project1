rm(list=ls())
setwd("~/GitProjects/MostQuantifiersHaveManyMeanings/dev")
library("dplyr")
library("MCMCpack")
library("LaplacesDemon")
library("rstan")
library("plotrix")
library(RColorBrewer)
# data preprocessing
library(papaja)

######################
## load predictions ##
######################

load("out_prior_predictions.rda")
dat <- read.csv("data/exp1-preprocessed.csv")

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

predResponse <- function(datid, quantifier = 'Fewer than half', dat0=dat, draws=list_of_draws){
  # identify which rows in the data correspond to this participant
  # identify which percentages were shown
  # extract relevant prior predictive data
  worker  <- unique(dat0$workerid)[datid]
  person  <- dat0$workerid == worker & dat0$quant == quantifier
  percent <- dat0[person, 'percent']
  pred    <- colMeans(draws$y_pred[, person])
  
  return(plot(percent, pred,
              las = 1,
              ylim = c(0.40, 0.60),
              xlab = 'Percent',
              ylab = 'p',
              main = paste(quantifier, worker)))
}

list_of_draws <- rstan::extract(logmodfit)
array_of_draws <- as.array(logmodfit)

#######################
## check convergence ##
#######################

hist(summary(logmodfit)$summary[,"Rhat"]
     , breaks = 100
     , main = ""
     , xlab =  "Rhat")
quantile(summary(logmodfit)$summary[,"Rhat"], c(0.025, 0.5, 0.975))
Rhat(summary(logmodfit))

# Both bulk-ESS and tail-ESS should be at least 100 
#(approximately) per Markov Chain in order to be reliable 
# and indicate that estimates of respective posterior quantiles are reliable.
hist(summary(logmodfit)$summary[,"n_eff"]
     , breaks = 100
     , main = ""
     , xlab =  "Number of effective samples")
quantile(summary(logmodfit)$summary[,"n_eff"], c(0.025, 0.5, 0.975))

print(dimnames(array_of_draws))
sampler_params <- rstan::get_sampler_params(logmodfit, inc_warmup = FALSE)
sampler_params_chain1 <- sampler_params[[1]]
colnames(sampler_params_chain1)
code <- get_stancode(logmodfit)
cat(code)

# the initial values are not the problem
inits <- get_inits(logmodfit)
inits_chain1 <- inits[[1]]
inits_chain2 <- inits[[2]]
print(inits_chain1$betaprior)
print(inits_chain2$betaprior)
# or maybe they are?
inits_chain1[c('sigma2', 'sigma2alpha', 'sigma2prior', 'sigma2alphaprior')]
inits_chain2[c('sigma2', 'sigma2alpha', 'sigma2prior', 'sigma2alphaprior')]

inits_chain1[c('alphaprior', 'betaprior')]
inits_chain2[c('alphaprior', 'betaprior')]

print(get_elapsed_time(logmodfit))

########################
## inspect parameters ##
########################

N <- 20

# p_pred (probability to give a 'true' response)
# mu_pred (parameter for logisitc link)
# y_pred (0 & 1 responses)
print(names(list_of_draws))

## check alpha
  dim(list_of_draws$alpha)
  round(colMeans(list_of_draws[['alpha']][, , 1:2]), 4)
  round(colMeans(list_of_draws$alphaprior[, , 1:2]), 4)
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
  
  par(mfrow = c(3, 2))
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'alphaprior')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'alphaprior')
  }
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'betaprior')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'betaprior')
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
  
  par(mfrow = c(3, 2))
  for(i in 1:N){
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'beta')
    drawDistribution(i, quantifier = 'Fewer than half', parameter = 'betaprior')
  }
  for(i in 1:N){
    drawDistribution(i, quantifier = 'More than half', parameter = 'beta')
    drawDistribution(i, quantifier = 'More than half', parameter = 'betaprior')
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
  apply(list_of_draws$nuprior, 2, median)

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

N <- 20
par(mfrow = c(2, 3))
for(i in 1:N){
  predResponse(i, quantifier = 'Fewer than half')
}
for(i in 1:N){
  predResponse(i, quantifier = 'More than half')
}

## predicted p
# To-do: extract pi from the samples
quantifier <- 'Fewer than half'
# quantifier <- 'More than half'

for(i in 1:20){
  person   <- dat$workerid == worker & dat$quant == quantifier
  median_p <- apply(list_of_draws$p_pred, 2, median)
  worker   <- unique(dat$workerid)[i]
  pred_p   <- median_p[person]
  percent  <- dat[person, 'percent']
  plot(percent, pred_p,
       las = 1,
       ylim = c(0.48, 0.52),
       xlab = 'Percent',
       ylab = 'p_pred',
       main = paste(quantifier, worker))
}

