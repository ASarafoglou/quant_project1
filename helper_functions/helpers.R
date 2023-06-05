# compute model predictions
modelPredictions <- function(alpha, beta, gamma, cperc){
  
  mu <- (cperc - beta)/alpha
  mu[cperc <= beta] <- 0
  
  pi <-  gamma + (1 - 2 * gamma) * (1 - exp(- mu))
  
  return(pi)
  
}

# other helper functions
transparentColor <- function(color, percent = 50, name = NULL) {

  rgb.val <- grDevices::col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)

  invisible(t.col)
}

drawDistribution <- function(pid, quantifier = 'Fewer than half', parameter = 'alpha', 
                             draws=list_of_draws, correct = 0.90){
  
  colid           <- ifelse(quantifier == 'Fewer than half', 1, 2)
  
  not_hier <- grepl('alpha|beta|gamma', parameter)
  
  if(not_hier){
    
    samples         <- list_of_draws[[parameter]][, pid, colid]
    
  } else {
    
    samples         <- list_of_draws[[parameter]][, colid]
    
  }

  median_samples  <- papaja::printnum(median(samples))
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

removeDivergentTransitions <- function(logmodfit, nchains = 3){
  
  # remove divergent transitions for further analysis
  posterior2        <- rstan::extract(logmodfit, inc_warmup = FALSE, permuted = FALSE)
  sampler_params    <- rstan::get_sampler_params(logmodfit, inc_warmup = FALSE)
  samples_processed <- list()
  
  for(i in 1:nchains){
    
    is_divergent           <- as.logical(sampler_params[[i]][,'divergent__'])
    samples_processed[[i]] <- posterior2[!is_divergent, i, ]
    
  }
  
  # collapse them together
  samples_processed <- do.call(rbind, samples_processed)
  
  return(samples_processed)
  
}

checkModelConvergence <- function(list_of_draws, parameters, hyperparameters){
  
  # Rhat diagnostics
  params_of_interest <- c(parameters, hyperparameters)
  Rhat_params        <- NULL
  
  for(i in seq_along(params_of_interest)){
    
    list_index     <- which(params_of_interest[i] == names(list_of_draws))
    Rhat_params[i] <- rstan::Rhat(as.matrix(list_of_draws[[list_index]]))
    
  }
  
  reached_convergence_for_all_parameters <- all(Rhat_params <= 1.01)
  
  # correlation plot for all hyperparameters for each chain
  
  hyperparam_names <- NULL
  
  for(i in seq_along(hyperparameters)){
    
    new_names        <- dimnames(array_of_draws)$parameters[grepl(
      paste0(hyperparameters[i], '\\['), dimnames(array_of_draws)$parameters)]
    hyperparam_names <- c(hyperparam_names, new_names)
    
  }
  
  # one plot with hyperparameters per chain
  plot_list <- list()
  
  for(i in 1:nchains){
    M <- cor(array_of_draws[,i,hyperparam_names])
    corrplot::corrplot(M, method = 'color', order = 'alphabet')
    plot_list[[i]] <- recordPlot()
    
  }
  
  output <- list(Rhat_params = Rhat_params,
                 reached_convergence = reached_convergence_for_all_parameters,
                 correlation_plot = plot_list
  )
  
  return(output)
  
}

analyzePredictedResponses <- function(samples_processed, predat,
                            prior, modelname, hyperparameters) {
  
  if(prior) hyperparameters <- paste0(hyperparameters, 'prior')
  hyperparam_names <- NULL
  
  for(i in seq_along(hyperparameters)){
    
    new_names        <- dimnames(array_of_draws)$parameters[grepl(
      paste0(hyperparameters[i], '\\['), dimnames(array_of_draws)$parameters)]
    hyperparam_names <- c(hyperparam_names, new_names)
    
  }
  
  output <- list()
  
  if(prior){
    
    # Prior predictive checks for a random row in the predicted data
    
    pred_names     <- colnames(samples_processed)[grepl('y_pred', colnames(samples_processed))]
    pred_responses <- samples_processed[, pred_names]
    pred_responses_orig <- pred_responses
    colnames(pred_responses_orig) <- paste0('y_pred_', 1:length(pred_names))
    write.csv2(pred_responses_orig,
               paste0('../output/prior_pred_', modelname, time, '.csv'),
               row.names = FALSE)
    write.csv2(samples_processed[, hyperparam_names],
               paste0('../output/prior_pred_hyper_', modelname, time, '.csv'),
               row.names = FALSE)
    # random row
    random_row            <- sample(1:nrow(pred_responses), 1)
    predat$pred_responses <- pred_responses[random_row, ]
    prop_predict          <- tapply(predat$pred_responses, list(predat[['percent_cat']], predat[['sub']], predat[['qq']]), mean, na.rm = T)
    
    # 4. plot prior predictives
    plot_list <- list()
    
    for(i in 1:predat$I){
      
      plot(1:nlevels(predat$percent_cat), prop_predict[,i,1]
           , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
           , xlab = paste('Proportions - Subject', i)
           , ylab = "Proportion predicted responses (prior)"
           , main = "Fewer than half"
           , ylim = c(0, 1)
           , frame.plot = F
           , xaxt = 'n')
      axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
      abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
      abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
      plot_list[[length(plot_list) + 1]] <- recordPlot()
      
      plot(1:nlevels(predat$percent_cat), prop_predict[,i,2]
           , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
           , xlab = paste('Proportions - Subject', i) 
           , ylab = "Proportion predicted responses (prior)"
           , main = "More than half"
           , ylim = c(0, 1)
           , frame.plot = F
           , xaxt = 'n')
      axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
      abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
      abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
      plot_list[[length(plot_list) + 1]] <- recordPlot()
    }
    
    # aggregated over all
    pred_responses <- apply(pred_responses, 2, mean)
    prop_predict   <- tapply(predat$pred_responses, list(predat[[percent_cat]], predat[['qq']]), mean, na.rm = T)
    
    # 1. monotonic increase
    inc_fewer <- !is.unsorted(prop_predict[,1])
    inc_more  <- !is.unsorted(prop_predict[,2])
    obey1     <- all(c(inc_fewer, inc_more) == TRUE)
    
    # 2. small response error
    first20percent <- prop_predict[1,] <= 0.2
    last20percent  <- prop_predict[nlevels(predat$percent_cat),] >= 0.8
    obey2          <- all(c(first20percent, last20percent) == TRUE)
    
    # 3. constrained vagueness
    threshold    <- which(levels(predat$percent_cat) == '41-60')
    atthreshold  <- 0.40 < prop_predict[threshold,] & prop_predict[threshold,] < 0.60
    obey3        <- all(atthreshold == TRUE)
    
    plot(1:nlevels(predat$percent_cat), prop_predict[,1]
         , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
         , xlab = 'Proportions - Aggregated'
         , ylab = "Proportion predicted responses (prior)"
         , main = "Fewer than half"
         , ylim = c(0, 1)
         , frame.plot = F
         , xaxt = 'n')
    axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
    abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
    abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
    plot_list[[length(plot_list) + 1]] <- recordPlot()
    
    plot(1:nlevels(predat$percent_cat), prop_predict[,2]
         , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
         , xlab = 'Proportions - Aggregated'
         , ylab = "Proportion predicted responses (prior)"
         , main = "More than half"
         , ylim = c(0, 1)
         , frame.plot = F
         , xaxt = 'n')
    axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
    abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
    abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
    plot_list[[length(plot_list) + 1]] <- recordPlot()
    
    output[['monotonic_increase']]    <- obey1
    output[['small_response_error']]  <- obey2
    output[['constrained_vagueness']] <- obey3
    
  } else {
    
    # Posterior predictive checks
    write.csv2(samples_processed[, hyperparam_names],
               paste0('../output/post_pred_hyper_', modelname, '.csv'),
               row.names = FALSE)
    pred_names <- colnames(samples_processed)[grepl('y_postpred', colnames(samples_processed))]
    pred_responses        <- samples_processed[, pred_names]
    predat$pred_responses <- apply(pred_responses, 2, mean)
    # # recode quantifiers
    # predat$y[predat$qq == 1] <- abs((predat$y[predat$qq == 1]) - 1)
    
    # plot posterior predictives
    prop         <- tapply(predat$y, list(predat$percent_cat, predat$sub, predat$qq), mean, na.rm = T)
    prop_predict <- tapply(predat$pred_responses, list(predat[[percent_cat]], predat[[sub]], predat[['qq']]), mean, na.rm = T)
    
    plot_list <- list()
    
    for(i in 1:predat$I){
      
      plot(1:nlevels(predat$percent_cat), prop[,i,1]
           , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
           , xlab = paste('Proportions - Subject', i)
           , ylab = "Proportion true responses"
           , main = "Fewer than half"
           , ylim = c(0, 1)
           , frame.plot = F
           , xaxt = 'n')
      axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
      lines(1:nlevels(predat$percent_cat), prop_predict[,i,1])
      abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
      abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
      plot_list[[length(plot_list) + 1]] <- recordPlot()
      
      plot(1:nlevels(predat$percent_cat), prop[,i,2]
           , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
           , xlab = paste('Proportions - Subject', i) 
           , ylab = "Proportion true responses"
           , main = "More than half"
           , ylim = c(0, 1)
           , frame.plot = F
           , xaxt = 'n')
      axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
      lines(1:nlevels(predat$percent_cat), prop_predict[,i,2])
      abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
      abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
      plot_list[[length(plot_list) + 1]] <- recordPlot()
    }
    
  }
  

  # plot(1:nlevels(predat$percent_cat), prop_predict[,1]
  #      , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
  #      , xlab = 'Proportions - Aggregated'
  #      , ylab = "Proportion predicted responses (prior)"
  #      , main = "Fewer than half"
  #      , ylim = c(0, 1)
  #      , frame.plot = F
  #      , xaxt = 'n')
  # axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
  # abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
  # abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
  # if(!prior)  lines(1:nlevels(predat$percent_cat), prop[,i,1])
  # plot_list[[length(plot_list) + 1]] <- recordPlot()
  # 
  # 
  # plot(1:nlevels(predat$percent_cat), prop_predict[,2]
  #      , pch = 19, col = 'orange', type='l', lwd = 2, las = 1
  #      , xlab = 'Proportions - Aggregated'
  #      , ylab = "Proportion predicted responses (prior)"
  #      , main = "More than half"
  #      , ylim = c(0, 1)
  #      , frame.plot = F
  #      , xaxt = 'n')
  # axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
  # abline(v = 3, lwd = 1.5, col = "darkgrey", lty = 3)
  # abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
  # if(!prior)  lines(1:nlevels(predat$percent_cat), prop[,i,2])
  # plot_list[[length(plot_list) + 1]] <- recordPlot()
  
  output[['pred_plot']] = plot_list
  
  return(output)
  
}

analyzePredictedResponsesR <- function(y_pred, predat, individual = FALSE, testtime = NULL) {
  
  if(predat$D > 1) {
    lineCols <- RColorBrewer::brewer.pal(predat$D, 'Dark2')
    } else {
      lineCols <- 'black'
    }
  
  
  if(is.null(testtime)) {
    
    percent_cat <- 'percent_cat'
    sub         <- 'sub'
    
  }
  
  if(testtime == 't1') {
    
    percent_cat <- 'percent_cat_t1'
    sub         <- 'sub_t1'
    
  } else if(testtime == 't2') {
    
    percent_cat <- 'percent_cat_t2'
    sub         <- 'sub_t2'
    
  } 
    
  
  plot_list <- list()
  
  if(individual) {
    
    for(d in 1:predat$D){

      plot.new()
      
      for(i in 1:predat$I){
        
        if(!is.null(dim(y_pred))) {
          predat$pred_responses <- apply(y_pred, 1, mean)
        } else {
          predat$pred_responses <- y_pred
        }
        prop_predict <- tapply(predat$pred_responses, list(predat[[percent_cat]], predat[[sub]], predat[['qq']]), mean, na.rm = T)
        
        if(i == 1){
          
          plot(1:nlevels(predat[[percent_cat]]), prop_predict[,1,d]
               , pch = 19, type='l', col = lineCols[d], lwd = 1, las = 1
               , xlab = 'Proportions'
               , ylab = "Proportion predicted responses (prior)"
               , main = paste(predat$quant_name[d], testtime)
               , ylim = c(0, 1)
               , frame.plot = F
               , xaxt = 'n')
          axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
          abline(v = 5.5, lwd = 1.5, col = "darkgrey", lty = 3)
          abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
          
        } else {
          
          lines(1:nlevels(predat[[percent_cat]]), prop_predict[,i,d], col = lineCols[d])
          
        }
      }
      
      plot_list[[length(plot_list) + 1]] <- recordPlot()
      
    }
    
  } else {
    
    plot.new()
    
    if(!is.null(dim(y_pred))) {
      predat$pred_responses <- apply(y_pred, 1, mean)
    } else {
      predat$pred_responses <- y_pred
    }
    prop_predict   <- tapply(predat$pred_responses, list(predat[[percent_cat]], predat[['qq']]), mean, na.rm = T)
    
    plot(1:nlevels(predat[[percent_cat]]), prop_predict[,1]
         , pch = 19, col = lineCols[1], type='l', las = 1, lwd =2
         , xlab = 'Proportions'
         , ylab = "Proportion predicted responses (prior)"
         , main = paste("Aggregated", testtime)
         , ylim = c(0, 1)
         , frame.plot = F
         , xaxt = 'n')
    for(d in 2:predat$D){
      lines(1:nlevels(predat[[percent_cat]]), prop_predict[,d], col = lineCols[d], lwd = 2)
    }
    axis(1, at = 1: length(rownames(prop_predict)), labels = rownames(prop_predict))
    abline(v = 5.5 , lwd = 1.5, col = "darkgrey", lty = 3)
    abline(h = .5, lwd = 1.5, col = "darkgrey", lty = 3)
    legend('topright', legend=predat$quant_name,
           col=lineCols, lty = 1, cex=0.8)
    
    plot_list[[length(plot_list) + 1]] <- recordPlot()
    
  }
  
  return(plot_list)
  
}

# function for truncated normals
my_rnorm <- function(nsamp = 1, mu, sigma, lower, upper){
  
  est <- rnorm(nsamp, mu, sigma)
  
  for(i in 1:nsamp){
    
    while(est[i] < lower | est[i] > upper){
      est[i] <- rnorm(1, mu, sigma)
      
    }
    
  }
  return(est)
}
my_rmvnorm <- function(nsamp = 1, Mu, Sigma, lower, upper){
  
  est <- mvtnorm::rmvnorm(nsamp, Mu, Sigma)
  
  for(i in 1:nsamp){
    
    while(any(est[i, ] < lower) | any(est[i, ] > upper)){
      est[i, ] <- mvtnorm::rmvnorm(1, Mu, Sigma)
      
    }
    
  }
  return(est)
}
makeS <- function(rho, sigma){
  
  R  <- matrix(c(1, rho, rho, 1), 2, 2)
  V  <- diag(sigma, 2, 2)
  S  <- V %*% R %*% V
  
  return(S)
  
}

