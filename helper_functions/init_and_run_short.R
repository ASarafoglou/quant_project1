# identify which trial concerns which quantifier
# 1="All", 2="Few", 3="Fewer than half", 4="Many", 
# 5="More than half", 6="Most", 7="None"; 8="Some"

getDat <- function(dat_t1, dat_t2 = NULL, prior = FALSE){

  if(is.null(dat_t2)){
    
    D     <- length(levels(dat$qq))
    N     <- nrow(dat)
    I     <- length(unique(dat$workerid))
    Time  <- 1
    y     <- dat$resp
    cperc <- (dat$percent - 50) / 100
    sub   <- as.numeric(factor(dat$workerid))
    quant_name <- levels(dat$qq)
    
    fewer <- ifelse(grepl('Fewer', dat$qq), 1, 0)
    more  <- ifelse(grepl('More', dat$qq), 1, 0)
    many  <- ifelse(grepl('Many', dat$qq), 1, 0)
    most  <- ifelse(grepl('Most', dat$qq), 1, 0)
    few   <- ifelse(grepl('Few$', dat$qq), 1, 0)
    
    standat <- list(D = D, N = N, I = I, T=Time, quant_name = quant_name,
                    cperc = cperc, sub = sub,
                    fewer = fewer, more = more,
                    many = many, most = most,
                    few = few) 
    
    if(!prior) standat[['y']] <- y
    
  } else {
    
    D     <- length(levels(dat_t1$qq))
    I     <- length(unique(dat_t1$workerid))
    Time  <- 2
    quant_name <- levels(dat_t1$qq)
    
    N_t1   <- nrow(dat_t1)
    N_t2   <- nrow(dat_t2)
    y_t1   <- dat_t1$resp
    y_t2   <- dat_t1$resp
    sub_t1 <- as.numeric(factor(dat_t1$workerid))
    sub_t2 <- as.numeric(factor(dat_t2$workerid))
    cperc_t1 <- (dat_t1$percent - 50) / 100
    cperc_t2 <- (dat_t2$percent - 50) / 100
    
    fewer_t1 <- ifelse(grepl('Fewer', dat_t1$qq), 1, 0)
    fewer_t2 <- ifelse(grepl('Fewer', dat_t2$qq), 1, 0)
    more_t1  <- ifelse(grepl('More', dat_t1$qq), 1, 0)
    more_t2  <- ifelse(grepl('More', dat_t2$qq), 1, 0)
    many_t1  <- ifelse(grepl('Many', dat_t1$qq), 1, 0)
    many_t2  <- ifelse(grepl('Many', dat_t2$qq), 1, 0)
    most_t1  <- ifelse(grepl('Most', dat_t1$qq), 1, 0)
    most_t2  <- ifelse(grepl('Most', dat_t2$qq), 1, 0)
    few_t1   <- ifelse(grepl('Few$', dat_t1$qq), 1, 0)
    few_t2   <- ifelse(grepl('Few$', dat_t2$qq), 1, 0)
    
    standat <- list(D = D, I = I, T=Time, quant_name = quant_name,
                    cperc_t1 = cperc_t1, cperc_t2 = cperc_t2,
                    N_t1 = N_t1, N_t2 = N_t2, 
                    sub_t1 = sub_t1, sub_t2 = sub_t2,
                    fewer_t1 = fewer_t1, fewer_t2 = fewer_t2,
                    more_t1 = more_t1, more_t2 = more_t2,
                    many_t1 = many_t1, many_t2 = many_t2,
                    most_t1 = most_t1, most_t2 = most_t2,
                    few_t1 = few_t1, few_t2 = few_t2) 
    
    if(!prior) {
      standat[['y_t1']] <- y_t1
      standat[['y_t2']] <- y_t2
    }
    
  }

 
  return(standat)
}

myRunner <- function(standat, iter = 1000, warmup = 400, 
                     mod, nchains = 4,prior = FALSE, addparams = NULL,...){

  if(prior) {
    
    parameters_to_monitor <- c("delta", "nu", "beta", "alpha", "gamma", 
                               "sigma", "sigmaalpha", "eta", "mug", "y_postpred",
                               addparams)
    
  } else {
    
    parameters_to_monitor <- c("deltaprior", "nuprior", "betaprior", "alphaprior", "gammaprior", 
                               "sigmaprior", "sigmaalphaprior", "y_pred", addparams)
    
  }
  
  fit <- sampling(mod, verbose=T,
                  data   = standat, 
                  iter   = iter,
                  warmup = warmup,
                  chains = nchains,
                  cores  = nchains,
                  pars   = 
                  , ...)
  return(fit)}