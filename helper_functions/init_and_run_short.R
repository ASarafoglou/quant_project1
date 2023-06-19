# identify which trial concerns which quantifier
# 1="All", 2="Few", 3="Fewer than half", 4="Many", 
# 5="More than half", 6="Most", 7="None"; 8="Some"

getDat <- function(dat_t1, dat_t2 = NULL, prior = FALSE){

  if(is.null(dat_t2)){
    
    D     <- length(unique(dat_t1$quant))
    N     <- nrow(dat_t1)
    I     <- length(unique(dat_t1$workerid))
    y     <- dat_t1$resp
    cperc <- (dat_t1$percent - 50) / 100
    sub   <- as.numeric(factor(dat_t1$workerid))
    quant_name <- unique(dat_t1$quant)
    
    standat <- list(D = D, N = N, I = I, quant_name = quant_name,
                    cperc = cperc, sub = sub) 
    
    if(!prior) standat[['y']] <- y
    
  } else {
    
    D     <- length(unique(dat_t1$qq))
    I     <- length(unique(dat_t1$workerid))
    Time  <- 2
    quant_name <- unique(dat_t1$qq)
    
    N_t1   <- nrow(dat_t1)
    N_t2   <- nrow(dat_t2)
    y_t1   <- dat_t1$resp
    y_t2   <- dat_t1$resp
    sub_t1 <- as.numeric(factor(dat_t1$workerid))
    sub_t2 <- as.numeric(factor(dat_t2$workerid))
    cperc_t1 <- (dat_t1$percent - 50) / 100
    cperc_t2 <- (dat_t2$percent - 50) / 100
    
    standat <- list(D = D, I = I, T=Time, quant_name = quant_name,
                    cperc_t1 = cperc_t1, cperc_t2 = cperc_t2,
                    N_t1 = N_t1, N_t2 = N_t2, 
                    sub_t1 = sub_t1, sub_t2 = sub_t2) 
    
    if(!prior) {
      standat[['y_t1']] <- y_t1
      standat[['y_t2']] <- y_t2
    }
    
  }

 
  return(standat)
}

myRunner <- function(standat, iter = 1000, warmup = 400, 
                     mod, nchains = 4, ncores = 2, addparams = NULL,...){
  
  fit <- rstan::sampling(mod, verbose=T,
                  data   = standat, 
                  iter   = iter,
                  warmup = warmup,
                  chains = nchains,
                  cores  = ncores,
                  pars   = 
                  , ...)
  return(fit)}
