library(stringr)
dat <- read.table("../data/exp1-preprocessed-all.csv", header = TRUE, sep = ',')

ndatasets      <- 2
ndat_simulated <- length(list.files('../data/')[grepl('simulated-all', list.files('../data/'))])

ndat_to_simulate <- ndatasets - ndat_simulated

seeds       <- 2022 + 1:ndat_to_simulate
nsubjs      <- 8 # 8 participants
ntrials     <- 3 # 300 data points
percentages <- c(5, 20, 40, 60, 80, 95)
unique_quants <- unique(dat$qq)
unique_quants <- str_to_title(unique_quants)
unique_quants <- str_replace_all(unique_quants, " ", "")
D <- length(unique_quants)

for(j in 1:ndat_to_simulate){
  
  set.seed(seeds[j])
  
  # inits
  newsubjs <- sample(unique(dat$workerid), nsubjs, replace = FALSE)
  newdat   <- subset(dat, workerid %in% newsubjs) 
  
  # visualize the pattern for each person: fewer than half
  successes    <- tapply(newdat$resp, list(newdat$percent_cat, newdat$workerid, newdat$qq), sum, na.rm = T)
  observations <- tapply(newdat$resp, list(newdat$percent_cat, newdat$workerid, newdat$qq), length)
  
  # put probabilities into a logit link for each participant
  # simulate from binomial
  quants <- list()
  
  for(d in 1:D){
    quants[[d]] <- data.frame(percent     = rep(rep(1:100, each = ntrials), nsubjs),
                              qq          = factor(unique_quants[d]),
                              workerid    = rep(1:nsubjs, each = ntrials*100),
                              theta       = NA,
                              resp        = NA)
  }

  newx <- 1:100
  
  for(i in 1:nsubjs){
    
    for(d in 1:D){
      
      glm_dat <- data.frame(
        x = percentages,
        y = successes[,,d][,i], 
        n = observations[,,d][,i])
      
    res  <- glm(cbind(y, n - y) ~ x, binomial, glm_dat)
    
    quants[[d]][quants[[d]]$workerid == i, 'theta'] <- rep(predict(res, data.frame(x = newx), type = "response"), each = ntrials)
    quants[[d]][quants[[d]]$workerid == i, 'resp']  <- sapply(quants[[d]][quants[[d]]$workerid == i, 'theta'], function(x) rbinom(1, 1, x))
      
    }

  }
  
  newdat <- do.call("rbind", quants)
  newdat <- newdat %>% mutate(percent_cat=cut(percent, 
                                              breaks=c(0, 20, 40, 60, 80, 100), 
                                              labels=c("1-20","21-40","41-60", "61-80", "81-100")))
  
  
  write.table(newdat, 
              paste0("../data/simulated-all-data-", ndat_simulated + j,".csv"), 
              row.names = FALSE, quote = FALSE, sep = ',')
  
}
