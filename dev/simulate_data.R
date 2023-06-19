library(stringr)
setwd("~/GitProjects/quant_project1/dev")
dat <- read.table("../data/exp1-preprocessed-all.csv", header = TRUE, sep = ',')
# dat1 <- read.table("../data/exp1-preprocessed-all.csv", header = TRUE, sep = ',')
# dat2 <- dat1
# dat2$workerid <- factor(dat2$workerid)
# levels(dat2$workerid) <- paste0(LETTERS, 1:100)[1:length(unique(dat2$workerid))]
# dat <- rbind(dat1, dat2)

# ndatasets      <- 1
# ndat_simulated <- length(list.files('../data/')[grepl('simulated-all-large', list.files('../data/'))])
ndat_to_simulate <- 2

seeds       <- 2022 + 1:ndat_to_simulate
nsubjs      <- 20  # 20 participants
ntrials     <- 2   # 200 data points per participant; same trials as sonia
percentages <- c(5, 20, 40, 60, 80, 95)
#percentages <- seq(0, 100, length.out = 11)
unique_quants <- unique(dat$qq)
unique_quants <- str_to_title(unique_quants)
unique_quants <- str_replace_all(unique_quants, " ", "")
# D <- length(unique_quants)
D <- 1 # "Most"

for(j in 1:ndat_to_simulate){
  
  set.seed(seeds[j])
  
  # inits
  newsubjs <- sample(unique(dat$workerid), nsubjs)
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
  newdat <- newdat %>% dplyr::mutate(percent_cat=cut(percent, 
                                              breaks=c(0, 20, 40, 60, 80, 100), 
                                              labels=c("1-20","21-40","41-60", "61-80", "81-100")))
  
  
  write.table(newdat, 
              paste0("../data/simulated-all-data-", j,".csv"), 
              row.names = FALSE, quote = FALSE, sep = ',')
  
}
