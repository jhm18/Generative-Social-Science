# Periodic Modeling
# Developed by: Martin Modrak (https://discourse.mc-stan.org/t/mathy-folks-thoughts-on-the-asymptotic-representation-of-this-k-hmm-model-for-periodic-signals/22038/24)
# 25 April 2021

# Clear Out Console Script
  cat("\014")

# Setting Work Directory
  setwd("~/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop")
  getwd()
  
# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())
  options(mc.cores = 4)
  
################
#   PACKAGES   #
################

library(cmdstanr)     #Used to run cmdstan from R
library(tidyverse)

#################
#   FUNCTIONS   #
#################
  
# Plot Utilities
  source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_2Oct2020.R")
  source("~/R Resources/R Plotting Utilities & Resources/util.R")
  
# Checking that I am pointing to cmdstan
  set_cmdstan_path("/Users/jonathan.h.morgan/cmdstan")
  cmdstan_path()
  cmdstan_version()
  
###############################
#   CREATING SYNTHETIC DATA   #
###############################
  
# Parameters
  n <- 30
  frequency <- 13
  phase <- 0.3
  noise <- 0.5
  amplitude <- 1.4
  
# Constructing Data
  x <- runif(n, max = 2 * pi)
  f <- amplitude * sin(frequency * x + phase)
  y <- rnorm(n, mean = f, sd = noise)
  x_fixed = c(pi / 4, 7/4 * pi)
  
# Creating Input List
  data <- list(
    n = n,                                      
    y = y,                                         
    x = x,            
    x_fixed = x_fixed,                               
    max_k = 10                                    
  )
  
###################################################
#   ESTIMATING STAN MODEL & SAVING DATA OBJECTS   #
###################################################

# Setting Seed
  set.seed(10271998) 
  
# Setting Temporary Directory
  temp <- tempdir()
  
# Compiling Model
  mod <- cmdstan_model("~/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop/periodic.stan")
  
# Checking Model
  mod$print()
  
# Sampling: Non-Centered Random Effects
  fit <- mod$sample(
    data = data,
    output_dir = temp,
    seed = 123,
    chains = 4,
    thin = 1,
    parallel_chains = 4,
    iter_warmup = 2000,
    iter_sampling = 2000,
    adapt_delta = 0.99,
    max_treedepth = 15,
    refresh = 500
  )
  
# Checking that the Model Passes: E-BFMI, Treedepth, Sample Size, & R-Hat Cursory Checks
  fit$cmdstan_diagnose()
  
################
#   PLOTTING   #
################

# Creating Data Objects
  fit$summary(c("noise", "z", "freq_chosen", "phase_chosen", "amplitude", "index"))
  
  bayesplot::mcmc_pairs(fit$draws(), pars = c("z[1]","z[2]", "amplitude", "noise"))
  
# Pulling Out Draws for Frequency, Phase, and Amplitude
  draws <- as.data.frame(posterior::as_draws_matrix(fit$draws(c("freq_chosen", "phase_chosen", "amplitude"))))
  
# Checking the frequency
  hist(draws$freq_chosen)
  
# Constructing True Model
  x_adj <- seq(0, 2 * pi, length.out = 101)
  y_adj <- amplitude * sin(frequency * x_adj + phase)
  true <- as.data.frame(cbind(x_adj,y_adj))
  colnames(true) <- c('x', 'y')
  rm(x_adj,y_adj)
  
# Observed Data
  observed <- tibble(x = x, y = y)
  
# Constructing Draws
  z <-  tibble(x = seq(0, 2 * pi, length.out = 100)) %>% crossing(as_tibble(draws) %>% mutate(id = 1:dim(draws)[1])) %>% 
    mutate(y = amplitude * sin(x * freq_chosen + phase_chosen)) %>%
    filter(id %in% sample(unique(id), 100))
  
rm(amplitude,f,frequency,n,noise, phase, temp,x,x_fixed,y)

# Plotting
  x11(width=10.6806, height=7.30556)
  periodic_visual <- function() {
    x_values <- pretty(z$x)
    y_values <- pretty(z$y)
    x_fixed = c(pi / 4, 7/4 * pi)
    
    plot(0, type='n', xlab=' ', ylab=' ', xlim=c(min(x_values), max(x_values)), ylim=c(min(y_values), max(y_values)) ,cex.axis=1.3, family='HersheySerif', las=1, main=' ', bty='n')
    grid(lwd = 2)
  
    ids <- sort(z$id)
    for(i in seq_along(z$id)){
      iteration <- z[(z$id == ids[[i]]), ]
      lines(iteration$x, iteration$y, col="black")
    }
  
    lines(true, col="cyan")
  
    abline(v=x_fixed[[1]],col='brown',lwd=2, lty=1)
    abline(v=x_fixed[[2]],col='brown',lwd=2, lty=1)
    
    points(observed, pch=21, bg="grey", col="blue")
  
    mtext(side = 1, text = 'x', col = "black", line = 2.5, cex = 2, family='HersheySerif')
    mtext(side = 2, text = 'y', col = "black", line = 2.5, cex = 2, family='HersheySerif')
  }
  
  g <- cowplot::as_grob(periodic_visual)
  p_1 <- cowplot::ggdraw(g)
  
  p_1
  
  
#############
#   NOTES   #
#############
  
# Parameters where is not fully contrained
  set.seed(24855)
  n <- 8
  frequency <- 4
  phase <- -0.5
  noise <- 0.5
  amplitude <- 1.4

  
# Constructing Data
  x <- runif(n, max = 2 * pi)
  f <- amplitude * sin(frequency * x + phase)
  y <- rnorm(n, mean = f, sd = noise)
  x_fixed = c(pi / 4, 7/4 * pi)
  
# Creating Input List
  data <- list(
    n = n,                                      
    y = y,                                         
    x = x,            
    x_fixed = x_fixed,                               
    max_k = 10                                    
  )
  
  
  
  

  