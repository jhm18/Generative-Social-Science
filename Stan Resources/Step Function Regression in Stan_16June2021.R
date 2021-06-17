# Step Function Regression in Stan: https://www.jchau.org/2021/06/16/step-function-regression-in-stan/
# Joris Chau
# 16 June 2021

# Clear Out Console Script
  cat("\014")

# Setting Work Directory
  setwd("~/Dropbox/My Mac (Jonathan’s MacBook Pro)/Desktop")
  getwd()

# Options
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())
  set.seed(1)
  
################
#   PACKAGES   #
################
library(dplyr)        #Data Management
library(cmdstanr)     #Used to run cmdstan from R

# Checking that I am pointing to cmdstan
  set_cmdstan_path("/Users/jonathan.h.morgan/cmdstan")
  cmdstan_path()
  cmdstan_version()
  
#################
#   FUNCTIONS   #
#################

# Helper function to calculate scaling (average) or wavelet (difference) coefficients
  filt <- function(C, fun) fun(C[c(T, F)], C[c(F, T)]) / sqrt(2)
  
# Helper function to reconstruct scaling coefficients at finer scale
  inv_filt <- function(C, D) c(t(cbind((C + D) / sqrt(2), (C - D) / sqrt(2))))
  
###############################
#   CREATING SYNTHETIC DATA   #
###############################

# Setting Data Parameters
  K <- 3         # nr. breakpoints
  N <- 128       # nr. observations
  mu <- 0:K      # local means
  sigma <- 0.2   # error sd
  
# Generating Data
  f <- rep(mu, each = N / (K + 1))
  x <- (1:N) / N
  y <- rnorm(N, mean = f, sd = sigma) 
  
# Quickly Visualizing
  x_axis <- pretty(x)
  y_axis <- pretty(y)
  
  plot(0, type='n', xlab=' ', ylab=' ', xlim=c(0, 1), ylim=c(min(y_axis), max(y_axis)) ,cex.axis=1.3, family='HersheySerif', las=1, main=' ', bty='n')
  grid(lwd = 2)

  mtext(side = 1, text = 'Time', col = "black", line = 3, cex = 1.5, family='HersheySerif')
  mtext(side = 2, text = 'Response', col = "black", line = 2.5, cex = 1.5, family='HersheySerif')
  
  title('K = 3 Breakpoints at Regular Intervals', family='serif', cex.main=2, line=1.25)
  
  points(x, y, pch=21, bg='grey', cex=1)
  
  lines(x, f, lty=2, lwd=2)
  
##############################################
#   PERFORMING HAAR WAVELET TRANSFORMATION   #
##############################################

# NOTE: We need to regularize the model because modeling each breakpoints results in an over parameterized model.
#       We do this by applying a Haar Wavelet transformation to our response data.
  
# Creating two lists to store scaling and wavelet coefficients from course to fine scales
  C <- D <- vector(mode = "list", length = log2(N))
  
# Recursively update course scale coefficients
  for(l in log2(N):1) {
    C[[l]] <- filt(C = if(l < log2(N)) C[[l + 1]] else f, fun = `+`)
    D[[l]] <- filt(C = if(l < log2(N)) C[[l + 1]] else f, fun = `-`)
  }
  
# The list with scaling coefficients C consists of scaled local averages at increasingly coarse resolution scales, 
# with the scaling coefficient at the coarsest scale (i.e. C[[1]]) being equivalent to the global mean scaled by a known factor:
  all.equal(C[[1]], 2^(log2(N)/2) * mean(f))

# Recursively reconstruct fine scale coefficients
  f1 <- C[[1]]
  for(l in 1:log2(N)) {
    f1 <- inv_filt(C = f1, D = D[[l]])  
  }
  
# Confirming that we were successful
  all.equal(f, f1)
  
######################################
#   COMPILING & RUNNING STAN MODEL   #
######################################
  
# Compiling Model 
  step2_model <- cmdstan_model("SparseLinear_Reg.stan", include_paths = "/Users/jonathan.h.morgan/Desktop/")
  
# Draw Samples
  step2_fit <- step2_model$sample(
    data = list(N = N, y = y, m0 = 0.05),
    chains = 4,
    iter_sampling = 1000,
    iter_warmup = 1000
  )
  
# Checking Sampling Results
  step2_fit
  
# sampling diagnostics
  step2_fit$cmdstan_diagnose()
  
# Creating Fit Objects
  sfit <-  step2_fit$output_files() %>%
    rstan::read_stan_csv()
  
# Creating Summary Object
  rstan_summary <- rstan::summary(sfit)
  rstan_summary <- rstan_summary$summary
  rstan_summary <- as.data.frame(rstan_summary)
  
###########################
#   VISUALIZING RESULTS   #
###########################
  
# Create Plot Elements
  f_pred <- as.data.frame(sfit, pars = "f")
  
  f_medians <- vector('numeric', ncol(f_pred))
  f_95 <- vector('numeric', ncol(f_pred))
  f_05 <- vector('numeric', ncol(f_pred))
  for (i in seq_along(f_medians)){
    f_medians[[i]] <- median(f_pred[[i]])
    f_05[[i]] <- as.numeric(quantile(f_pred[[i]], probs=c(0.05)))
    f_95[[i]] <- as.numeric(quantile(f_pred[[i]], probs=c(0.95)))
  }
  
# Creating Base Plot
  x_axis <- pretty(x)
  y_axis <- pretty(y)
  
  plot(0, type='n', xlab=' ', ylab=' ', xlim=c(0, 1), ylim=c(min(y_axis), max(y_axis)) ,cex.axis=1.3, family='HersheySerif', las=1, main=' ', bty='n')
  grid(lwd = 2)
  
# Adding Labels
  mtext(side = 1, text = 'Time', col = "black", line = 3, cex = 1.5, family='HersheySerif')
  mtext(side = 2, text = 'Response', col = "black", line = 2.5, cex = 1.5, family='HersheySerif')
  
# Adding Title
  title('K = 3 Breakpoints at Regular Intervals', family='serif', cex.main=2, line=1.25)
  
# Adding Points
  points(x, y, pch=21, bg='grey', cex=1)
  
# Adding Credible Interval
  polygon(c(x,rev(x)), c(f_95,rev(f_05)) ,col="cornsilk", border = NA)
  
# Adding Posterior Median
  lines(x, f_medians, lty=2, lwd=2, col='blue')
  