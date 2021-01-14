#R Stan Trials
#Jonathan H. Morgan
#29 September 2020

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Options
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())

set.seed(10271998) ## not required but assures repeatable results

#Setting Directory
setwd("/Users/jonathan.h.morgan/Desktop")
getwd()

################
#   PACKAGES   #
################

#Note: Using cmdstanr because I already have cmdstan installed.

library(cmdstanr)       #Used to run cmdstan from R
library(posterior)      #Used to sample posterior distribution
library(bayesplot)      #Used to generate Bayesian diagnostic plots
library(qcc)            #Used to generate control charts for Mean, Standard Deviation, and Range             


color_scheme_set("brightblue")

#Checking that I am pointing to cmdstan
set_cmdstan_path("/Users/jonathan.h.morgan/cmdstan")
cmdstan_path()
cmdstan_version()

#################
#   FUNCTIONS   #
#################


######################
#   SYNTHETIC DATA   #
######################

data <- list(
  x = c(1,2,3,4,5,6,7,8,9,10),
  y = c(5.19,6.56,9.19,8.09,7.6,7.08,6.74,9.3,8.98,11.5)
)

#Setting Temporary Directory
temp <- tempdir()

##################
#   STAN MODEL   #
##################

#Compiling Model
mod <- cmdstan_model("/Users/jonathan.h.morgan/Stan Resources/BayesStanBriefIntro2017Dist/model/linear2.stan")

#Checking Model
mod$print()

#Defining Initial Values to Facilitate Sampling
init <- function() list(
  a = rnorm(1, 6, 3),
  b = rnorm(1, 0.5, 0.5),
  sigma = exp(rnorm(1,log(1.5), 1)))

#Sampling
fit <- mod$sample(
  data = data,
  output_dir = temp,
  seed = 123,
  chains = 4,
  thin = 1,
  parallel_chains = 4,
  init = init,
  iter_warmup = 2000,
  iter_sampling = 2000,
  adapt_delta = 0.85,
  refresh = 500
)

######################
#   VISUALIZATIONS   #
######################

#############
#   TESTS   #
#############

#Calculate the c4 constant for control chart
n <- 10
f1 = sqrt(2/(n-1)) 
f2 = factorial(n/2 -1)/factorial((n-1)/2 - 1)
c4 = f1 * f2













