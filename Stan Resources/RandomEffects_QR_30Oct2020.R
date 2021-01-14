#Generalized Linear Mixed Model with a Change-Point
#Paul McKeigue: http://www.homepages.ed.ac.uk/pmckeigu/stats/glmm_poisson.html
#5 May 2019

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Setting Work Directory
setwd("~/Stan Resources")
getwd()

#Options
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())


################
#   PACKAGES   #
################

library(forecast)   #Used to generate autocorrelation plots
library(psych)      #Used to generate correlation plots
library(extraDistr) #Simulate directly a Half-Cauchy Distribution
library(cmdstanr)   #Used to run cmdstan from R
library(posterior)  #Used to sample posterior distribution
library(bayesplot)  #Used to generate Bayesian diagnostic plots
library(dplyr)      #Data Management
library(ddpcr)
library(pander)

#Checking that I am pointing to cmdstan
set_cmdstan_path("/Users/jonathan.h.morgan/cmdstan")
cmdstan_path()
cmdstan_version()

################################
#   CONSTRUCT SYNTHETIC DATA   #
################################

N.indivs <- 200                 #Num individuals
beta <- -0.5                    #Log rate ratio parameter
mean.numintervals <- 10
mean.logbaseline <- 0
sd.logbaseline <- 2

newrun <- TRUE
withvb <- FALSE

if(newrun) { 
  
  #Generate random number of person-time intervals for each individual
  T <- 1 + rpois(N.indivs, mean.numintervals)
  
  #Simulate baseline hazard rates from a log-normal distribution
  logbaseline <- rnorm(N.indivs, mean.logbaseline, sd.logbaseline)
  
  #Simulate fixed co-variate
  x1 <- rnorm(N.indivs, 0, 1) # fixed covariate
  x2 <- rnorm(N.indivs, 0, 1) # fixed covariate
  
  #Simulate from model with no change point
  lambda <- exp(logbaseline + beta * x1)
  mixdata <- NULL
  for(i in 1:N.indivs) {
    mixdata <- rbind(mixdata,
                     data.frame(indiv=as.factor(rep(i, T[i])),
                                time=1:T[i],
                                x1=rep(x1[i], T[i]),
                                x2=rep(x2[i], T[i]),
                                y=rpois(T[i], lambda[i])
                     )) 
  }
  head(mixdata)
  y <- mixdata$y
  N <- length(y)
  N_indivs <- length(unique(mixdata$indiv))
  X <- model.matrix(object =  y ~ x1 + x2, data = mixdata)[, -1]
  X <- scale(X, center=TRUE, scale=FALSE) # centre covariates
  U <- ncol(X)
  indiv <- as.integer(mixdata$indiv)
  scale_icept <- 5
  priormean_icept <- -5
  
} else {
  load(file="glmpoisson.RData")
}

table(y)

####################################
#   NEGATIVE BINOMIAL REGRESSION   #
####################################

require(MASS)
nbmodel <- glm.nb(formula =  y ~ x1 +  x2, data = mixdata, link = log)

summary(nbmodel)

############################################################
#   POISSON MODEL WITH ADAPTIVE GAUSS-HERMITE QUADRATURE   #
############################################################

#Adaptive Gauss-Hermite Quadrature: Used for the computation of the log-likelihood function for 
#                                   generalized linear mixed models. 

require(lme4)
## adaptive Gauss-Hermite quadrature
ghmodel <- lme4::glmer(formula =  y ~ x1 + x2 + (1 | indiv), data = mixdata, family = poisson,
                       control = glmerControl(),
                       start = NULL, verbose = 0L, nAGQ = 1L, # subset, weights, na.action,
                       offset = NULL, contrasts = NULL, # mustart, etastart,
                       devFunOnly = FALSE)
summary(ghmodel)

#######################
#   STAN ESTIMATION   #
#######################

set.seed(10271998) ## not required but assures repeatable results

#Setting Temporary Directory
temp <- tempdir()

#Create Data Object for the Stan Model
data <- list(
  N = N,                                         #Number of Observations
  U = dim(X)[[2]],                               #Number of Unpenalized Columns in X Matrix
  N_indivs = N.indivs,                           #Number of Individuals
  indiv = indiv,                                  #Index Variable of Individuals for each Observation
  priormean_icept = priormean_icept,             #Prior Mean of Intercept
  scale_icept = scale_icept,                     #SD of Prior on Intercept
  y = y,                                         #Event Count
  X  = X                                         #Design Matrix without Intercept Column
)

data

#Compiling Model
mod <- cmdstan_model("/Users/jonathan.h.morgan/Desktop/Poisson_QR_Mixed_30Oct2020.stan")

#Checking Model
mod$print()

#Sampling
fit <- mod$sample(
  data = data,
  output_dir = temp,
  seed = 123,
  chains = 4,
  thin = 1,
  parallel_chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  adapt_delta = 0.90,
  refresh = 500
)

############################
#   MODEL VISUALIZATIONS   #
############################

#Using Diagnose as a High-Level Assessment
fit$cmdstan_diagnose()

#Creating Fit Objects
sfit <- fit$output_files() %>%
  rstan::read_stan_csv()

#Creating Summary Object
rstan_summary <- rstan::summary(sfit)
rstan_summary <- rstan_summary$summary
rstan_summary

#Quickly Look at: "alpha", "beta", and "sigma_indiv" 
alpha_summary <- fit$summary('alpha')
beta_1_summary <- fit$summary('beta[1]')
beta_2_summary <- fit$summary('beta[2]')
sigma_indv_summary <- fit$summary('sigma_indiv')

#Getting Posterior Draws
draws_array <- fit$draws()
str(draws_array)

#Converting to data frame 
draws_df <- as_draws_df(draws_array)

variables <- c('alpha', 'beta[1]', 'beta[2]', 'sigma_indiv')
posterior_list <- vector('list', length(variables))
names(posterior_list) <- variables
for (i in seq_along(posterior_list)) {
  posterior_list[[i]] <- vector('list', 4)
}

chain_values <- matrix(c(1, 2001, 4001, 6001, 2000, 4000, 6000, 8000),ncol=2, nrow=4)

for (i in seq_along(variables)) {
  var_draws <- draws_df[c(variables[[i]])]
  for (j in seq_along(posterior_list[[i]])){
    iteration_values <- chain_values[j, ]
    posterior_list[[i]][[j]] <- var_draws[c(iteration_values[[1]]:iteration_values[[2]]), ][[1]]
    rm(iteration_values)
  }
  rm(var_draws)
}

#Plotting Chains and Densities
colors <- color_scheme_get("mix-blue-red")[c(1:4)]
titles <- expression(alpha, beta[1], beta[2], sigma["individual"])

#Chains
png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 2, 3, 4), nrow = length(variables), ncol = 1)
  layout(mat = layout.matrix,
       heights = c(2, 2, 2, 2), # Heights of the two rows
       widths = c(2, 2, 2, 2)) # Widths of the two columns

  for (i in seq_along(posterior_list)){
    y_axis <- pretty(unlist(posterior_list[[i]]))
    x_values <- seq(1, length(posterior_list[[1]][[1]]), 1)

    plot(0, type='n', xlab=' ', ylab=' ', xlim=c(0, 2000), ylim=c(min(y_axis), max(y_axis)) ,cex.axis=1.3, family='HersheySerif', las=1, main=' ', bty='n')
    grid(lwd = 2)

    for (j in 1:length(posterior_list[[i]])) {
      lines(x=x_values, y=posterior_list[[i]][[j]], col=colors[[j]])
    }
    title(titles[[i]], family='serif', cex.main=2.5, line=1.25)
  }
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

p_1

#Densities
png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 2, 3, 4), nrow = length(variables), ncol = 1)
  layout(mat = layout.matrix,
       heights = c(2, 2, 2, 2), # Heights of the two rows
       widths = c(2, 2, 2, 2)) # Widths of the two columns

  for (i in seq_along(posterior_list)){
    var_den <- density(unlist(posterior_list[[i]]))
    y_axis <- c(pretty(var_den$y), max(pretty(var_den$y)) + (max(pretty(var_den$y))/length(pretty(var_den$y))))
    x_values <- pretty(var_den$x)
  
    plot(0, type='n', xlab=' ', ylab=' ', xlim=c(min(x_values), max(x_values)), ylim=c(min(y_axis), max(y_axis)) ,cex.axis=1.3, family='HersheySerif', las=1, main=' ', bty='n')
    grid(lwd = 2)
  
    for (j in 1:length(posterior_list[[i]])) {
      lines(x=density(posterior_list[[i]][[j]])$x, y=density(posterior_list[[i]][[j]])$y, col=colors[[j]])
    }
    title(titles[[i]], family='serif', cex.main=2.5, line=1.25)
  }
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_2 <- ggplotify::as.ggplot(g)
rm(g)

p_2
