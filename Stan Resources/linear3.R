#Simple linear model example
#Applying Regularized Horseshoe Prior
#Juho Piironen and Aki Vehtari

rm(list = ls())
gc()

modelName <- "linear2"

################
#   PACKAGES   #
################

library(cmdstanr)   #Used to run cmdstan from R
library(bayesplot)
library(tidyverse)
library(parallel)
source(file.path(toolsDir, "stanTools.R"))

set.seed(10271998) ## not required but assures repeatable results

################################################################################################

#Priors

#Number of Expected Explanatory Variables
p0 <- 2
tau0 <- p0/(1-p0) / sqrt(10)



## create data set
data <- list(
    nObs = as.integer(10),
    d = as.integer(1),
    global_df= 1,
    nu_global = 1,
    nu_local = 1,
    scale_global = 1
    global_scale = tau0,
    x = c(1,2,3,4,5,6,7,8,9,10),
    y = c(5.19,6.56,9.19,8.09,7.6,7.08,6.74,9.3,8.98,11.5)
)

## create initial estimates
init <- function() list(
    a = rnorm(1, 6, 3),
    b = rnorm(1, 0.5, 0.5),
    sigma = exp(rnorm(1,log(1.5), 1)))

## Specify the variables for which you want history and density plots
parametersToPlot <- c("a","b","sigma")

## Additional variables to monitor
otherRVs <- c("ypred")

parameters <- c(parametersToPlot, otherRVs)

################################################################################################
# run Stan

nChains <- 4
nPost <- 1000 ## Number of post-burn-in samples per chain after thinning
nBurn <- 1000 ## Number of burn-in samples per chain after thinning
nThin <- 1

nIter <- (nPost + nBurn) * nThin
nBurnin <- nBurn * nThin

dir.create(outDir)

fit <- stan(file = file.path(modelDir, paste(modelName, ".stan", sep = "")),
            data = data,
            pars = parameters,
            iter = nIter,
            warmup = nBurnin,
            thin = nThin, 
            init = init,
            chains = nChains)

save(fit, file = file.path(outDir, paste(modelName, "Fit.Rsave", sep = "")))
##load(file.path(outDir, paste(modelName, "Fit.Rsave", sep = "")))

################################################################################################
## posterior distributions of parameters

dir.create(figDir)
dir.create(tabDir)

## open graphics device
pdf(file = file.path(figDir, paste(modelName,"Plots%03d.pdf", sep = "")),
	width = 6, height = 6, onefile = F)

options(bayesplot.base_size = 12,
        bayesplot.base_family = "sans")
color_scheme_set(scheme = "brightblue")
myTheme <- theme(text = element_text(size = 12), axis.text = element_text(size = 12))

rhats <- rhat(fit, pars = parametersToPlot)
mcmc_rhat(rhats) + yaxis_text() + myTheme

ratios1 <- neff_ratio(fit, pars = parametersToPlot)
mcmc_neff(ratios1) + yaxis_text() + myTheme

posterior <- as.array(fit)
mcmc_acf(posterior, pars = parametersToPlot) + myTheme

mcmcHistory(fit, pars = parametersToPlot, nParPerPage = 4, myTheme = myTheme)

mcmc_dens_overlay(posterior, parametersToPlot)
mcmc_dens(posterior, parametersToPlot) + myTheme

pairs(fit, pars = parametersToPlot)

ptable <- monitor(as.array(fit, pars = parametersToPlot), warmup = 0, print = FALSE)
write.csv(ptable, file = file.path(tabDir, paste(modelName, "ParameterTable.csv", sep = "")))

################################################################################################
## posterior predictive distributions

pred <- as.data.frame(fit, pars = "ypred") %>%
    gather(factor_key = TRUE) %>%
        group_by(key) %>%
            summarize(lb = quantile(value, probs = 0.05),
                      median = quantile(value, probs = 0.5),
                      ub = quantile(value, probs = 0.95)) %>%
                          bind_cols(data)

p1 <- ggplot(pred, aes(x = x, y = y))
p1 <- p1 + geom_point() +
    labs(x = "x", y = "y") +
        theme(text = element_text(size = 12), axis.text = element_text(size = 12),
              legend.position = "none", strip.text = element_text(size = 8)) 
p1 + geom_line(aes(x = x, y = median)) +
    geom_ribbon(aes(ymin = lb, ymax = ub), alpha = 0.25)

dev.off()
