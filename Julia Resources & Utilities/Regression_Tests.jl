#Jonathan H. Morgan
#Julia: Econometrics (https://juliaeconomics.com/)
#26 June 2020

#Setting Work Directory: Home
cd("/Users/jonathan.h.morgan/Julia Resources")

################
#   PACKAGES   #
################

using LinearAlgebra     #Provide tools for solving linear equations
using GLM               #Supports linear and generalized linear models
using Distributions     #Package for probability distributions and associated functions
using Optim             #Univariate and multivariate optimization in Julia.

#################
#   FUNCTIONS   #
#################

include("/Users/jonathan.h.morgan/Julia Resources/Julia_Utilities_28May2020.jl")

########################
#   LINEAR FUNCTIONS   #
########################

#Generating Some Synthetic Data
N=1000
mu = 0    #The mean of the truncated Normal
sigma = 1 #The standard deviation of the truncated Normal
lb = 0    #The truncation lower bound
ub = 1    #The truncation upper bound
d = Truncated(Normal(mu, sigma), lb, ub)  #Construct the distribution type
d_2 = Normal(mu, sigma)
x = rand(d, N) #Simulate 1000 obs from the truncated Normal

#Generating y with Error
genEpsilon = Normal(0, 1)
epsilon = rand(genEpsilon,N)
y = x  + epsilon

#OLS Estimation with Basic Function
estimates = OLSestimator(y,x)

#OLS Estimation Done in GLM
data = DataFrames.DataFrame(hcat(x, y))
DataFrames.rename!(data, ["x", "y"])

ols = lm(@formula(y ~ x), data)

#####################################
#   MAXIMUM LIKELIHOOD ESTIMATION   #
#####################################

#Computing the Log-Likelihood of OLS: https://juliaeconomics.com/2014/06/16/numerical-maximum-likelihood-the-ols-example/
function loglike(rho)
   beta = rho[1:4]
   sigma2 = exp(rho[5])
   residual = Y-X*beta
   dist = Normal(0, sqrt(sigma2))
   contributions = logpdf(dist,residual)
   loglikelihood = sum(contributions)
   return -loglikelihood
end

#Maximum Likelihood Estimation: https://juliaeconomics.com/2014/06/16/numerical-maximum-likelihood-the-ols-example/
params0 = [.1,.2,.3,.4,.5]
optimum = optimize(loglike,params0,method=:cg)
MLE = optimum.minimum
MLE[5] = exp(MLE[5])
println(MLE)

#Predicting Values using GLM
predict(ols)

#Build on this with other models
#Figure out how to get MLE to work.
