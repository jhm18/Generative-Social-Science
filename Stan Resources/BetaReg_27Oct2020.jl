#Beta Regression: Food Expenditures
#Posted by Kazuki Yoshida (https://www.rpubs.com/kaz_yos/stan_beta1)
#Adapted into Julia: Jonathan H. Morgan
#27 October 2020

#Activating the Environment
using Pkg
Pkg.activate("/Users/jonathan.h.morgan/Desktop/Themis.Cog/Big_5/Data_Scripts/Data_Scripts")
Pkg.status()

#Defining my Package Versions for Reproducibility in the Future
cd(mktempdir()) do
           Pkg.activate(".")
           Pkg.add(PackageSpec(name="CSV", version="0.7.7"))
           Pkg.add(PackageSpec(name="CmdStan", version="6.0.9"))
           Pkg.add(PackageSpec(name="DataFrames", version="0.21.7"))
           Pkg.add(PackageSpec(name="DataFramesMeta", version="0.5.1"))
           Pkg.add(PackageSpec(name="GLM", version="1.3.11"))
           Pkg.add(PackageSpec(name="Glob", version="1.3.0"))
           Pkg.add(PackageSpec(name="MCMCChains", version="4.1.0"))
           Pkg.add(PackageSpec(name="MixedModels", version="3.0.0"))
           Pkg.add(PackageSpec(name="Plots", version="1.6.0"))
           Pkg.add(PackageSpec(name="RCall", version="0.13.7"))
           Pkg.add(PackageSpec(name="StanDiagnose", version="2.1.0"))
           Pkg.add(PackageSpec(name="StanSample", version="2.2.1"))
           Pkg.add(PackageSpec(name="StatisticalRethinking", version="2.2.5"))
           Pkg.add(PackageSpec(name="StatsBase", version="0.33.1"))
           Pkg.add(PackageSpec(name="StatsPlots", version="0.14.13"),
                   preserve=Pkg.PRESERVE_DIRECT)
           Pkg.status()
end

################
#   PACKAGES   #
################

using CSV                   #Export Files as CSV
using CmdStan               #Stan Julia Interface
using Distributed           #Support Parallel Computing
using DataFrames            #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta        #Facilitates DataFrame Manipulation
using Distributed           #Support Parallel Computing
using GLM                   #Used to model General Linear Models
using Glob                  #Useful Package for String Manipulation
using MCMCChains            #Used for MCMC based estimation
#using MixedModels           #Used for Mixed Effects Models (Get Variance Components)
using Plots                 #Generate basic GR Plots
using RCall                 #Supports Calling R Functions
using StanDiagnose          #Used to assess Stan Models
using StanSample            #Used for Sampling Posterior Distribution
using StatisticalRethinking #Supports Prior and Posterior Analyses
using StatsBase             #Provides basic support for statistics
using StatsPlots            #Generates Statistical Plots for Stan.jl

#################
#   FUNCTIONS   #
#################

#Options: Parallel Processcing & Temporary Directory
addprocs(4)
nprocs()
tmpdir = mktempdir()

######################
#   IMPORTING DATA   #
######################

R"""

data(FoodExpenditure, package = "betareg")

X1 <- model.matrix(object = ~ income + persons, data = FoodExpenditure)
X2 <- model.matrix(object = ~ income + persons, data = FoodExpenditure)

X1[[1]] <- as.integer(X1[[1]])
X1[[3]] <- as.integer(X1[[3]])

X2[[1]] <- as.integer(X2[[1]])
X2[[3]] <- as.integer(X2[[3]])

"""
food_exp = reval("FoodExpenditure")
food_exp = rcopy(food_exp)

X1 = reval("X1")
X1 = rcopy(X1)


X2 = reval("X2")
X2 = rcopy(X2)

food_exp.p_food = food_exp.food./food_exp.income
food_exp = food_exp[:,[3, 1, 2, 4]]

############################
#   BASIC VISUALIZATIONS   #
############################

#Expenditures per Person
sort(unique(food_exp.persons))
Plots.scatter(xlim = (0.5, 7.5), ylim=(0,1), food_exp.persons, food_exp.p_food, markercolor="blue",markersize=6, xlab="Indviduals", ylab="Food Percentage" ,title="Expenditures by Person", legend=false)

#Expenditures vs. Income
sort(unique(food_exp.income))
Plots.scatter(xlim = (25, 89), ylim=(0,1), food_exp.income, food_exp.p_food, markercolor="blue",markersize=6, xlab="Income", ylab="Food Purchases" ,title="Expenditures vs. Income", legend=false)

#############################
#   SPECIFYING STAN MODEL   #
#############################

#Create Data Objects
N = nrow(food_exp)
y = food_exp.p_food

#Mean Model
X1_dim = size(X1)[2]
beta_x1_mean = [0; 0; 0]
beta_x1_sd = [10; 1; 1]

#Precision Model
X2_dim = size(X2)[2]
beta_x2_mean = [0; 0; 0]
beta_x2_sd = [10; 1; 1]

base_data = Dict("N" => N, "y" => y, "X1" => X1, "X1_dim" => X1_dim, "beta_x1_mean" => beta_x1_mean, "beta_x1_sd" => beta_x1_sd, "X2" => X2, "X2_dim" => X2_dim, "beta_x2_mean" => beta_x2_mean, "beta_x2_sd" => beta_x2_sd)

#Specifying Stan Script
beta_reg = "
data {
        /* Outcome data */
        int<lower=0> N;
        real<lower=0,upper=1> y[N];

        /* X1 matrix for the mean parameter. Include a constant column. */
        int<lower=1> X1_dim;
        matrix[N, X1_dim] X1;
        real beta_x1_mean[X1_dim];
        real<lower=0> beta_x1_sd[X1_dim];

        /* X2 matrix for the precision parameter. Include a constant column. */
        int<lower=1> X2_dim;
        matrix[N, X2_dim] X2;
        real beta_x2_mean[X2_dim];
        real<lower=0> beta_x2_sd[X2_dim];
}

parameters {
        vector[X1_dim] beta_x1;
        vector[X2_dim] beta_x2;
}

transformed parameters {
        vector[N] eta_x1 = X1 * beta_x1;
        vector[N] eta_x2 = X2 * beta_x2;

        /* logit for mean. exp is the inverse. */
        vector[N] mu = inv_logit(eta_x1);

        /* log for precision. exp is the inverse. */
        vector[N] phi = exp(eta_x2);
}

model {

        /* Priors */
        for (j in 1:X1_dim) {
                target += normal_lpdf(beta_x1[j] | beta_x1_mean[j], beta_x1_sd[j]);
                }

        for (k in 1:X2_dim) {
                target += normal_lpdf(beta_x2[k] | beta_x2_mean[k], beta_x2_sd[k]);
                }

        /* Mean model */
        for (i in 1:N) {
                target += beta_lpdf(y[i] | (mu .* phi)[i], ((1-mu) .* phi)[i]);
                }
}

"
#Specifying Sampling Model
sm = SampleModel("beta_reg", beta_reg, [4], seed=StanSample.RandomSeed(seed=10271998),
  method=StanSample.Sample(save_warmup=true, num_warmup=1000, num_samples=1000, thin=1, adapt=StanSample.Adapt(delta=0.9)), output=StanSample.Output(refresh=500),
  tmpdir=tmpdir)

#Sampling Posterior
rc = StanSample.stan_sample(sm, data=base_data, n_chains=4, diagnostics=true)

#Looking at Results (Could be Refined)
if success(rc)
  chn = read_samples(sm; output_format=:mcmcchains, include_internals=true)
  show(chn)
end

#Getting Stan Summay and Description
stan_summary(sm, true)
describe(chn)

#Convergence Diagnostics: http://www.stat.columbia.edu/~gelman/research/published/brooksgelman2.pdf
MCMCChains.gelmandiag(chn::Chains)

###########################
#   GETTING STAN OUTPUT   #
###########################

stan_files = glob("*.csv", tmpdir)
model_summary = DataFrame!(CSV.File(stan_files[6]))

#Creating Chain Datasets
chain_names = chn.value[:, :, 1].axes
chain_names = [:lp__, :accept_stat__, :stepsize__, :treedepth__, :n_leapfrog__, :divergent__, :energy__, Symbol("beta_x1.1"),
              Symbol("beta_x1.2"), Symbol("beta_x1.3"), Symbol("beta_x2.1"), Symbol("beta_x2.2"), Symbol("beta_x2.3")]

chains = fill(DataFrames.DataFrame(), 1, 4)
for i in 1:length(chains)
    chains[i] = DataFrames.DataFrame(chn.value[:, :, i].data)
    chains[i] = chains[i][(1001:nrow(chains[i])), (1:13)]
    DataFrames.rename!(chains[i], chain_names)
end

#Trace Plot
l_1 = @layout [a; b]
  c_1 = plot([chains[1][:,8] chains[2][:,8] chains[3][:,8] chains[4][:,8]], title="Intercepts: Mean Model", label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  c_2 = plot([chains[1][:,11] chains[2][:,11] chains[3][:,11] chains[4][:,11]], title="Intercepts: Precision Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
intercpet_plots = plot(c_1, c_2, layout= l_1, legend = false)

l_1 = @layout [a; b]
  c_3 = plot([chains[1][9] chains[2][9] chains[3][9] chains[4][9]], title="Income: Mean Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  c_4 = plot([chains[1][12] chains[2][12] chains[3][12] chains[4][12]], title="Income: Precison Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
income_plots = plot(c_3, c_4, layout= l_1, legend = false)

l_1 = @layout [a; b]
  c_5 = plot([chains[1][10] chains[2][10] chains[3][10] chains[4][10]], title="Persons: Mean Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  c_6 = plot([chains[1][13] chains[2][13] chains[3][13] chains[4][13]], title="Persons: Precision Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
person_plots = plot(c_5, c_6, layout= l_1, legend = false)

#Density Plots
l_2 = @layout [a; b]
  d_1 = density([chains[1][:,8] chains[2][:,8] chains[3][:,8] chains[4][:,8]], title="Intercepts: Mean Model", label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  d_2 = density([chains[1][:,11] chains[2][:,11] chains[3][:,11] chains[4][:,11]], title="Intercepts: Precision Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
intercept_fits = plot(d_1, d_2, layout = l_2, legend=false)

l_2 = @layout [a; b]
  d_3 = density([chains[1][9] chains[2][9] chains[3][9] chains[4][9]], title="Income: Mean Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  d_4 = density([chains[1][12] chains[2][12] chains[3][12] chains[4][12]], title="Income: Precison Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
intercept_fits = plot(d_3, d_4, layout = l_2, legend=false)

l_2 = @layout [a; b]
  d_5 = density([chains[1][10] chains[2][10] chains[3][10] chains[4][10]], title="Persons: Mean Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
  d_6 = density([chains[1][13] chains[2][13] chains[3][13] chains[4][13]], title="Persons: Precision Model",label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"])
intercept_fits = plot(d_5, d_6, layout = l_2, legend=false)

#Diagnostic Plots
