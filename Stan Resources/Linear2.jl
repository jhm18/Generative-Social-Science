#Stan Linear Model Example using Stan.jl
#Adapted from https://metrumrg.com/course/brief-introduction-bayesian-modeling-using-stan/
#Jonathan H. Morgan
#14 September 2020

#Activating the Environment
using Pkg
Pkg.activate("/Users/jonathan.h.morgan/Desktop/ACT/Duke10/Scripts_&_Visualizations/Julia_Scripts")
Pkg.instantiate()
Pkg.status()

#Defining my Package Versions for Reproducibility in the Future
cd(mktempdir()) do
           Pkg.activate(".")
           Pkg.add(PackageSpec(name="CSV", version="0.7.7"))
           Pkg.add(PackageSpec(name="DataFrames", version="0.21.7"))
           Pkg.add(PackageSpec(name="DataFramesMeta", version="0.5.1"))
           Pkg.add(PackageSpec(name="Distributions", version="0.23.8"))
           Pkg.add(PackageSpec(name="Glob", version="1.3.0"))
           Pkg.add(PackageSpec(name="LaTeXStrings", version="1.1.0"))
           Pkg.add(PackageSpec(name="MCMCChains", version="4.1.0"))
           Pkg.add(PackageSpec(name="NamedArrays", version="0.9.4"))
           Pkg.add(PackageSpec(name="Plots", version="1.6.0"))
           Pkg.add(PackageSpec(name="RCall", version="0.13.7"))
           Pkg.add(PackageSpec(name="StanDiagnose", version="2.1.0"))
           Pkg.add(PackageSpec(name="StanSample", version="2.2.1"))
           Pkg.add(PackageSpec(name="StatisticalRethinking", version="2.2.5"))
           Pkg.add(PackageSpec(name="StatsBase", version="0.33.1"))
           Pkg.add(PackageSpec(name="MixedModels", version="3.0.0"))
           Pkg.add(PackageSpec(name="StatsPlots", version="0.14.13"),
                   preserve=Pkg.PRESERVE_DIRECT)
           Pkg.status()
end

################
#   PACKAGES   #
################

using CSV                   #Export Files as CSV
using DataFrames            #Generates Julia Style DataFrames and DataFrame Manipulation
using DataFramesMeta        #Facilitates DataFrame Manipulation
using Distributed           #Support Parallel Computing
using Distributions         #Used for sampling from a simulated normal
using Glob                  #Useful Package for String Manipulation
using LaTeXStrings          #Support Mathematical Symbols in Plot Labels
using MCMCChains            #Used for MCMC based estimation
using MixedModels           #Used to analyse covariance matrices
using NamedArrays           #Replicating R's Named Lists Functionality
using Plots                 #Useful for Basic Plotting
using RCall                 #Supports Calling R Functions
using StanSample            #Used for Sampling Posterior Distribution
using CmdStan               #Stan Julia Interface
using StanDiagnose          #Used to assess Stan Models
using StatisticalRethinking #Supports Prior and Posterior Analyses
using StatsBase             #Provides basic support for statistics
using StatsPlots            #Generates Statistical Plots for Stan.jl

#################
#   FUNCTIONS   #
#################

include("/Users/jonathan.h.morgan/Julia Resources/Julia_Utilities_23August2020.jl")

#Options: Parallel Processcing & Temporary Directory
addprocs(4)
nprocs()
tmpdir = mktempdir()

###############################
#   CREATING SYNTHETIC DATA   #
###############################

#Base Data: Named Array that Corresponds to a Named List in R
x = [1,2,3,4,5,6,7,8,9,10]
y = [5.19,6.56,9.19,8.09,7.6,7.08,6.74,9.3,8.98,11.5]
nObs = length(x)
base_data = Dict("nObs" => nObs, "x" => x, "y" => y)

#Initial Parameter Estimates
function init()
    alpha = rand(Distributions.Normal(6,3),1)
    beta = rand(Distributions.Normal(0.5, 0.5),1)
    sigma = exp.(rand(Distributions.Normal(log(1.5), 1),1))
    parameter_estimates = Dict("a" => alpha, "b" => beta, "sigma" => sigma)
    return parameter_estimates
end

init()

##############################################
#  CORRELATIONS & PRIOR PREDICATIVE CHECKS   #
##############################################

#Correlations
viz_data = DataFrames.DataFrame([x y])
DataFrames.rename!(viz_data, ["x", "y"])

@rput viz_data

R"""
#Home
setwd("~/Desktop")
getwd()

options(scipen=6)
options(stringsAsFactors = FALSE)
options(device = "quartz")

setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(width = 12, height = 10, xpos = 0,
                                             pointsize = 10))

################
#   PACKAGES   #
################

library("psych")

######################
#   Visualizations   #
######################

png("p_1.png", width = 781, height = 680)
  par(family='HersheySerif')
  pairs.panels(viz_data, pch=21, main="Linear 2: Example Data", cex=1.5)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("Linear2_Pairs_22Sept2020.pdf",
                dpi=600, width = 6.5, height = 5)

"""

#Possible Intercept Priors
a1 = rand(Distributions.Normal(0, 0.5), 100000)
a2 = rand(Distributions.Normal(0, 1), 100000)
a3 = rand(Distributions.Normal(0, 100), 100000)
prior_dist = DataFrames.DataFrame([a1 a2 a3])
DataFrames.rename!(prior_dist, ["normal (0, 0.5)", "normal (0, 1)", "normal (0, 100)"])

l_0 = @layout [a; b]
  pr_1 = @df prior_dist density(cols(1:2), title="Informative Priors", legend = :topleft)
  pr_2 = @df prior_dist density(cols(3), title="Weakly Informative Priors", linecolor = :black,label = "normal (0, 100)", legend = :topleft)
prior_plots = plot(pr_1, pr_2, layout= l_0, legend = false)

savefig(prior_plots,"prior_dist.png")

#Intercept and Beta Priors: Demonstrating Functional Range
a1 = rand(Distributions.Normal(0, 0.5), 100)
a2 = rand(Distributions.Normal(0, 1), 100)
a3 = rand(Distributions.Normal(0, 100), 100)

b1 = rand(Distributions.Normal(0, 0.25), 100)
b2 = rand(Distributions.Normal(0, 0.50), 100)
b3 = rand(Distributions.Normal(0, 100), 100)

pr_3 = plot(xlim = (-5, 5), ylim=(-5, 5), title = "α ~ normal(0, 0.5), β ~ normal(0, 0.25) ", legend=false)
for i in 1:length(a1)
  Plots.abline!(a1[i], b1[i], linecolor=:steelblue)
end

pr_4 = plot(xlim = (-5, 5), ylim=(-5, 5), title = "α ~ normal(0, 1), β ~ normal(0, 0.5) ", legend=false)
for i in 1:length(a2)
  Plots.abline!(a2[i], b2[i], linecolor=:brown)
end

pr_5 = plot(xlim = (-100, 100), ylim=(-2000, 2000), title = "α ~ normal(0, 100), β ~ normal(0, 100) ", legend=false)
for i in 1:length(a3)
  Plots.abline!(a3[i], b3[i], linecolor=:black)
end

l_0 = @layout [a; b; c]
pr_3
pr_4
pr_5
intercept_plots = plot(pr_3, pr_4, pr_5, layout= l_0, legend = false)
savefig(intercept_plots,"InterceptSlope_plots.png")

#############################
#   SPECIFYING STAN MODEL   #
#############################

linear2 = "
data{
  int<lower = 1> nObs;
  real<lower = 1> x[nObs];
  real<lower = 1> y[nObs];
}

parameters{
  real a;
  real b;
  real<lower=0> sigma;
 }

transformed parameters{
  vector[nObs] ymean;

  for(i in 1:nObs) ymean[i] = a + b * x[i];
}

model{
  //Priors
  a ~ normal(0, 100);
  b ~ normal(0, 100);
  sigma ~ cauchy(0, 2);

  y ~ normal(ymean, sigma);
  }

generated quantities{
  vector[nObs] ypred;

  for(i in 1:nObs) ypred[i] = normal_rng(ymean[i], sigma);
  }
";

####################
#   SAMPLE MODEL   #
####################

#Specifying Sampling Model
sm = SampleModel("linear_2", linear2, [4], seed=StanSample.RandomSeed(seed=10271998),
  method=StanSample.Sample(save_warmup=true, num_warmup=1000, num_samples=1000, thin=1, adapt=StanSample.Adapt(delta=0.9)),
  tmpdir=tmpdir)

#Sampling Posterior
rc = StanSample.stan_sample(sm, data=base_data, n_chains=4, init=init(), diagnostics=true)

#Checking Model
stanmodel = DiagnoseModel("linear2", linear2;
  method=StanDiagnose.Diagnose(StanDiagnose.Gradient(epsilon=1e-6)),
  tmpdir = tmpdir);

rc = stan_diagnose(stanmodel; data=base_data)

#Looking at Results (Could be Refined)
if success(rc)
  chn = read_samples(sm; output_format=:mcmcchains, include_internals=true)
  c1_pred = stan_generate_quantities(sm, 1)
  c2_pred = stan_generate_quantities(sm, 2)
  c3_pred = stan_generate_quantities(sm, 3)
  c4_pred = stan_generate_quantities(sm, 4)
  diags = StanDiagnose.read_diagnose(stanmodel)
  display(diags)
  show(chn)
end

####################
#   MODEL OUTPUT   #
####################

#Getting Stan Summay and Description
stan_summary(sm, true)
describe(chn)

#Convergence Diagnostics: http://www.stat.columbia.edu/~gelman/research/published/brooksgelman2.pdf
MCMCChains.gelmandiag(chn::Chains)

#Creating Chain Datasets
chain_names = chn.value[:, :, 1].axes
chain_names = [:lp__, :accept_stat__, :stepsize__, :treedepth__, :n_leapfrog__, :divergent__, :energy__, :a, :b, :sigma]

#Chain 1
chain_1 = DataFrames.DataFrame(chn.value[:, :, 1].data)
chain_1 = chain_1[(1001:nrow(chain_1)), (1:10)]
DataFrames.rename!(chain_1, chain_names)
c1_a_mean = mean(chain_1.a)
c1_a_sd = Distributions.std(chain_1.a)
c1_b_mean = mean(chain_1.b)
c1_b_sd = Distributions.std(chain_1.b)
c1_sig_mean = mean(chain_1.sigma)
c1_sig_sd = Distributions.std(chain_1.sigma)

#Chain 2
chain_2 = DataFrames.DataFrame(chn.value[:, :, 2].data)
chain_2 = chain_2[(1001:nrow(chain_2)), (1:10)]
DataFrames.rename!(chain_2, chain_names)
c2_a_mean = mean(chain_2.a)
c2_a_sd = Distributions.std(chain_2.a)
c2_b_mean = mean(chain_2.b)
c2_b_sd = Distributions.std(chain_2.b)
c2_sig_mean = mean(chain_2.sigma)
c2_sig_sd = Distributions.std(chain_2.sigma)

#Chain 3
chain_3 = DataFrames.DataFrame(chn.value[:, :, 3].data)
chain_3 = chain_3[(1001:nrow(chain_3)), (1:10)]
DataFrames.rename!(chain_3, chain_names)
c3_a_mean = mean(chain_3.a)
c3_a_sd = Distributions.std(chain_3.a)
c3_b_mean = mean(chain_3.b)
c3_b_sd = Distributions.std(chain_3.b)
c3_sig_mean = mean(chain_3.sigma)
c3_sig_sd = Distributions.std(chain_3.sigma)

#Chain 4
chain_4 = DataFrames.DataFrame(chn.value[:, :, 4].data)
chain_4 = chain_4[(1001:nrow(chain_4)), (1:10)]
DataFrames.rename!(chain_4, chain_names)
c4_a_mean = mean(chain_4.a)
c4_a_sd = Distributions.std(chain_4.a)
c4_b_mean = mean(chain_4.b)
c4_b_sd = Distributions.std(chain_4.b)
c4_sig_mean = mean(chain_4.sigma)
c4_sig_sd = Distributions.std(chain_4.sigma)

#Coefficients Dataset
coeff_names = vec(["chain_1.a" "chain_2.a" "chain_3.a" "chain_4.a" "chain_1.b" "chain_2.b" "chain_3.b" "chain_4.b" "chain_1.sigma" "chain_2.sigma" "chain_3.sigma" "chain_4.sigma"])
coeff_names = Symbol.(coeff_names)
coeff_estimates = DataFrames.DataFrame([chain_1.a chain_2.a chain_3.a chain_4.a chain_1.b chain_2.b chain_3.b chain_4.b chain_1.sigma chain_2.sigma chain_3.sigma chain_4.sigma])
DataFrames.rename!(coeff_estimates, coeff_names)

#############################################
#   CHAIN & FIT DIAGNOSTIC VISUALIZATIONS   #
#############################################

#Looking at Log File & CSVs: Need to Run stan_summary()
log_files = glob("*.log", tmpdir)
stan_files = glob("*.csv", tmpdir)

model_summary = DataFrame!(CSV.File(stan_files[13]))
R_Hat = model_summary[:,[1, 10]]
R_Hat = R_Hat[(8:10), :]
R_Hat.R_hat = parse.(Float64, R_Hat.R_hat)

N_Eff = model_summary[:, [1, 8]]
N_Eff = N_Eff[(8:10), :]
N_Eff.Efficiency = N_Eff.N_Eff/4000

#BASE PLOTS
p1 = plot(chn)
p2 = pooleddensity(chn)

#PLOT CHAINS

#Full Chains: All Chains
plot(chn::Chains, seriestype = :traceplot)

#Parameters
l_1 = @layout [a; b; c]
  c_1 = @df coeff_estimates plot(cols(1:4), title="Alpha",legend = :topleft)
  c_2 = @df coeff_estimates plot(cols(5:8), title="Beta", legend = :topleft)
  c_3 = @df coeff_estimates plot(cols(9:12), title="Sigma", legend = :topleft)
trace_plots = plot(c_1, c_2, c_3, layout= l_1, legend = false)

savefig(trace_plots,"p_traces.png")

#PARAMETER FIT DISTRIBUTIONS
l_2 = @layout [a b; c]
  p_1 = @df coeff_estimates density(cols(1:4), title="Alpha", label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"], legend = :topleft)
  p_2 = @df coeff_estimates density(cols(5:8), title="Beta", label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"], legend = :topleft)
  p_3 = @df coeff_estimates density(cols(9:12), title="Sigma", label=["Chain 1" "Chain 2" "Chain 3" "Chain 4"], legend = :topright)
p_fits = plot(p_1, p_2, p_3, layout = l_2)

savefig(p_fits,"p_fits.png")

#DIVERGENCE

#Getting Divergence Data
run_type = DataFrames.DataFrame([fill("Warmup", 1000, 1); fill("Model Runs", 1000, 1)])
DataFrames.rename!(run_type, ["phase"])

div_1 = DataFrames.DataFrame(chn.value[:, :, 1].data)
div_1 = div_1[:,6]
run_type.div_1 = div_1

div_2 = DataFrames.DataFrame(chn.value[:, :, 2].data)
div_2 = div_2[:,6]
run_type.div_2 = div_2

div_3 = DataFrames.DataFrame(chn.value[:, :, 3].data)
div_3 = div_3[:,6]
run_type.div_3 = div_3

div_4 = DataFrames.DataFrame(chn.value[:, :, 4].data)
div_4 = div_4[:,6]
run_type.div_4 = div_4

#Calculating Proportion of Divergences by Phase
combine(nrow, groupby(run_type, [:phase], sort=true, skipmissing=true))

div_iterations = [2, 3, 4, 5]
div_stats = fill(0.0, 4, 2)

for i in 1:length(div_iterations)
  divergence = run_type[(run_type[:,div_iterations[i]].==1),:]
  divergence = divergence[:,[1,div_iterations[i]]]
  total = nrow(divergence)
  warmup_d = divergence[(divergence[:,1].=="Warmup"), :]
  warmup_total = nrow(warmup_d)
  model_total = total - warmup_total
  run_stats = [warmup_total/1000 model_total/1000]
  div_stats[i,1] = run_stats[1]
  div_stats[i,2] = run_stats[2]
end

#TREE DEPTH
tree_1 = DataFrames.DataFrame(chn.value[:, :, 1].data)
tree_1 = tree_1[:,4]

tree_2 = DataFrames.DataFrame(chn.value[:, :, 2].data)
tree_2 = tree_2[:,4]

tree_3 = DataFrames.DataFrame(chn.value[:, :, 3].data)
tree_3 = tree_3[:,4]

tree_4 = DataFrames.DataFrame(chn.value[:, :, 4].data)
tree_4 = tree_4[:,4]

trees = DataFrames.DataFrame([tree_1 tree_2 tree_3 tree_4])
DataFrames.rename!(trees, ["Chain 1", "Chain 2", "Chain 3", "Chain 4"])

#STEP-SIZE
step_1 = DataFrames.DataFrame(chn.value[:, :, 1].data)
step_1 = step_1[:,3]

step_2 = DataFrames.DataFrame(chn.value[:, :, 2].data)
step_2 = step_2[:,3]

step_3 = DataFrames.DataFrame(chn.value[:, :, 3].data)
step_3 = step_3[:,3]

step_4 = DataFrames.DataFrame(chn.value[:, :, 4].data)
step_4 = step_4[:,3]

steps = DataFrames.DataFrame([step_1 step_2 step_3 step_4])
DataFrames.rename!(steps, ["Chain 1", "Chain 2", "Chain 3", "Chain 4"])

#Plotting
div_plot = groupedbar(div_stats, bar_position = :dodge, bar_width=0.7, ylim=(0,0.02), color = [:blue :brown], xlab="Chains", ylabel = "Proportion", title="Divergence", label=["Warmup" "Model Runs"])
savefig(div_plot,"div_plot.png")

tree_plot = @df trees plot(cols(1:4), title="Tree Depth", ylim=(0,15), xlab="Iteration", ylabel = "Depth", legend=:topright)
tree_plot = plot!([10], seriestype="hline", linestyle = :dash, linecolor = :brown,label="Default Step Depth")
savefig(tree_plot,"tree_plot.png")

step_plot = @df steps plot(cols(1:4), title="Step Size", ylim=(0,15), xlab="Iteration", ylabel = "Size", legend=:topright)
savefig(step_plot,"step_plot.png")

#R_HAT FIT
variables = [1: 1: nrow(R_Hat); ]
var_names = R_Hat[:,1]
x = R_Hat.R_hat
x_ticks = [0.95; 1; 1.05; 1.10]
y_ticks = [1: 1: nrow(R_Hat); ]

r_plot = Plots.scatter(xlim = (0.95, 1.12), ylim=(0,(nrow(R_Hat)+1)), x, variables, markercolor="blue",markersize=6,xticks=x_ticks, yticks=(y_ticks, var_names), xlab="Ȓ", title="Convergence Checks", legend=false)
r_plot = plot!([1, 1.05, 1.1], seriestype="vline", linestyle = :dash, linecolor = :brown)
for i in 1:nrow(R_Hat)
  r_plot = plot!([0.95, x[i]], [variables[i], variables[i]], seriestype="line", linecolor = :black)
end

r_plot

#EFFECTIVE SAMPLE SIZES/N
x = N_Eff.Efficiency

e_plot = Plots.scatter(xlim=(0,1), ylim=(0,(nrow(R_Hat)+1)), x, variables, markercolor="blue",markersize=6, yticks=(y_ticks, var_names), xlab="N_Eff/N", title="Sampling Efficiency", legend=false)
for i in 1:nrow(R_Hat)
  e_plot = plot!([0, x[i]], [variables[i], variables[i]], seriestype="line", linecolor = :black)
end

e_plot

l_3 = @layout [a b]
sampling_plots = plot(r_plot, e_plot,layout= l_3, legend = false, ytickfontsize=9, yguidefontsize=9, xguidefontsize=9)
savefig(sampling_plots, "sampling_plots.png")

#SAMPLE AUTOCORRELATION PLOTS
# Note: On next iteration, use group coloring to highlight divergent runs.

#Calculating Autcorrelations
auto_correlations = fill(1.0,20, ncol(coeff_estimates))
for i in 1:ncol(coeff_estimates)
 auto_correlation = StatsBase.autocor(coeff_estimates[:,i], 1:20)
 auto_correlations[:,i] = auto_correlation
end

p_array = fill(plot(), 4, 3)
l_4 = @layout [a b c; d e f; g h i; j k l]

for i in 1:ncol(coeff_estimates)
  p = Plots.plot(auto_correlations[:,i], xlab="Lag", ylab="Autocorrelation")
  plot!([0], seriestype="hline")
  p_array[i] = p
end

p_array[1] = plot(p_array[1], title="Alpha")
p_array[5] = plot(p_array[5], title="Beta")
p_array[9] = plot(p_array[9], title="Sigma")

p_1 = p_array[1]
p_2 = p_array[5]
p_3 = p_array[9]
p_4 = p_array[2]
p_5 = p_array[6]
p_6 = p_array[10]
p_7 = p_array[3]
p_8 = p_array[7]
p_9 = p_array[11]
p_10 = p_array[4]
p_11 = p_array[8]
p_12 = p_array[12]

auto_plots = plot(p_1, p_2, p_3, p_4, p_5, p_6, p_7, p_8, p_9, p_10, p_11, p_12,layout= l_4, legend = false, ytickfontsize=5, yguidefontsize=6, xguidefontsize=6)

savefig(auto_plots, "auto_plots.png")

#PAIRS PLOTS: PARAMTERS, LP, & ENERGY
chains = [chain_1; chain_2; chain_3; chain_3]
chains = chains[:,[8,9,10,1, 7]]

@rput chains

R"""
#Home
setwd("~/Desktop")
getwd()

options(scipen=6)
options(stringsAsFactors = FALSE)
options(device = "quartz")

setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(width = 12, height = 10, xpos = 0,
                                             pointsize = 10))

################
#   PACKAGES   #
################

library("psych")

######################
#   Visualizations   #
######################

png("p_1.png", width = 781, height = 680)
  par(family='HersheySerif')
  pairs.panels(chains, pch=21, main="Linear 2: Model Outputs", cex=1.5)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("Linear2_ModelPairs_22Sept2020.pdf",
                dpi=600, width = 6.5, height = 5)

"""

#PREDICTED VALUES COMPARED TO DISTRIBUTION
l_5 = @layout [a b; c d]
  y_val = density(y, title="Y")
  f_1 = @df c1_pred density(cols(1:10), title="Predicted Values: Chain 1", legend = :topleft)
  f_2 = @df c2_pred density(cols(1:10), title="Predicted Values: Chain 2", legend = :topleft)
  f_3 = @df c3_pred density(cols(1:10), title="Predicted Values: Chain 3", legend = :topleft)
pred_plots = plot(y_val, f_1, f_2, f_3, layout= l_5, legend = false)

savefig(pred_plots, "pred_dist.png")

#POSTERIOR VALUES CHECK: MEDIAN PREDICTED VALUES FOR THE ENTIRE FUNCTION
# Median Prediction
# 90% Credible Interval

predictions = [c1_pred; c2_pred; c3_pred; c4_pred]
pred_scores = fill(1.0, ncol(predictions), 3)
for i in 1:ncol(predictions)
  pred_median = median(predictions[:,i])
  pred_interval = quantile(predictions[:,i], [0.05, 0.95])
  pred_values = sort([pred_median; pred_interval])
  pred_scores[i,1] = pred_values[1]
  pred_scores[i,2] = pred_values[2]
  pred_scores[i,3] = pred_values[3]
end

x = [1,2,3,4,5,6,7,8,9,10]
y = [5.19,6.56,9.19,8.09,7.6,7.08,6.74,9.3,8.98,11.5]
pred_scores = [x y pred_scores]
pred_scores = DataFrames.DataFrame(pred_scores)
DataFrames.rename!(pred_scores, ["x", "y","5th_percentile", "Median", "95th_percentile"])

@rput pred_scores

R"""
setwd("~/Desktop")
getwd()

options(scipen=6)
options(stringsAsFactors = FALSE)
options(device = "quartz")

setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(width = 12, height = 10, xpos = 0, pointsize = 10))

#################
#   FUNCTIONS   #
#################

#Home Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_13Dec2019.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

#####################
#   VISUALIZATION   #
#####################

#Creating Plotting Smoothing Elements
xy_1 <- pred_scores[c(1, 3)]
xy_2 <- pred_scores[c(1, 4)]
xy_3 <- pred_scores[c(1, 5)]

plot(0)
val.curve_1 <- xspline(xy_1, shape = -0.5, draw = FALSE)
val.curve_2 <- xspline(xy_2, shape = -0.5, draw = FALSE)
val.curve_3 <- xspline(xy_3, shape = -0.5, draw = FALSE)

curve1_length <- length(val.curve_1$x)
curve2_length <- length(val.curve_2$x)
curve3_length <- length(val.curve_3$x)

curve_min <- min(c(curve1_length, curve2_length, curve3_length))

val.curve_1[[1]] <- val.curve_1[[1]][c(1:curve_min)]
val.curve_1[[2]] <- val.curve_1[[2]][c(1:curve_min)]
val.curve_2[[2]] <- val.curve_2[[2]][c(1:curve_min)]
val.curve_3[[2]] <- val.curve_3[[2]][c(1:curve_min)]

predictions <- data.frame(val.curve_1)
predictions$y2 <- val.curve_2$y
predictions$y3 <- val.curve_3$y
colnames(predictions) <- colnames(pred_scores[c(1,3,4,5)])

junk <- dev.off(which = dev.cur())

rm(xy_1, xy_2, xy_3, val.curve_1, val.curve_2, val.curve_3, curve1_length, curve2_length, curve3_length)

#Creating Plot Elements
y_values = pretty(append(predictions[[2]], predictions[[4]]))

#Calculating localized error
error <- (pred_scores[[5]] - pred_scores[[3]])/2

#Plotting
png("p1.png", width = 729, height = 501)
  par(mar=c(5.1, 4.75, 4.1, 2.1), mgp=c(3.5, 1, 0))
  plot(type='n', x=predictions$x,xlim=c(min(predictions$x), max(predictions$x)), ylim=c(min(y_values), max(y_values)),las=1, bty='n',
     family='HersheySerif', cex.lab=1.5, cex.axis=1.3, xlab=' ', ylab=' ', axes=TRUE)

  #Adding Gridlines
  grid(lwd = 2)

  #Specifying Axes
  mtext(side = 1, text = 'x', at = median(predictions$x), col = "black", line = 3, cex = 1.5, family='HersheySerif')
  mtext(side = 2, text = 'y', at = median(y_values), col = "black", line =3, cex = 1.5, family='HersheySerif')

  #Adding Confidence Bands
  polygon(c(predictions[[1]],rev(predictions[[1]])), c(predictions[[4]],rev(predictions[[2]])) ,col="cornsilk", border = NA)

  #Adding Errorbars
  plotsegraph(pred_scores[[1]], pred_scores$Median, error, 0, color = "grey")

  #Adding Fitted Points
  points(pred_scores$x, pred_scores$Median, pch=21, bg='grey', cex=1.5)

  #Adding Confidence Bands
  lines(predictions$x, predictions[[2]], lty = 'dashed', col = 'red')
  lines(predictions$x, predictions[[4]], lty = 'dashed', col = 'red')

  #Adding Observations
  points(pred_scores$x, pred_scores$y,  pch=21)

  #Adding Fit Line
  lines(predictions$x, predictions[[3]], col='blue', lwd=2)
dev.off()

#Reading png back in to transform into a ggplot object
g <- magick::image_read('p1.png')
file.remove('p1.png')
p1 <- ggplotify::as.ggplot(g)
rm(g)

p1
ggplot2::ggsave("linear2_modelfit_23Sept2020.pdf")

"""

######################
#   TAYLOR DIAGRAM   #
######################

@rput pred_scores

R"""
setwd("~/Desktop")
getwd()

options(scipen=6)
options(stringsAsFactors = FALSE)
options(device = "quartz")

setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(width = 12, height = 10, xpos = 0, pointsize = 10))

#################
#   FUNCTIONS   #
#################

#Home Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_13Dec2019.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

#####################
#   VISUALIZATION   #
#####################

#Creating Some Data
ref <- pred_scores[[2]]

#Models 1 and 2
model1 <- pred_scores[[4]]
#model2 <- ref+rnorm(30)

#Model Statement

#First Model
png("p1.png", width = 729, height = 501)
  Taylor(ref, model1, F, 1, 'red', T, " ")

  #Second Model
  #Taylor(ref, model2, T, 2, 'blue', T)

dev.off()

#Reading png back in to transform into a ggplot object
g <- magick::image_read('p1.png')
file.remove('p1.png')
p1 <- ggplotify::as.ggplot(g)
rm(g)

p1
ggplot2::ggsave("linear2_TaylorDiagram_23Sept2020.pdf")

"""

#############
#   NOTES   #
#############

#Getting Unicode Special Characters: https://appleinsider.com/articles/18/03/16/how-to-type-accented-letters-in-macos-three-different-ways

#Thread Example: https://github.com/StanJulia/Stan.jl/blob/master/Examples_Threads/threads_01.jl
#Plotting: https://github.com/JuliaPlots/StatsPlots.jl

#Plots with Reference Lines
pyplot()
df = DataFrame(a = 1:10, b = 10*rand(10), c = 10 * rand(10))
f = Plots.font("DejaVu Sans", 10)
@df df plot(:a, [:b :c], label=["fitted prices" "obs prices"], xtickfont=f, ytickfont=f, legendfont=f, guidefont=f, titlefont=f) # x = :a, y = [:b :c]. Notice this is two columns!
plot!([5], seriestype="vline")
annotate!(5, 0, text("\$\\bar \\delta \$",f, :left))
plot!([7], seriestype="vline")
annotate!(7, 0, text("\$\\bar \\gamma \$",f, :left))

#Custom Symbols
verts = [(-1.0, 1.0), (-1.28, 0.6), (-0.2, -1.4), (0.2, -1.4), (1.28, 0.6), (1.0, 1.0), (-1.0, 1.0), (-0.2, -0.6), (0.0, -0.2), (-0.4, 0.6), (1.28, 0.6), (0.2, -1.4), (-0.2, -1.4), (0.6, 0.2), (-0.2, 0.2), (0.0, -0.2), (0.2, 0.2), (-0.2, -0.6)]
x = 0.1:0.2:0.9
y = 0.7 * rand(5) .+ 0.15
plot(x, y, line = (3, :dash, :lightblue), marker = (Shape(verts), 30, RGBA(0, 0, 0, 0.2)), bg = :pink, fg = :darkblue, xlim = (0, 1), ylim = (0, 1), leg = false)

using Plots;
Random.seed!(100)
x = rand(TDist(5), 60)  # Create hypothetical dataset x
acf = StatsBase.autocor(x, 1:20)  # autocorrelation for lags 1:20
pacf = StatsBase.pacf(x, 1:20) # partial autocorrelation for lags 1:20
## Plotting using Plots.jl
plot(bar(acf, title = "Autocorrelation", legend = false), bar(pacf, title = "Partial autocorrelation", legend = false))

#Pretty: Classic Style Plot
using Dates
using Astro
using Colors
using Plots

days = Dates.datetime2julian.(Dates.DateTime(2018, 1, 1, 0, 0, 0):Dates.Day(1):Dates.DateTime(2018, 12, 31, 0, 0, 0))
eq_values = map(Astro.equation_time, days)

days = Dates.DateTime(2018, 1, 1, 0, 0, 0):Dates.Day(1):Dates.DateTime(2018, 12, 31, 0, 0, 0)
datestrings = Dates.format.(days, "u dd")
xticks = (1:14:366, datestrings[1:14:366])

plot(eq_values)

plot!(
    eq_values,

    label  = "equation of time (calculated)",
    line=(:black, 0.5, 6, :solid),

    size=(800, 600),

    xticks = (1:14:366, datestrings[1:14:366]),
    yticks = -20:2.5:20,

    ylabel = "Minutes faster or slower than GMT",
    xlabel = "day in year",

    title  = "The Equation of Time",
    xrotation = rad2deg(pi/3),

    fillrange = 0,
    fillalpha = 0.25,
    fillcolor = :lightgoldenrod,

    background_color = :ivory
    )

#Line Types
linetypes = [:path :steppre :steppost :sticks :scatter]
n = length(linetypes)
x = Vector[sort(rand(20)) for i = 1:n]
y = rand(20,n)
plot(x,y,line=(linetypes,3),lab=map(string,linetypes),ms=15)

#Delete by Index
equation_files = setdiff(equation_files, [2])

#EVALUATING VARIANCE COMPONENTS

cd("/Users/jonathan.h.morgan/dataplot/Dataplot_Tests")

lith_data = DataFrame!(CSV.File("Lithograph_Data.csv"))
lith_data.cassette = string.(lith_data.cassette)
lith_data.wafer = string.(lith_data.wafer)

lith_data

#Fitting Simple Mixed Effects Model to Isolate Variance Components
fm1 = fit!(LinearMixedModel(@formula(raw ~ 1 + (1|cassette/wafer)), lith_data))
