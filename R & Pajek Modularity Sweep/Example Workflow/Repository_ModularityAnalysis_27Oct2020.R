#Repository Modularity Analysis
#Jonathan H. Morgan
#27 October 2020

#Clear Out Console Script
cat("\014")

setwd("/Users/jonathan.h.morgan/Desktop/Themis.Cog/Big_5/Data_Scripts/Repository_Network")
getwd()

options(scipen=6)
options(stringsAsFactors = FALSE)
#options(device = "quartz")

#setHook(packageEvent("grDevices", "onLoad"),
#        function(...) grDevices::X11.options(width = 12, height = 10, xpos = 0, pointsize = 10))

#################
#   FUNCTIONS   #
#################

source("/Applications/Pajek64/Pajek R Tools/R_Pajek Functions_15July2019.R")
source('/Applications/Pajek64/Pajek R Tools/Resolution Analyses_26May2020.R')

##########################
#  MODULARITY ANALYSIS   #
##########################

Rep_Files = list.files(pattern="*.rep")
basic_res(Rep_Files[[1]])

#Size by Cluster Plot
`Basic Resolution`

#Reading in the partition files with read_clu
read_clu()

clu <- as.data.frame(Partition_1[-c(1)])

read_net(Net_Files[[1]])

#Running the Function
mod_proc(Net_Files[[1]], clu)

#Saving Files

#Plots
`Basic Resolution`
ggplot2::ggsave(`Basic Resolution`, file='Repo_Similarity_Basic Resolution.pdf',
                dpi=600, width = 8.5, height = 5)

`Convex Hull Optimality`
ggplot2::ggsave(`Convex Hull Optimality`,file='Repo_Similarity_CHAMPS.pdf',
                dpi=600, width = 8.5, height = 5)

`ARI Partition Consensus`
ggplot2::ggsave(`ARI Partition Consensus`, file='Repo_Similarity_ARI Con.pdf',
                dpi=600, width = 8.5, height = 5)

`Modularity Profile`
ggplot2::ggsave(`Modularity Profile`, file='Repo_Similarity_Modularity Profile.pdf',
                dpi=600, width = 8.5, height = 5)

#Datasets
save(Modularity, file = "Repo_Modularity_26Oct2020.Rda")

Optimal_Modularity <- `Optimal Modularity`
save(Optimal_Modularity, file = "Repo_OptimalModularity_26May2020.Rda")

ARI_Stats <- `Aggregate ARI Statistics`
save(ARI_Stats, file = "Repo_ARI_26May2020.Rda")

#Best Solution
#RepoSim_0.95_5