#GSII Helper Function
#Jonathan H. Morgan
#23 February 2020

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Home
setwd("~/Desktop/Group_Simulator") 
getwd()

#Home: Setting Output directory for writing CSVs
DATA.DIR <- ("~/Desktop")

################
#   PACKAGES   #
################

library(reticulate)
library(data.table)

#Pointing to the Python executable I am calling. 
#Verify in your Jupyter notebook your python file location using sys.executable in your notebook.

use_python("/anaconda3/bin/python", required = T)

#Checking that I sucessfullly pointed reticulate to Python
py_discover_config()

#If you get Null ENV error: Trying installing a new version of RStudio from: https://rstudio.com/products/rstudio/download/preview/

####################################
#   SPECIFYING AND RUNNING GS II   #
####################################

#CREATING VARIABLES

#Must Specify Networks in the Python script
network_n <- 4

ag_rate <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
rec_rate <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
sim_n <- 5

#CREATING AGENT AND MODEL DATA LISTS

#Agent Data List
agent_list <- vector('list', network_n)                   
for (i in seq_along(agent_list)){
  agent_list[[i]] <- vector('list', length(ag_rate))
  names(agent_list)[i] <- paste0('net_', i)
}
for (i in seq_along(agent_list)){
  for (j in 1:length(ag_rate)){
    agent_list[[i]][[j]] <- vector('list', sim_n)
  }
}
for (i in seq_along(agent_list)){
  for (j in 1:length(ag_rate)){
    for (k in 1:sim_n){
      names(agent_list[[i]])[j] <- paste0('rate_', ag_rate[[j]])
      names(agent_list[[i]][[j]])[k] <- paste0('iteration_', k)
    }
  }
}

#Model Data List
model_list <- vector('list', network_n)                   
for (i in seq_along(model_list)){
  model_list[[i]] <- vector('list', length(ag_rate))
  names(model_list)[i] <- paste0('net_', i)
}
for (i in seq_along(model_list)){
  for (j in 1:length(ag_rate)){
    model_list[[i]][[j]] <- vector('list', sim_n)
  }
}
for (i in seq_along(model_list)){
  for (j in 1:length(ag_rate)){
    for (k in 1:sim_n){
      names(model_list[[i]])[j] <- paste0('rate_', ag_rate[[j]])
      names(model_list[[i]][[j]])[k] <- paste0('iteration_', k)
    }
  }
}

#Looping through networks at different address-the-group rates
for (i in 1:network_n){
  for (j in 1:length(ag_rate)){
    for (k in 1:sim_n){
      
#Python is zero indexed and expects and integer. 
#So, we convert i into iter which i-1 and an interger.
    i_iter <- as.integer(i - 1)
    j_iter <- as.integer(i - 1)

    py_run_file("H_Net_Simulation.py")
      
    model_list[[i]][[j]][[k]] <- py$model_data
    colnames(model_list[[i]][[j]][[k]])[1] <- c('Step')
    model_list[[i]][[j]][[k]]$network <- i
    model_list[[i]][[j]][[k]]$rate <- ag_rate[[j]]
    model_list[[i]][[j]][[k]]$run <- k

    agent_list[[i]][[j]][[k]] <- py$agent_data
    agent_list[[i]][[j]][[k]]$network <- i
    agent_list[[i]][[j]][[k]]$rate <- ag_rate[[j]]
    agent_list[[i]][[j]][[k]]$run <- k
  }
 }
}

#######################
#   FORMATTING DATA   #
#######################

#Agent Data
for (i in 1:network_n){
  for (j in 1:length(ag_rate)){
    agent_list[[i]][[j]] <- data.table::rbindlist(agent_list[[i]][[j]])
  }
}

for (i in 1:network_n){
  agent_list[[i]] <- data.table::rbindlist(agent_list[[i]])
}

agent_list <- data.table::rbindlist(agent_list)
unique(agent_list$network)
unique(agent_list$rate)
unique(agent_list$run)

#Model Data
for (i in 1:network_n){
  for (j in 1:length(ag_rate)){
    model_list[[i]][[j]] <- data.table::rbindlist(model_list[[i]][[j]])
  }
}

for (i in 1:network_n){
  model_list[[i]] <- data.table::rbindlist(model_list[[i]])
}

model_list <- data.table::rbindlist(model_list)
