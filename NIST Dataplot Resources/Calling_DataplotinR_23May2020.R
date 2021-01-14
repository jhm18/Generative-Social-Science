#Running Dataplot from R
#Jonthan H. Morgan
#23 May 2020

#Note: Script Assumes that the DP Files are in the Working Directory

list.of.packages <- c('rstudioapi')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Clear Out Console Script
cat("\014")

#Setting Work Directory
#setwd("~/Desktop/Themis.Cog/Big_5/Data_Scripts")
#getwd()

#Getting DP Files from directory
DP_files = list.files(pattern="*.DP")
file_dir <- getwd()
DP_files <- lapply(DP_files, function(x) paste0(file_dir, "/",x))
DP <- DP_files[[i]]

#Setting Run Directory (Dataplot's Home Directory)
run_dir <- c("/Users/jonathan.h.morgan/bin")

#Activating Terminal
rstudioapi::terminalActivate()

Sys.sleep(10)

#Getting my terminal's unique ID to be sure I am sending commands to one terminal and not spawning a millions others.
terminal_list <- rstudioapi::terminalList()
termId <- terminal_list[[1]]

#Formulating Commands
command_list <- vector('list', 3)
  command_list[[1]] <- paste0("cd ", run_dir, "\n")
  command_list[[2]] <- c('./dataplot\n')
  command_list[[3]] <- paste0("CALL ", DP, "\n")

#Sending Commands to Terminal
for (i in seq_along(command_list)){
    rstudioapi::terminalSend(termId, command_list[[i]])
}
  
#Killing this terminal instance
#rstudioapi::terminalKill(termId)
