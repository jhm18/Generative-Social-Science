#Running Pajek from R: Example Script for Mac
#Jonathan H. Morgan
#19 May 2020

#Note: I am demonstrating the harder case
#      If running on Windows, you dispense with the c_directory object and delete the wine64 command

#Install Packages Necessary for the Script if Missing
list.of.packages <- c('rstudioapi')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Setting Work Directory
setwd("~/Desktop/DNAC/Pendant_Paper/Data and Scripts/")
getwd()

#Getting MCR File from directory
mcr_files = list.files(pattern="*.MCR")
mcr <- mcr_files[[1]]

#Creating a Run Directory so the Function Can More Easily on Multiple Machines
run_dir <- c("/Applications/Pajek64")

#This step is only necessary if runnin on an Mac, where the file systems are different
#On a Windows Machine, you could simply just get the full path of the MCR files in the list.files statement.
c_dir <- c('C:\\users\\jonathan.h.morgan\\Desktop\\DNAC\\Pendant_Paper\\Data and Scripts\\')

#Because I put spaces in file names, I need to put the command in quotes which is what I am doing here.
file_command <- paste0(c_dir, mcr)
file_command <- shQuote(file_command)

#Starting Terminal
#You can do this with the system command as demonstrated at: http://vlado.fmf.uni-lj.si/pub/networks/pajek/howto/execute.htm
#But, R system commands are more problematic when working across operating systems, so I am using the rstudioapi.
#Note, I have found that python's sys module works better across operating systems than R's system commands.
rstudioapi::terminalActivate()

#Getting my terminal's unique ID to be sure I am sending commands to one terminal and not spawning a millions others.
terminal_list <- rstudioapi::terminalList()
termId <- terminal_list[[1]]

#Note: On Windows, you simply start command two with pajek "...2Core_PendantReduction_20April2020.MCR"
command_list <- vector('list', 2)
  command_list[[1]] <- paste0("cd ", run_dir, "\n")
  command_list[[2]] <- paste("wine64 pajek.exe", file_command, "\n")

#Sending Commands to Terminal
for (i in seq_along(command_list)){
  rstudioapi::terminalSend(termId, command_list[[i]])
}

#Killing this terminal instance
rstudioapi::terminalKill(termId)