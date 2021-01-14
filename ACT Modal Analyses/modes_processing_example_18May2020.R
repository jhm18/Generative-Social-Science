#Process Modes User Script
#Jonathan H. Morgan
#18 May 2020

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Setting Working Directory
setwd("~/Desktop/ACT/Duke10/Scripts and Visualizations/")
getwd()

#Calling Function
source("~/Desktop/ACT/ACT R Functions/modes_processing_18May2020.R")

#Loading Dataset I am analyzing
load('duke10_usif_12May2020.Rda')

#Isolating my concept list. 
#Note:  The concept list is represented as separate object in the event that the analyst wants to examine only a subset of the concepts contained in the entire dataset.
concept <- sessions$Concept

#Creating a subset dataset containing two columns.
#Importantly, the function assumes that the first column of this dataset is the concept label, and the second lists the ratings associated with the labels.
concept_data <- sessions[c(52,53)]

#The name used to label the output dataset
name <- c('evaluation_modes')

#Running the Function
process_modes(concept, concept_data, name)

