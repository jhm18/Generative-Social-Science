#Group Simulator Processing Script
#Jonathan H. Morgan
#5 December 2020

#Note: This script includes a long set of loops. 
#      Rather than trying to highlight the entire loop, I suggest using RStudio's script navigation functionality by collapsing the 
#      the script using the arrows by the beginning of the loop, right next to the script's row number.

#setwd("/Users/junzhao/Dropbox/Research/Dartmouth/Modeling Identity and Sentiments in Collaborative Groups/Group Simulator")
setwd("~/Desktop/Group_Simulator")
getwd()

################
#   PACKAGES   #
################
library(dplyr)        #Data Management, used for its count and filter functions because they are convenient.
library(ggplot)       #Supports Grammar of Graphics Visualizations (Used Here to Output Plots as High DPI PDFs)
library(ggfortify)    #Converts R graphics into ggplot objects.
library(readr)        #More efficiently handles CSV files than R's native libraries.

#################
#   FUNCTIONS   #
#################

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

######################
#   IMPORTING DATA   #
######################

#Reading in as a list all files with a .txt file designation.
GS_Files = list.files(pattern="*.txt", full.names = TRUE)

#Isolating the Group Simulator Action File of Interest
GS_Files <- GS_Files [-c(2,3,5)]

#Reading-In the contexts of the action file to parse.
GS_Simulations <- vector("list", length(GS_Files))
for (i in seq_along(GS_Simulations)){
  GS_Simulations[[i]] <- readLines(GS_Files[[i]])
}

#######################
#   FORMATTING DATA   #
#######################

#Creating a Vector of Variable Names to Use to Construct the Simulation Files
Names <- c('Run Number', 'Event Number', 'IPA Number', 'Behavior Evaluation', 'Behavior Potency', 'Behavor Activity', 'Deflection', 'Actor Reciprocated', 'Actor',
           'Actor Fundamental Evaluation', 'Actor Fundamental Potency', 'Actor Fundamental Activity', 'Actor Transient Evaluation', 'Actor Transient Potency',
           'Actor Transient Activity', 'Actor Emotion Evaluation', 'Actor Emotion Potency', 'Actor Emotion Activity', 'Actor Deflection', 
           'Acts Originated by the Actor', 'Acts Received by the Actor', 'Object', 'Object Fundamental Evaluation', 'Object Fundamental Potency', 
           'Object Fundamental Activity', 'Object Transient Evaluation', 'Object Transient Potency', 'Object Transient Activity', 'Object Emotion Evaluation',
           'Object Emotion Potency', 'Object Emotion Activity', 'Object Deflection')

#Creating an Empty List to Populate from the Groups Simulator action files I am analyzing. 
#This functionality allows me to read more than one Group Simulator action file at a time.
Simulations <- vector('list', length(GS_Files))
names(Simulations) <- GS_Files

#Creating an Empty List to Populate with the Parameter Specifications for each Simulation (the first two rows of each run)
Parameters <- vector('list', length(GS_Files))
names(Parameters) <- GS_Files

#Loop that Creates for Each Run in Each File a Dataframe of Simulation Results to Analyze--this may take a bit depending on how many simulations you ran.
for (i in seq_along(Simulations)) {
  lengths <- vector('numeric', length(GS_Simulations[[i]]))
  for (j in seq_along(lengths)) {
    lengths[[j]] <- nchar(GS_Simulations[[i]][[j]])
  }
 
  lengths_check <- ifelse(lengths >= 250, 1, 0)
  sim_iterations <- seq(1, length(GS_Simulations[[i]]), 1)
  sim_iterations <- as.data.frame(cbind(lengths_check, sim_iterations))
  
  parameters <- sim_iterations[sim_iterations[[1]] == 1, ]
  c_1 <- parameters[seq(1, nrow(parameters), 2), ][[2]]
  c_2 <- parameters[seq(2, nrow(parameters), 2), ][[2]]
  parameter_rows <- as.data.frame(cbind(c_1, c_2))
  
  parameter_list <- vector('list', nrow(parameter_rows))
  names(parameter_list) <- paste0('sim_', seq(1,length(parameter_list), 1))
  for (j in seq_along(parameter_list)){
    parameter_list[[j]] <- GS_Simulations[[i]][c(parameter_rows[j,1], parameter_rows[j,2])]
  }
  
  check_rows <- sim_iterations[sim_iterations[[1]] == 1, ]
  check_even <- check_rows[seq(2, nrow(check_rows), 2), ]
  c_1 <- check_even[[2]]
  c_1 <- c_1[-c(length(c_1))]
  c_2 <- check_even[[2]]
  c_2 <- c_2[-c(1)]
  check_rows <- as.data.frame(cbind(c_1, c_2))
  check_rows$c_1 <- check_rows$c_1 + 1 
  check_rows$c_2 <- check_rows$c_2 - 2
  check_rows <- rbind(check_rows, c((max(parameter_rows$c_2) + 1), length(GS_Simulations[[i]])))
  check_rows$sim_id <- as.numeric(row.names(check_rows))
  
  simulations_list <- vector('list', nrow(check_rows)) 
  for (j in seq_along(simulations_list)){
    sim_rows <- sim_iterations[c(check_rows[j,1]:check_rows[j,2]), ]
    sim_rows$sim_id <- check_rows[j,3]
    simulations_list[[j]] <- sim_rows
    rm(sim_rows)
  }
  simulations_list <- do.call("rbind", simulations_list)
  
  rows <- length(simulations_list$sim_iterations)
  columns <- length(Names)
  simulation_matrix <- as.data.frame(matrix(numeric(rows*columns), nrow = rows, ncol = columns))
  colnames(simulation_matrix) <- Names
  
  sim_iterations <- simulations_list$sim_iterations
  
  for (j in seq_along(sim_iterations)){
    element <- GS_Simulations[[i]][[sim_iterations[[j]]]]
    element_list <- strsplit(element, ",")[[1]]
    element_list[[4]] <- gsub("[[]", "", element_list[[4]])
    element_list[[4]] <- gsub("[]]", "", element_list[[4]])
    element_four <- strsplit(element_list[[4]], " ")[[1]][c(2:4)]
  
    element_list[[8]] <- gsub("[[]", "", element_list[[8]])
    element_list[[8]] <- gsub("[]]", "", element_list[[8]])
    element_eight <- strsplit(element_list[[8]], " ")[[1]][c(2:4)]
  
    element_list[[9]] <- gsub("[[]", "", element_list[[9]])
    element_list[[9]] <- gsub("[]]", "", element_list[[9]])
    element_nine <- strsplit(element_list[[9]], " ")[[1]][c(2:4)]
  
    element_list[[10]] <- gsub("[[]", "", element_list[[10]])
    element_list[[10]] <- gsub("[]]", "", element_list[[10]])
    element_ten <- strsplit(element_list[[10]], " ")[[1]][c(2:4)]
  
    element_list[[15]] <- gsub("[[]", "", element_list[[15]])
    element_list[[15]] <- gsub("[]]", "", element_list[[15]])
    element_fifteen <- strsplit(element_list[[15]], " ")[[1]][c(2:4)]
  
    element_list[[16]] <- gsub("[[]", "", element_list[[16]])
    element_list[[16]] <- gsub("[]]", "", element_list[[16]])
    element_sixteen <- strsplit(element_list[[16]], " ")[[1]][c(2:4)]
  
    element_list[[17]] <- gsub("[[]", "", element_list[[17]])
    element_list[[17]] <- gsub("[]]", "", element_list[[17]])
    element_seventeen <- strsplit(element_list[[17]], " ")[[1]][c(2:4)]
  
    #Populating Matrix
    simulation_matrix[j,1] <- as.numeric(element_list[[1]])
    simulation_matrix[j,2] <- as.numeric(element_list[[2]])
    simulation_matrix[j, 3] <- as.numeric(element_list[[3]])
    simulation_matrix[j, c(4,5,6)] <- as.numeric(element_four)
    simulation_matrix[j, 7] <- as.numeric(element_list[[5]])
    simulation_matrix[j, 8] <- trim(as.character(element_list[[6]]))
    simulation_matrix[j, 9] <- trim(as.character(element_list[[7]]))
    simulation_matrix[j, c(10,11,12)] <- as.numeric(element_eight)
    simulation_matrix[j, c(13,14,15)] <- as.numeric(element_nine)
    simulation_matrix[j, c(16,17,18)] <- as.numeric(element_ten)
    simulation_matrix[j, 19] <- as.numeric(element_list[[11]])
    simulation_matrix[j, 20] <- as.numeric(element_list[[12]])
    simulation_matrix[j, 21] <- as.numeric(element_list[[13]])
    simulation_matrix[j, 22] <- trim(as.character(element_list[[14]]))
    simulation_matrix[j, c(23, 24, 25)] <- as.numeric(element_fifteen)
    simulation_matrix[j, c(26, 27, 28)] <- as.numeric(element_sixteen)
    simulation_matrix[j, c(29, 30, 31)] <- as.numeric(element_seventeen)
    simulation_matrix[j, 32] <- as.numeric(element_list[[18]])
  
    rm(element, element_list, element_four, element_eight, element_nine, element_ten, element_fifteen, element_sixteen, element_seventeen)
  }
  
  simulation_matrix <- cbind(simulations_list$sim_id, simulation_matrix)
  colnames(simulation_matrix)[[1]] <- c('sim_id')
  
  Simulations[[i]] <- simulation_matrix
  Parameters[[i]] <- parameter_list
  
  rm(i, j, c_1, c_2, rows, columns, lengths, lengths_check, parameter_rows, parameters, check_even, check_rows, 
     sim_iterations, simulation_matrix, parameter_list, simulations_list)
}
  
#######################
#   EXAMINING FILES   #
#######################

#Outputting into a data set the results from: GSFixed_Example_actions.txt
#   Simulation 2 includes multiple experimental runs, indicated by the sim_id variable.
#   If you had multiple action files, you would have more Simulation files in your list.
Simulation_1 <- Simulations[[2]]

#Writing-to-CSV the Simulation data using R's readr package.
readr::write_csv(Simulation_1, 'Jury Replication_10Jan2029.csv')

#Add Space after each Simulation and Writing-out the parameter settings for each Simulation
for (i in seq_along(Parameters)) {
  for (j in seq_along(Parameters[[i]])) {
    Parameters[[i]][[j]] <- append(Parameters[[i]][[j]], c(" "))
  }
}

lapply(Parameters$`./GSFixed_Example_actions.txt`, write, file="/Users/jonathan.h.morgan/Desktop/sim_paramters.txt", sep='\r', append=T);

######################
#   VISUALIZATIONS   #
######################

#This is visualization compare IPA Proportions for each of the 11 Address-the-Group Rate Simulations found in GSFixed_Example_actions.txt

#Plotting Elements
colors <- c('blue', 'red')
lines <- c(1, 2)

ipa_plots <- vector('list', length(unique(Simulation_1$sim_id)))
names(ipa_plots) <- paste('Simulation', seq(1, length(ipa_plots), 1))

for(i in seq_along(ipa_plots)) {
  simulation <- Simulation_1[Simulation_1$sim_id == i, ]

  #Replicating Jury Study Visualization (Heise 2013)
  IPA <- group_by(simulation, `IPA Number`) %>%
    dplyr::count()

  #Dropping all actions where the group was the object for comparison: 90706 observations lost
  IPA_2 <- simulation %>%
    dplyr::filter(Object != '(whole-group 0)')

  IPA_2 <- group_by(IPA_2, `IPA Number`) %>%
    dplyr::count()

  IPA$Proportion <- IPA$n/sum(IPA$n)
  IPA_2$Proportion <- IPA_2$n/sum(IPA_2$n)
  
  y_axis <- pretty(IPA$Proportion)

  #Plotting
  png("p_1.png", width = 877, height = 676)
    layout(rbind(1,2), heights=c(9,1))  # put legend on bottom 1/8th of the chart
    par(bty="n", oma=c(1,1, 1, 1), mar=c(3.5,4,2,1))
    plot(0, type='n', xlim=c(1, 12), ylim=c(0, max(y_axis)), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
      axes=FALSE, bty='n')

    axis(1, at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12') ,
        cex.axis = 1.5, family = 'HersheySerif')
    mtext(side=1, "IPA Categories", at=6.5, line=2.5, family='HersheySerif', cex=2)

    axis(side=2, paste0((y_axis*100), '%'), at=y_axis, line=0.1, cex=2.5, las=1,family='HersheySerif')
    mtext(side=2, "Behavior Percentage", at=median(y_axis), line=3, family='HersheySerif', cex=2)

    #Adding Gridlines
    grid(lwd = 2)

    lines(IPA$`IPA Number`,IPA$Proportion, lwd=2, col='blue')
    points(IPA$`IPA Number`,IPA$Proportion, pch=21, bg='grey', col='blue', cex=1.3)

    lines(IPA_2$`IPA Number`,IPA_2$Proportion, lwd=2, lty=2, col='red')
    points(IPA_2$`IPA Number`,IPA_2$Proportion, pch=21, bg='grey', col='red', cex=1.3)
  
    title(names(ipa_plots)[[i]], family='HersheySerif', cex.main=3, line=0.25)
  
    #Adding Legend
    par(mar=c(0, 0, 0, 0))
    plot.new()
    legend('center','groups',c("All Observations", "Agent-to-Agent"), lty=lines, col=colors, lwd=2, ncol=4,  
          pch=21, pt.bg='grey', cex=1, bty ="n")
  dev.off()
  
  g <- magick::image_read('p_1.png')
  file.remove('p_1.png')
  
  ipa_plots[[i]] <- g
  rm(g, IPA, IPA_2, simulation, y_axis)
}

#Plotting all two 2x3 Panel Plots: Simulations 1-6 and Simulations 7-11
series_1 <- ipa_plots[1:6]
series_2 <- ipa_plots[7:11]

#Constructing Panel Plot: Series 1
png("p_1.png", width = 824, height = 531)
  layout.matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
  layout(mat = layout.matrix,
       heights = c(2, 2, 2), # Heights of the two rows
       widths = c(2, 2, 2)) # Widths of the two columns

  for (i in seq_along(series_1)){
    par(mar=c(0, 0, 0, 0))
    plot(series_1[[i]])
  }
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

p_1
ggplot2::ggsave("IPAPlots_Series1_8Dec2020.pdf", dpi=600, width = 7, height = 4.5)

#Constructing Panel Plot: Series 2
png("p_1.png", width = 824, height = 531)
  layout.matrix <- matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3)
  layout(mat = layout.matrix,
       heights = c(2, 2, 2), # Heights of the two rows
       widths = c(2, 2, 2)) # Widths of the two columns

  for (i in seq_along(series_2)){
    par(mar=c(0, 0, 0, 0))
    plot(series_2[[i]])
  }
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_2 <- ggplotify::as.ggplot(g)
rm(g)

p_2
ggplot2::ggsave("IPAPlots_Series2_8Dec2020.pdf", dpi=600, width = 7, height = 4.5)

