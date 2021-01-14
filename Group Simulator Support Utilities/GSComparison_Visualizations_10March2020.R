#Group Simulator and Group Simulator Fixed Comparison
#Jonathan H. Morgan
#10 March 2020

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Work
#setwd('~/Desktop/GroupSimulator Paper/GroupSimulator_Processing/Group Simulator Batch Scripts')
#getwd()

#Home
setwd("~/Desktop/Group_Simulator/GroupSim_Comparisons") 
getwd()

#Home: Setting Output directory for writing CSVs
DATA.DIR <- ("/Users/jonathan.h.morgan/dataplot/Analysis_Scripts/GS")

################
#   PACKAGES   #
################

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggplotify)
library(magick)

#################
#   FUNCTIONS   #
#################

rotate <- function(x) t(apply(x, 2, rev))

#Work Utilities
#source("~/Desktop/FHP Micro Projects/R Plot Utilities_13Dec2019.R")
#source("~/Desktop/FHP Micro Projects/util.R")

#Plot Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_9Dec2019.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

######################
#   IMPORTING DATA   #
######################

load('GS_Actions_9March2020.Rda')
#load('GS_EStats_9March2020.Rda')
#load('GS_HStats_9March2020.Rda')

load('GSFixed_Actions_11March2020.Rdata')
#load('GSFixed_EStats_10March2020.Rda')
#load('GSFixed_HStats_10March2020.Rda')

#######################
#   FORMATTING DATA   #
#######################

#*******************   -Due to slight format changes between GS and GS_Fixed, small formatting corrections need to be made-   *********************#

#Creating Names Vector for Re-Ordering Later
c_names <- colnames(actions_data)
c_1 <- c(c_names[[1]], 'rate')
c_names <- c_names[-c(1)]
c_names <- c(c_1, c_names)
rm(c_1)

rate <- seq(0,1,0.1)

#Creating new Run Number
run_ids <- vector('list', 4)
names(run_ids) <- unique(actions_data$experiment)

for (i in seq_along(run_ids)){
  run_ids[[i]] <- vector('list', 11)
}

for (i in seq_along(run_ids)){
  for (j in seq_along(run_ids[[i]])){
    run_ids[[i]][[j]] <- vector('list', 200)
    names(run_ids[[i]])[j] <- j
  }
}

event_number <- seq(1, 500, 1)
for (i in seq_along(run_ids)){
  for (j in seq_along(run_ids[[i]])){
    for (k in seq_along(run_ids[[i]][[j]])){
      run_ids[[i]][[j]][[k]] <- as.data.frame(cbind(k, event_number)) 
      run_ids[[i]][[j]][[k]] <- cbind(names(run_ids[i]) , rate[[j]], run_ids[[i]][[j]][[k]], stringsAsFactors=FALSE)
      colnames( run_ids[[i]][[j]][[k]]) <- c('experiment', 'rate', 'Run Number', 'Event Number')
    }
  }
}

for (i in seq_along(run_ids)){
  for (j in seq_along(run_ids[[i]])){
    run_ids[[i]][[j]] <- do.call(rbind, run_ids[[i]][[j]])
  }
}

for (i in seq_along(run_ids)){
  run_ids[[i]] <- do.call(rbind, run_ids[[i]])
}

run_ids <- do.call(rbind, run_ids)
rownames(run_ids) <- seq(1, nrow(run_ids), 1)

#Eliminating Commas and Brackets
for (i in 1:ncol(actions_data)){
  actions_data[ ,i] <- gsub("[[]", "", actions_data[ ,i])
  actions_data[ ,i] <- gsub("[]]", "", actions_data[ ,i])
  actions_data[ ,i] <- gsub(",", "", actions_data[ ,i])
}

#Adding run_ids with actions_data to fix Run Number
run_ids <- run_ids[-c(1,3)]
actions_data <- actions_data[c(-2)]
actions_data <- cbind(run_ids, actions_data)

#Re-Ordering Columns to be in Cannonical Order
actions_data <- actions_data[ ,c_names]

#Writing-Out File to Deal with Types Issues (Just Fater :-( )
#Statistics
readr::write_csv(actions_data, file.path(DATA.DIR, "GSFixed_Actions_11March2020.csv"))
gsfixed_actions_data <- readr::read_csv('GSFixed_Actions_11March2020.csv')
save(gsfixed_actions_data, file='GSFixed_Actions_11March2020.Rdata')

#Adding rate to actions_data and gsfixed_actions_data
rate <- run_ids[[2]]
actions_data <- cbind(rate, actions_data)
actions_data <- actions_data[ ,c_names]

gsfixed_actions_data <- cbind(rate, gsfixed_actions_data)
gsfixed_actions_data <- gsfixed_actions_data[ ,c_names]

#Saving data with rate added
#save(gsfixed_actions_data, file='GSFixed_Actions_11March2020.Rdata')
#save(actions_data, file='GS_Actions_9March2020.Rda')

#***********************************************   -Creating Aggregate Datasets for Visualization-   *************************************************#

#CREATING CUMULATIVE STATISTICS DATA

#Egalitarian: Address-the-Group
e_gs_ag_event.deflection <- actions_data[actions_data$experiment == "GS_Egalitarian_AddressGroup_Comparisons", ]
e_gs_ag_event.deflection <-  e_gs_ag_event.deflection[c(1:3, 9)]
colnames(e_gs_ag_event.deflection) <- c("experiment", "Run_Number", "Event_Number", "gs_def")

e_gsf_ag_event.deflection <- gsfixed_actions_data[gsfixed_actions_data$experiment == "GS_Egalitarian_AddressGroup_Comparisons", ]
e_gsf_ag_event.deflection <-  e_gsf_ag_event.deflection[c(9)]
colnames(e_gsf_ag_event.deflection) <- c("gsf_def")

e_ag_event.deflection <- cbind(e_gs_ag_event.deflection, e_gsf_ag_event.deflection)
#e_ag_event.deflection <- e_ag_event.deflection[1:50, ]
readr::write_csv(e_ag_event.deflection, file.path(DATA.DIR, "e_ag_deflection.csv"))

#Hierarchical: Address-the-Group
h_gs_ag_event.deflection <- actions_data[actions_data$experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
h_gs_ag_event.deflection <-  h_gs_ag_event.deflection[c(1:3, 9)]
colnames(h_gs_ag_event.deflection) <- c("experiment", "Run_Number", "Event_Number", "gs_def")

h_gsf_ag_event.deflection <- gsfixed_actions_data[gsfixed_actions_data$experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
h_gsf_ag_event.deflection <-  h_gsf_ag_event.deflection[c(9)]
colnames(h_gsf_ag_event.deflection) <- c("gsf_def")

h_ag_event.deflection <- cbind(h_gs_ag_event.deflection, h_gsf_ag_event.deflection)
#h_ag_event.deflection <- h_ag_event.deflection[1:50, ]
readr::write_csv(h_ag_event.deflection, file.path(DATA.DIR, "h_ag_deflection.csv"))

#Egalitarian: Reciprocation
e_gs_rec_event.deflection <- actions_data[actions_data$experiment == "GS_Egalitarian_Reciprocity_Comparisons", ]
e_gs_rec_event.deflection <-  e_gs_rec_event.deflection[c(1:3, 9)]
colnames(e_gs_rec_event.deflection) <- c("experiment", "Run_Number", "Event_Number", "gs_def")

e_gsf_rec_event.deflection <- gsfixed_actions_data[gsfixed_actions_data$experiment == "GS_Egalitarian_Reciprocity_Comparisons", ]
e_gsf_rec_event.deflection <-  e_gsf_rec_event.deflection[c(9)]
colnames(e_gsf_rec_event.deflection) <- c("gsf_def")

e_rec_event.deflection <- cbind(e_gs_rec_event.deflection, e_gsf_rec_event.deflection)
#e_rec_event.deflection <- e_rec_event.deflection[1:50, ]
readr::write_csv(e_rec_event.deflection, file.path(DATA.DIR, "e_rec_deflection.csv"))

#Hierarchical: Reciprocation
h_gs_rec_event.deflection <- actions_data[actions_data$experiment == "GS_Hierarchical_Reciprocity_Comparisons", ]
h_gs_rec_event.deflection <-  h_gs_rec_event.deflection[c(1:3, 9)]
colnames(h_gs_rec_event.deflection) <- c("experiment", "Run_Number", "Event_Number", "gs_def")

h_gsf_rec_event.deflection <- gsfixed_actions_data[gsfixed_actions_data$experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
h_gsf_rec_event.deflection <-  h_gsf_rec_event.deflection[c(9)]
colnames(h_gsf_rec_event.deflection) <- c("gsf_def")

h_rec_event.deflection <- cbind(h_gs_rec_event.deflection, h_gsf_rec_event.deflection)
#h_rec_event.deflection <- h_rec_event.deflection[1:50, ]
readr::write_csv(h_rec_event.deflection, file.path(DATA.DIR, "h_rec_deflection.csv"))

rm(e_gs_ag_event.deflection, e_gsf_ag_event.deflection, e_ag_event.deflection, h_gs_ag_event.deflection, h_gsf_ag_event.deflection,
   h_ag_event.deflection, e_gs_rec_event.deflection, e_gsf_rec_event.deflection, e_rec_event.deflection, h_gs_rec_event.deflection,
   h_gsf_rec_event.deflection, h_rec_event.deflection)

#CREATING BLOCK PLOT DATA (Refernces to Dataplot Example)
  #Y = Event Deflection
  #Proc =  Egalitarian & Hierarhical
  #Plant = Experiment: Address-the-Group & Reciprocity Rate
  #Speed = Rates

#Subsetting Data
gsf_block_data <- gsfixed_actions_data[c(1, 2, 9)]

#Splitting Out Group Type from Experiment
codes <- strsplit(gsf_block_data$experiment, "_")
for (i in seq_along(codes)){
  codes[[i]] <- strsplit(codes[[i]], " ")
}

group_type <- vector('character', length(codes))
for (i in seq_along(group_type)){
  group_type[[i]] <- unlist(codes[[i]][2])
}

sim_type <- vector('character', length(codes))
for (i in seq_along(sim_type)){
  sim_type[[i]] <- unlist(codes[[i]][3])
}

gsf_block_data$group_type <- group_type
gsf_block_data$sim_type <- sim_type

gsf_block_data$group_id <- ifelse(group_type == "Egalitarian", 1, 2)
gsf_block_data$sim_id <- ifelse(sim_type == "AddressGroup", 1, 2)
  
gsf_block_data <- gsf_block_data[c(7, 6, 2, 3)]

gsf_block_data <- dplyr::group_by(gsf_block_data, sim_id, group_id, rate) %>% 
  dplyr::summarise(mean_event_deflection = mean(Deflection)) 

gsf_block_data <- gsf_block_data[c(4,2,1,3)]

#Writing Out gsf_bock_data
readr::write_csv(gsf_block_data, file.path(DATA.DIR, "gsf_block_data.csv"))

rm(codes, sim_type, group_type)

#CREATING IPA HEATMAP AND STACKED BAR CHART DATA
ipa_gs <- dplyr::group_by(actions_data, experiment, rate, `IPA Number`) %>% 
              dplyr::summarise(Frequency = n()) 
ipa_gs$Simulator <- c("GS")

ipa_gsfixed <- dplyr::group_by(gsfixed_actions_data, experiment, rate, `IPA Number`) %>% 
                   dplyr::summarise(Frequency = n()) 
ipa_gsfixed$Simulator <- c("GSF")

ipa <- rbind(ipa_gs, ipa_gsfixed)

#Creating Higher Level Bales Categories
  #Task Behaviors (4-9)
  #Active Task Behaviors: 4-6
  #Passive Task Behaviors: 7-9

ipa_category <- vector('numeric', nrow(ipa))
for (i in seq_along(ipa_category)){
  ipa_category[[i]] <- if(ipa[[3]][[i]] >= 4 & ipa[[3]][[i]] <= 6) {
                     ipa_category[[i]] = "Active Task"
                    }else if(ipa[[3]][[i]] >= 7 & ipa[[3]][[i]] <= 9) {
                          ipa_category[[i]] = "Passive Task"
                      }else if(ipa[[3]][[i]] >= 10 & ipa[[3]][[i]] <= 12) {
                          ipa_category[[i]] = "Negative Socio-Emotive"
                      }else {
                          ipa_category[[i]] = "Positive Socio-Emotive"
                      }
}

ipa$IPA_Category <- ipa_category
rm(ipa_category)

#Aggregate Counts across IPA Cagegories
ipa <- dplyr::group_by(ipa, Simulator, experiment, rate, IPA_Category) %>% 
  dplyr::summarise(IPA_Sum = sum(Frequency)) 

ipa_total <- dplyr::group_by(ipa, Simulator, experiment, rate) %>% 
  dplyr::summarise(IPA_Total = sum(IPA_Sum)) 

ipa <-   ipa <- ipa %>%
  left_join(ipa_total, by=c("Simulator", "experiment", "rate"))

#Calculating the Proportion of each IPA Category
ipa$proportion <- ipa[[5]]/ipa[[6]]

#Transforming IPA_Category into a Factor Variable to Ensure Order
ipa$IPA_Category <- factor(ipa$IPA_Category, levels=c("Positive Socio-Emotive", "Active Task", 
                                                      "Passive Task","Negative Socio-Emotive"), ordered=TRUE)

#CREATING IDENTITY TRACES DATA

#GS Data Used for both Actor and Role Trace Analyses
gs_agent_trace <- actions_data[c(1, 2, 3, 11, 21, 24, 34)]
gs_agent_trace$Simulator <- c("GS")

#GSF Data Used for both Actor and Role Trace Analyses
gsf_agent_trace <- gsfixed_actions_data[c(1, 2, 3, 11, 21, 24, 34)]
gsf_agent_trace$Simulator <- c("GSF")

#Stacking Actor and Object Data for the Purposes of Visualizing the Actor Traces

#Group Simulator
gs_agent_trace_actor <-  gs_agent_trace[c(8, 1:5)]
colnames(gs_agent_trace_actor) <- c("Simulator", "Experiment", "Rate", "Run Number", "Agent", "Deflection")

gs_agent_trace_object <- gs_agent_trace[c(8, 1:3, 6:7)]
colnames(gs_agent_trace_object) <- c("Simulator", "Experiment", "Rate", "Run Number", "Agent", "Deflection")

gs_agent_trace_stacked <- rbind(gs_agent_trace_actor, gs_agent_trace_object)
rm(gs_agent_trace_actor, gs_agent_trace_object)

#Group Simulator Fixed
gsf_agent_trace_actor <-  gsf_agent_trace[c(8, 1:5)]
colnames(gsf_agent_trace_actor) <- c("Simulator", "Experiment", "Rate", "Run Number", "Agent", "Deflection")

gsf_agent_trace_object <- gsf_agent_trace[c(8, 1:3, 6:7)]
colnames(gsf_agent_trace_object) <- c("Simulator", "Experiment", "Rate", "Run Number", "Agent", "Deflection")

gsf_agent_trace_stacked <- rbind(gsf_agent_trace_actor, gsf_agent_trace_object)
rm(gsf_agent_trace_actor, gsf_agent_trace_object)

#Stacking the Agent Trace Data
agent_trace <- rbind(gs_agent_trace_stacked, gsf_agent_trace_stacked)

#Aggregate Deflection by Simulator, 
agent_trace <- dplyr::group_by(agent_trace, Simulator, Experiment, Rate, Agent) %>% 
  dplyr::summarise(Deflection_Median = median(Deflection)) 

#Pulling Out the Group Agent to Visualize Separately 
group_deflection <- agent_trace[agent_trace$Agent == "(whole-group)", ]

#Dropping the Group Agent for the Agent Trace Visualize
agent_trace <- agent_trace[agent_trace$Agent != "(whole-group)", ]

#Creating Role Labels
agent_trace$group_type = sapply(strsplit(agent_trace$Experiment, "_"), function(x) x[2])

agent_trace$role_label = ifelse(agent_trace$group_type == "Hierarchical" & agent_trace$Agent == "(male_1)", "Boss", "Client")
agent_trace$role_label = ifelse(agent_trace$group_type == "Egalitarian", "Man", agent_trace$role_label)

######################
#   VISUALIZATIONS   #
######################

#ANDREWS PLOT

#CUMULATIVE STATISTICS PLOT: Plots made in Dataplot currently
g_1 <- magick::image_read('GS_E_AG_Cumulative_Statistics.png')
g_2 <- magick::image_read('GSF_E_AG_Cumulative_Statistics.png')
p_1 <- ggplotify::as.ggplot(g_1)
p_2 <- ggplotify::as.ggplot(g_2)
rm(g_1, g_2)

pdf("Event_Deflection_12March2020.pdf", width = 8.5, height = 5)
  lay <- rbind(c(1, 2))
  grid.arrange(grobs = list(p_1, p_2), layout_matrix = lay)
dev.off()
  
#IPA HEAT MAP VISUALIZATION
#   1x2 Matrix
#   2 Heat Maps: Egalitarian and Hierarchical
#   Heat Map Panels by Simulatons

#Index Elements
rate <- seq(0,1, 0.1)
i_index <- unique(ipa$Simulator)
j_index <- unique(ipa$experiment)

#Plotting Elements
col <- hcl.colors(12, "YlOrRd", rev = TRUE)
titles <- c("GS Egalitarian", 'GS Hierarchical', 'GS Fixed Egalitarian', 'GS Fixed Hierarchical')

heatmap_list <- vector('list', length(i_index))
names(heatmap_list) <- i_index
for (i in seq_along(i_index)){
  heatmap_list[[i]] <- vector('list', length(j_index))
}
for (i in seq_along(i_index)){
  for (j in seq_along(j_index)){
    names(heatmap_list[[i]]) <- j_index
  }
}

for (i in seq_along(heatmap_list)){
  for (j in seq_along(heatmap_list[[i]])){
    sim <- ipa[ipa$Simulator==i_index[[i]] & ipa$experiment == j_index[[j]], ]

    #Creating a Visualization Matrix
    value <- sim[sim$rate == rate[[1]], ]
    value$rate <- as.factor(value$rate)
    value$`IPA Number` <- as.factor(value$`IPA Number`)
    value$Proportion <- value$Frequency/(sum(value$Frequency))
    freq_0 <- as.data.frame(value[c(3,1,5:6)])
    colnames(freq_0)[[4]] <- rate[[1]]
    rownames(freq_0) <- value$`IPA Number`
    rm(value)

    for (k in 2:length(rate)){
      value <- sim[sim$rate == rate[[k]], ]
      value$rate <- as.factor(value$rate)
      value$`IPA Number` <- as.factor(value$`IPA Number`)
      value$Proportion <- value$Frequency/(sum(value$Frequency))
      freq <- as.data.frame(value[c(3,1,5:6)])
      colnames(freq)[[4]] <- rate[[k]]
      rownames(freq) <- value$`IPA Number`
  
      freq_0 <- freq_0 %>%
        join(freq, by=c("experiment", "Simulator", "IPA Number"))
      freq_0[is.na(freq_0)] <- 0
  
      rm(value, freq)
    }

    heatmap_list[[i]][[j]] <- freq_0
    rm(freq_0)
  }
}

#Reorder the Lists By Type of Simulation
ag_heatmap <- vector('list', 4)
  ag_heatmap[[1]] <- heatmap_list[[1]][[1]]
  ag_heatmap[[2]] <- heatmap_list[[1]][[3]]
  ag_heatmap[[3]] <- heatmap_list[[2]][[1]]
  ag_heatmap[[4]] <- heatmap_list[[2]][[3]]

rec_heatmap <- vector('list', 4)
  rec_heatmap[[1]] <- heatmap_list[[1]][[2]]
  rec_heatmap[[2]] <- heatmap_list[[1]][[4]]
  rec_heatmap[[3]] <- heatmap_list[[2]][[2]]
  rec_heatmap[[4]] <- heatmap_list[[2]][[4]]
  
for (i in seq_along(ag_heatmap)){
  ag_heatmap[[i]] <- ag_heatmap[[i]][c(4:14)]
  ag_heatmap[[i]] <- as.matrix(ag_heatmap[[i]])
}
    
for (i in seq_along(ag_heatmap)){
  rec_heatmap[[i]] <- rec_heatmap[[i]][c(4:14)]
  rec_heatmap[[i]] <- as.matrix(rec_heatmap[[i]])
}
  
png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2)
  layout(mat = layout.matrix,
       heights = c(2, 2, 0.5), # Heights of the two rows
       widths = c(2, 2, 0.5)) # Widths of the two columns
  #layout.show(6)

  #Plotting: Address-the-group and reciprocity heatmaps generated separately.
  for (i in seq_along(rec_heatmap)){
      par(mar=c(3, 4.1, 4.1, 2.1), family='HersheySerif')
      plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
          axes=FALSE, bty='n')

      x_axis <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      y_axis <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
      axis(1, at=x_axis, lab=c('0', '0.1', '0.2', '0.3', '0.4', '0.5', '0.6', '0.7', '0.8',
                            '0.9', '1'), cex.axis = 1.3, family = 'HersheySerif')
      axis(2, at=y_axis, las=1 ,lab=rev(y_axis), cex.axis = 1.3, family = 'HersheySerif')

      #Adding Axese Labels
      mtext(side = 1, text = 'Rate', at = 6, col = "grey20", line = 2.5, cex = 1.3, family='HersheySerif')
      mtext(side = 2, text = 'IPA Categories', at = 6, col = "grey20", line = 2.3, cex = 1.3, family='HersheySerif')

      #Adding Title
      title(titles[[i]], family='HersheySerif', cex.main=1.9, line=0.85)

      #Adding Heat Map
      par(new=TRUE)
      image(rotate(rec_heatmap[[i]]), useRaster = TRUE, col=col, axes= FALSE)
  }

  #Add Legends
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot.new()
  legend('center','groups', c('Positive Socio-Emotive (IPA 1-3)', 'Active Task (IPA 4-6)',
                               'Passive Task (6-9)', 'Negative Socio-Emotive'), ncol=2,
                               cex=1, bty ="n")

  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
     axes=FALSE, bty='n')

  text(x= 5.5, y=7.5, labels = c("Percentage"))
  legend_image <- rotate(as.raster(matrix(rev(col)), ncol=1))
  rasterImage(legend_image, 3.1, 3.5,  9.1, 6)
  text(x=seq(3.1,9.1,l=5), y= 2.5, labels = paste0((seq(0,0.3,l=5)*100), "%"), cex=0.7)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("AG_HeatMaps_16March2020.pdf", 
                dpi=600, width = 8.5, height = 5)

ggplot2::ggsave("RC_HeatMaps_16March2020.pdf", 
                dpi=600, width = 8.5, height = 5)

#IPA Stacked Bar Chart 

ipa <- ipa[ipa$Simulator == "GSF", ]

ipa$proportion<- round(ipa$proportion, 2L)

#Creating List for Each Experiment
ipa_list <- vector('list', length(unique(ipa$experiment)))
experiments <- unique(ipa$experiment)
for (i in 1:length(ipa_list)){
  ipa_list[[i]] <- ipa[ipa$experiment == experiments[[i]], ]
}

names(ipa_list) <- experiments
rm(experiments)
ipa_list <- ipa_list[c(1, 3, 2, 4)]

#Plot Elements
rate <- seq(0,1, 0.1)
col_names <- c('0%', '10%', '20%', '30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%')
titles <- c("Egalitarian Groups", "Hierarchical Groups", "Egalitarian Groups", "Hierarchical Groups")
axis_list <- c('Action-on-Group Rate', 'Action-on-Group Rate', 'Reciprocity Rate', 'Reciprocity Rate')

for (i in 1:length(ipa_list)){
  value <- ipa_list[[i]][ipa_list[[i]]$rate == rate[[1]], ]
  freq_0 <- as.data.frame(value[c(4,7)])
  colnames(freq_0)[[2]] <- col_names[[1]]
  rownames(freq_0) <- value$IPA_Category
  rm(value)

  for (j in 2:length(rate)){
    value <- ipa_list[[i]][ipa_list[[i]]$rate == rate[[j]], ]
    freq <- as.data.frame(value[c(4,7)])
    colnames(freq)[[2]] <- col_names[[j]]
    rownames(freq) <- value$IPA_Category
  
    freq_0 <- freq_0 %>%
      join(freq, by=c("IPA_Category"))
    freq_0[is.na(freq_0)] <- 0
  
    rm(value, freq)
  }

  freq_0 <- freq_0[order(freq_0$IPA_Category),]
  Viz_Mat <- as.matrix(freq_0[-c(1)])
  rownames(Viz_Mat) <- as.character(freq_0$IPA_Category)

  ipa_list[[i]] <- Viz_Mat

  rm(freq_0, Viz_Mat)
}

#Plotting
png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2)
  layout(mat = layout.matrix,
       heights = c(2, 2, 0.5), # Heights of the two rows
       widths = c(2, 2, 0.5)) # Widths of the two columns
  #layout.show(6)

  for (i in 1:length(ipa_list)){
    par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(4,4.5,4,3.5), mgp = c(2.5, 0.8, 0.1))
    x <- barplot(ipa_list[[i]], col=c("brown", "blue", "dodgerblue", "grey"), density=c(30, 40, 30, 40), las = 1, 
             axes=FALSE, family = 'HersheySerif', xlab=' ', ylab=' ')

    at1 <- c(0.70, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5, 12.7)
    at2 <- c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)
    axis(1, at1, labels = F , cex.axis = 3, family = 'HersheySerif')
    axis(2, at2, labels = c('0%', '20%', '40%', '60%', '80%', '100%') , cex.axis = 1, family = 'HersheySerif', las=1)

    mtext(side=1, line = 2.5, cex=1.5, axis_list[i], family = 'HersheySerif')
    mtext(side=2, line = 2.9, cex=1.5, 'IPA Percentage', family = 'HersheySerif')

    #Adding Panel Titles
    mtext(side=3, titles[[i]], line=1.3, family='HersheySerif', font=1,  cex=1.5)
    
    #p <- par('usr')
    #text(p[2], mean(p[3:4]), labels = sim_title[[i]], family='HersheySerif', cex=2.1, font=4, xpd = NA, srt = -90)
  }
  
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  
  legend('center','groups', legend=c('Positive Socio-Emotive', 'Active Task', 'Passive Task', 'Negative Socio-Emotive'), cex=1.5, 
         fill=c("brown", "blue", "dodgerblue", "grey"), density=c(30, 40, 30, 40), bty='n', ncol=2)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("IPA_BarChart_15March2020.pdf", 
                dpi=600, width = 8.5, height = 5)


#AGENT TRACES

#Plot Elements
zero <- 0
titles <- c("Egalitarian Groups", "Hierarchical Groups", "Egalitarian Groups", "Hierarchical Groups")
axis_list <- c('Action-on-Group Rate', 'Action-on-Group Rate', 'Reciprocity Rate', 'Reciprocity Rate')

#GS Simulations
gs_agent_trace <- agent_trace[agent_trace$Simulator == "GS", ]

#GSF Simulations
gsf_agent_trace <- agent_trace[agent_trace$Simulator == "GSF", ]

#Agent Deflections
#agent_medians <- gs_agent_trace
agent_medians <- gsf_agent_trace

agent_deflection <- vector('list', 4)
  agent_deflection[[1]] <- agent_medians[agent_medians$Experiment == "GS_Egalitarian_AddressGroup_Comparisons", ]
  agent_deflection[[2]] <- agent_medians[agent_medians$Experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
  agent_deflection[[3]] <- agent_medians[agent_medians$Experiment == "GS_Egalitarian_Reciprocity_Comparisons", ]
  agent_deflection[[4]] <- agent_medians[agent_medians$Experiment == "GS_Hierarchical_Reciprocity_Comparisons", ]

print(max(agent_deflection[[1]]$Deflection_Median))
print(max(agent_deflection[[2]]$Deflection_Median))
print(max(agent_deflection[[3]]$Deflection_Median))

png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2)
  layout(mat = layout.matrix,
       heights = c(2, 2, 0.5), # Heights of the two rows
       widths = c(2, 2, 0.5)) # Widths of the two columns
  #layout.show(6)

  for (i in seq_along(agent_deflection)){
    par(mar=c(4.5, 7, 4.1, 2.1), bty='n', family='HersheySerif')
    plot(0, type='n', xlim=c(0, 1), ylim=c(0,3), xlab=' ', ylab= ' ', 
       cex=1, cex.lab=1, main=" ",  family = 'HersheySerif', bty='n', axes= FALSE)
  
  #Adding Gridlines
  grid(lwd = 2)
  
  x_axis <- c(0, 0.25, 0.5, 0.75, 1)
  y_axis <- c(0, 0.5, 1, 1.5, 2, 2.5, 3)
  axis(1, at=x_axis, lab=c('0%', '25%', '50%', '75%', '100%'), cex.axis = 1.3, family = 'serif')
  axis(2, at=y_axis, lab=y_axis, cex.axis = 1.3, family = 'HersheySerif', las= 1)
  mtext(side = 1, line = 2.9, cex=1.3, axis_list[[i]], family = 'HersheySerif')
  mtext(side = 2, line = 2.9, cex=1.3, 'Median Deflection', family = 'HersheySerif')
  
  #Adding Agent Median Deflection
  colors <- c('brown', 'black', 'blue')
  for (j in seq_along(unique(agent_deflection[[i]][[4]]))){
    agents <- unique(agent_deflection[[i]][[4]])
    median <- agent_deflection[[i]][agent_deflection[[i]][[4]] == agents[[j]], ] 
    lines(median[[3]], median[[5]], col=colors[j], lwd=3) 
    rm(agents, median)
  }
  
  #Adding Title
  title(titles[[i]], family='HersheySerif', cex.main=1.9, line=0.5)
  }

  #Add Legends
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  legend('center','groups', legend=c('Man 1', 'Man 2', 'Man 3'), cex=1.5, col=colors, lwd=2, bty='n', ncol=3)
  
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  legend('center','groups', legend=c('Client 1', 'Client 2', 'Boss'), cex=1.5, col=colors, lwd=2, bty='n', ncol=3)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

#ggplot2::ggsave("GS_Median Agent Deflection_16March2020.pdf", 
#                dpi=600, width = 8.5, height = 5)

ggplot2::ggsave("GSF_Median Agent Deflection_16March2020.pdf", 
                dpi=600, width = 8.5, height = 5)

#GROUP DEFLECTION
max(group_deflection$Deflection_Median)
experiments <- unique(group_deflection$Experiment)

png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2)
  layout(mat = layout.matrix,
       heights = c(2, 2, 0.5), # Heights of the two rows
       widths = c(2, 2, 0.5)) # Widths of the two columns
  #layout.show(6)

  for (i in seq_along(experiments)){
    median <- group_deflection[group_deflection$Experiment == experiments[[i]], ] 

    par(mar=c(4.5, 7, 4.1, 2.1), bty='n', family='HersheySerif')
    plot(0, type='n', xlim=c(0, 1), ylim=c(0,8), xlab=' ', ylab= ' ', 
      cex=1, cex.lab=1, main=" ",  family = 'HersheySerif', bty='n', axes= FALSE)

    #Adding Gridlines
    grid(lwd = 2)

    x_axis <- c(0, 0.25, 0.5, 0.75, 1)
    y_axis <- c(0, 2, 4, 6, 8)
    axis(1, at=x_axis, lab=c('0%', '25%', '50%', '75%', '100%'), cex.axis = 1.3, family = 'serif')
    axis(2, at=y_axis, lab=y_axis, cex.axis = 1.3, family = 'HersheySerif', las= 1)
    mtext(side = 1, line = 2.9, cex=1.3, axis_list[[i]], family = 'HersheySerif')
    mtext(side = 2, line = 2.9, cex=1.3, 'Median Deflection', family = 'HersheySerif')

    #Adding Agent Median Deflection
    lines(median[median$Simulator == "GS", ][[3]] ,median[median$Simulator == "GS", ][[5]] , col='black', lwd=3) 
    lines(median[median$Simulator == "GSF", ][[3]] ,median[median$Simulator == "GSF", ][[5]] , col='brown', lwd=3) 

    #Adding Title
    title(titles[[i]], family='HersheySerif', cex.main=1.9, line=0.5)
  }

  colors <- c('black', 'brown')
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  legend('center','groups', legend=c('Group Simulator', 'Group Simulator (Fixed)'), cex=1.5, 
         col=colors, lwd=2, bty='n', ncol=1)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("Median Group Deflection_16March2020.pdf", 
                dpi=600, width = 8.5, height = 5)

#ACTOR & OBJECT PERSON TRACES

#GS Data Used for both Actor and Role Trace Analyses
gs_agent_trace <- actions_data[c(1, 2, 3, 11, 21, 24, 34)]
gs_agent_trace$Simulator <- c("GS")

#GSF Data Used for both Actor and Role Trace Analyses
gsf_agent_trace <- gsfixed_actions_data[c(1, 2, 3, 11, 21, 24, 34)]
gsf_agent_trace$Simulator <- c("GSF")

#GS: Aggregate Deflection for Actors and Object-Persons 
gs_actor_trace <- dplyr::group_by(gs_agent_trace, Simulator, experiment, rate, Actor) %>% 
  dplyr::summarise(Deflection_Median = median(`Actor Deflection`)) 

gs_object_trace <- dplyr::group_by(gs_agent_trace, Simulator, experiment, rate, Object) %>% 
  dplyr::summarise(Deflection_Median = median(`Object Deflection`)) 

#Dropping the Group Agent for the Agent Trace Visualize
gs_object_trace <- gs_object_trace[gs_object_trace$Object != "(whole-group)", ]

#GS: Aggregate Deflection for Actors and Object-Persons 
gsf_actor_trace <- dplyr::group_by(gsf_agent_trace, Simulator, experiment, rate, Actor) %>% 
  dplyr::summarise(Deflection_Median = median(`Actor Deflection`)) 

gsf_object_trace <- dplyr::group_by(gsf_agent_trace, Simulator, experiment, rate, Object) %>% 
  dplyr::summarise(Deflection_Median = median(`Object Deflection`)) 

#Dropping the Group Agent for the Agent Trace Visualize
gsf_object_trace <- gsf_object_trace[gsf_object_trace$Object != "(whole-group)", ]


#Plot Elements
titles <- c("Egalitarian Groups", "Hierarchical Groups", "Egalitarian Groups", "Hierarchical Groups")
axis_list <- c('Action-on-Group Rate', 'Action-on-Group Rate', 'Reciprocity Rate', 'Reciprocity Rate')

#Agent Deflections
#actor_medians <- gs_actor_trace
#object_medians <- gs_object_trace
actor_medians <- gsf_actor_trace
object_medians <- gsf_object_trace

actor_deflection <- vector('list', 4)
  actor_deflection[[1]] <- actor_medians[actor_medians$experiment == "GS_Egalitarian_AddressGroup_Comparisons", ]
  actor_deflection[[2]] <- actor_medians[actor_medians$experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
  actor_deflection[[3]] <- actor_medians[actor_medians$experiment == "GS_Egalitarian_Reciprocity_Comparisons", ]
  actor_deflection[[4]] <- actor_medians[actor_medians$experiment == "GS_Hierarchical_Reciprocity_Comparisons", ]

object_deflection <- vector('list', 4)
  object_deflection[[1]] <- object_medians[object_medians$experiment == "GS_Egalitarian_AddressGroup_Comparisons", ]
  object_deflection[[2]] <- object_medians[object_medians$experiment == "GS_Hierarchical_AddressGroup_Comparisons", ]
  object_deflection[[3]] <- object_medians[object_medians$experiment == "GS_Egalitarian_Reciprocity_Comparisons", ]
  object_deflection[[4]] <- object_medians[object_medians$experiment == "GS_Hierarchical_Reciprocity_Comparisons", ]
  
  print(max(object_deflection[[1]]$Deflection_Median))
  print(max(object_deflection[[2]]$Deflection_Median))
  print(max(object_deflection[[3]]$Deflection_Median))
  
png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 5, 2, 4, 6), nrow = 3, ncol = 2)
  layout(mat = layout.matrix,
         heights = c(2, 2, 0.5), # Heights of the two rows
         widths = c(2, 2, 0.5)) # Widths of the two columns
  #layout.show(6)
  
  for (i in seq_along(actor_deflection)){
    par(mar=c(4.5, 7, 4.1, 2.1), bty='n', family='HersheySerif')
    plot(0, type='n', xlim=c(0, 1), ylim=c(0,5), xlab=' ', ylab= ' ', 
         cex=1, cex.lab=1, main=" ",  family = 'HersheySerif', bty='n', axes= FALSE)
    
    #Adding Gridlines
    grid(lwd = 2)
    
    x_axis <- c(0, 0.25, 0.5, 0.75, 1)
    y_axis <- c(0, 1, 2, 3, 4, 5)
    axis(1, at=x_axis, lab=c('0%', '25%', '50%', '75%', '100%'), cex.axis = 1.3, family = 'serif')
    axis(2, at=y_axis, lab=y_axis, cex.axis = 1.3, family = 'HersheySerif', las= 1)
    mtext(side = 1, line = 2.9, cex=1.3, axis_list[[i]], family = 'HersheySerif')
    mtext(side = 2, line = 2.9, cex=1.3, 'Median Deflection', family = 'HersheySerif')
    
    #Adding Actor Median Deflection
    colors <- c('brown', 'black', 'blue')
    for (j in seq_along(unique(actor_deflection[[i]][[4]]))){
      actors <- unique(actor_deflection[[i]][[4]])
      median <- actor_deflection[[i]][actor_deflection[[i]][[4]] == actors[[j]], ] 
      lines(median[[3]], median[[5]], col=colors[j], lwd=3) 
      rm(actors, median)
    }
    
    #Adding Actor Median Deflection
    colors <- c('brown', 'black', 'blue')
    for (j in seq_along(unique(object_deflection[[i]][[4]]))){
      objects <- unique(object_deflection[[i]][[4]])
      median <- object_deflection[[i]][object_deflection[[i]][[4]] == objects[[j]], ] 
      lines(median[[3]], median[[5]], col=colors[j], lwd=3, lty=2) 
      rm(objects, median)
    }
    
    #Adding Title
    title(titles[[i]], family='HersheySerif', cex.main=1.9, line=0.5)
  }
  
  #Add Legends
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  colors = c('white', 'brown', 'black', 'blue')
  legend('top','groups', legend=c('Actors:', 'Man 1', 'Man 2', 'Man 3'), cex=1.5, col=colors, lwd=2, bty='n', ncol=4)
  legend('bottom','groups', legend=c('Objects:', 'Man 1', 'Man 2', 'Man 3'), cex=1.5, col=colors, lwd=2, lty = 2, bty='n', ncol=4)
  
  par(mar=c(0, 0, 0, 0), family='HersheySerif')
  plot(0, type='n', xlim=c(1, 11), ylim=c(1, 12), xlab=' ', ylab=' ', cex.lab=1.5, family='HersheySerif', 
       axes=FALSE, bty='n')
  legend('top','groups', legend=c('Actors:', 'Client 1', 'Client 2', 'Boss'), cex=1.5, col=colors, lwd=2, bty='n', ncol=4)
  legend('bottom','groups', legend=c('Objects:', 'Client 1', 'Client 2', 'Boss'), cex=1.5, col=colors, lwd=2, lty=2, bty='n', ncol=4)
dev.off()
  
g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)
  
#ggplot2::ggsave("GS_Median Role Deflection_17March2020.pdf", 
#                dpi=600, width = 8.5, height = 5)

ggplot2::ggsave("GSF_Median Role Deflection_17March2020.pdf", 
                dpi=600, width = 8.5, height = 5)



