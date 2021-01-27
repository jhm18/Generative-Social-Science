#Bias Project: Final Figures
#Jonathan H. Morgan
#22 January 2021

#Clear Out Console Script
cat("\014")

#Setting Work Directory
setwd("/Users/jonathan.h.morgan/Desktop/Bias Project/Data and Scripts")
getwd()

#Options
options(stringsAsFactors = FALSE)
options(mc.cores = parallel::detectCores())
options(repr.plot.width=10.6806, repr.plot.height=7.30556)

################
#   PACKAGES   #
################

library(magrittr)   #R Pipe Functionality
library(dplyr)      #Data Management
library(cowplot)    #Used to convert R Graphics Plots into GROBs
library(ggplot2)    #Used to output plots as PDFs

#################
#   FUNCTIONS   #
#################

'%!in%' <- function(x,y)!('%in%'(x,y))

set_plot_dimensions <- function(width_choice, height_choice) {
  options(repr.plot.width=width_choice, repr.plot.height=height_choice)
}

#Plot Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_2Oct2020.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

######################
#   IMPORTING DATA   #
######################

load("Logged_Measures_NetworksList_03June2019.rda") 

#######################
#   FORMATTING DATA   #
#######################

# Creating variables with which to subset
  centrality_measures <- c("indegree.cor", "outdegree.cor", "totaldegree.cor", "bonpower.centrality.cor", "close.centrality.inverse.cor", "between.centrality.cor")        
  centralization_measures <- c("indegree.stds", "outdegree.stds", "totaldegree.stds", "bonpower.centrality.stds", "close.centrality.inverse.stds", "between.centrality.stds")                         
  topology_measures <- c("component.size.weak.prop", "bicomp.size.prop", "mean.distance.inv.scale", "transitivity", "tau.RC", "rand.stat")   

# Creating a List for each Measure Type for the Purposes of Visualizations (6 measures per figure)
  Centrality_List <- Networks_List
  Centralization_List <- Networks_List
  Topology_List <- Networks_List

# Subsetting the Lists for Each Set of Measures
  for (i in seq_along(Centrality_List)){
    Centrality_List[[i]] <- Centrality_List[[i]][ ,c('net.name','directed', 'missing.datacor', 'missing.percentage', 'imputation.type', centrality_measures)]
  }

  for (i in seq_along(Centralization_List)){
    Centralization_List[[i]] <- Centralization_List[[i]][,c('net.name','directed', 'missing.datacor', 'missing.percentage', 'imputation.type', centralization_measures)]
  }

  for (i in seq_along(Topology_List)){
    Topology_List[[i]] <- Topology_List[[i]][,c('net.name','directed', 'missing.datacor', 'missing.percentage', 'imputation.type', topology_measures)]
  }

# Gathering Datasets to Create a Stacked Dataset in terms of Measure for each Measure List
  for (i in seq_along(Centrality_List)){
    Centrality_List[[i]] <- tidyr::gather(data=Centrality_List[[i]], key=Measure, value=centrality_measures, -net.name, -directed, -missing.datacor, -missing.percentage, -imputation.type)
  }

  for (i in seq_along(Centralization_List)){
    Centralization_List[[i]] <- tidyr::gather(data=Centralization_List[[i]], key=Measure, value=centralization_measures, -net.name, -directed, -missing.datacor, -missing.percentage, -imputation.type)
  }

  for (i in seq_along(Topology_List)){
    Topology_List[[i]] <- tidyr::gather(data=Topology_List[[i]], key=Measure, value=topology_measures, -net.name, -directed, -missing.datacor, -missing.percentage, -imputation.type)
  }

# Renaming meausures and transforming variables into factors
  for (i in seq_along(Centrality_List)){
    Centrality_List[[i]] <- Centrality_List[[i]] %>%
      mutate(Measure =  ifelse(Measure == "indegree.cor", "InDegree",
                        ifelse(Measure == "outdegree.cor", "OutDegree",
                        ifelse(Measure == "totaldegree.cor", "Total Degree", 
                        ifelse(Measure == "bonpower.centrality.cor", "Bon. Power Centrality",
                        ifelse(Measure == "close.centrality.inverse.cor", "Closeness Centrality",
                                                         ifelse(Measure == "between.centrality.cor","Betweeness Centrality", "NA"))))))) %>%
      dplyr::rename(`Bias` = `centrality_measures`) %>%
      mutate(filter = ifelse(directed == 0 & imputation.type == "directed", 2,
                      ifelse(directed == 0 & imputation.type == "probabilistic", 1, 0))) %>%
      filter(filter == 0)
  
      Centrality_List[[i]]$Measure = factor(Centrality_List[[i]]$Measure,levels=c("InDegree","OutDegree","Total Degree", "Bon. Power Centrality",
                                                                              "Closeness Centrality","Betweeness Centrality"),ordered=TRUE)
  
      Centrality_List[[i]]$imputation.type = factor(Centrality_List[[i]]$imputation.type,levels=c("none", "probabilistic", "reciprocated", "directed", "imputation strategy 1", "imputation strategy 2"),ordered=TRUE)
  
      Centrality_List[[i]]$missing.datacor = factor(Centrality_List[[i]]$missing.datacor,levels=c(-0.75, 0.00, 0.75),ordered=TRUE)
    }

  for (i in seq_along(Centralization_List)){
    Centralization_List[[i]] <- Centralization_List[[i]] %>%
      mutate(Measure =  ifelse(Measure == "indegree.stds", "InDegree STDS",
                        ifelse(Measure == "outdegree.stds", "OutDegree STDS",
                        ifelse(Measure == "totaldegree.stds", "Total Degree STDS", 
                        ifelse(Measure == "bonpower.centrality.stds", "Bon. Power Centrality STDS",
                        ifelse(Measure == "close.centrality.inverse.stds", "Closeness Centrality STDS",
                        ifelse(Measure == "between.centrality.stds","Betweeness Centrality STDS", "NA"))))))) %>%
      dplyr::rename(`Bias` = `centralization_measures`) %>%
      mutate(filter = ifelse(directed == 0 & imputation.type == "directed", 2,
                      ifelse(directed == 0 & imputation.type == "probabilistic", 1, 0))) %>%
      filter(filter == 0)
  
      Centralization_List[[i]]$Measure = factor(Centralization_List[[i]]$Measure,levels=c("InDegree STDS","OutDegree STDS","Total Degree STDS", "Bon. Power Centrality STDS",
                                                                                          "Closeness Centrality STDS","Betweeness Centrality STDS"),ordered=TRUE)
  
      Centralization_List[[i]]$imputation.type = factor(Centralization_List[[i]]$imputation.type,levels=c("none", "probabilistic", "reciprocated", "directed", "imputation strategy 1", "imputation strategy 2"),ordered=TRUE)
  
      Centralization_List[[i]]$missing.datacor = factor(Centralization_List[[i]]$missing.datacor,levels=c(-0.75, 0.00, 0.75),ordered=TRUE)
  }

  for (i in seq_along(Topology_List)){
    Topology_List[[i]]  <- Topology_List[[i]] %>%
      mutate(Measure =  ifelse(Measure == "component.size.weak.prop", "Component Size",
                        ifelse(Measure == "bicomp.size.prop", "Bicomponent Size",
                        ifelse(Measure == "mean.distance.inv.scale", "Distance", 
                        ifelse(Measure == "transitivity", "Transitivity",
                        ifelse(Measure == "tau.RC", "Tau RC",
                        ifelse(Measure == "rand.stat","CONCOR", "NA"))))))) %>%
      dplyr::rename(`Bias` = `topology_measures`) %>%
      mutate(filter = ifelse(directed == 0 & imputation.type == "directed", 2,
                      ifelse(directed == 0 & imputation.type == "probabilistic", 1, 0))) %>%
      filter(filter == 0)
  
      Topology_List[[i]]$Measure = factor(Topology_List[[i]]$Measure,levels=c("Component Size","Bicomponent Size","Distance", "Transitivity","Tau RC","CONCOR"),ordered=TRUE)
  
      Topology_List[[i]]$imputation.type = factor(Topology_List[[i]]$imputation.type,levels=c("none", "probabilistic", "reciprocated", "directed", "imputation strategy 1", "imputation strategy 2"),ordered=TRUE)
  
      Topology_List[[i]]$missing.datacor = factor(Topology_List[[i]]$missing.datacor,levels=c(-0.75, 0.00, 0.75),ordered=TRUE)
  }

  rm(centrality_measures, centralization_measures, topology_measures, i)
  
#############################
#   VISUALIZATION ELEMENTS  #
#############################
  
# Creating Labels
  corr_labels <- c(-0.75, 0, 0.75)
  axis_labels <- c('1%', '35%', '70%')
  u_colors <- c('black','brown', 'blue', 'goldenrod2')
  d_colors <- c('black', 'olivedrab3', 'brown', 'indianred2', 'blue', 'goldenrod2')
  u_shapes <- c(1, 2, 3, 5)
  d_shapes <- c(1, 0, 2, 6, 3, 5)

# Creating Undirected Base Visualization Matrix
  m_u <- matrix(c(21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 0,
                8, 8, 8, 8, 9, 9, 9, 9, 10,10,10,10,11,11,11,11, 5,
                8, 8, 8, 8, 9, 9, 9, 9, 10,10,10,10,11,11,11,11, 5,
                8, 8, 8, 8, 9, 9, 9, 9, 10,10,10,10,11,11,11,11, 5,
                12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,6,
                12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,6,
                12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,6,
                16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,7,
                16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,7,
                16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,7,
                20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20), ncol  = 17, byrow = TRUE)
  layout(m_u)
  layout.show(21)
  
# Creating Directed Base Visualization Matrix
  m_d <- matrix(c(25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,
                  1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0,
                  9, 9, 9, 9, 10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,6,
                  9, 9, 9, 9, 10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,6,
                  9, 9, 9, 9, 10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,6,
                  14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,7,
                  14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,7,
                  14,14,14,14,15,15,15,15,16,16,16,16,17,17,17,17,18,18,18,18,7,
                  19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,8,
                  19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,8,
                  19,19,19,19,20,20,20,20,21,21,21,21,22,22,22,22,23,23,23,23,8,
                  24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,
                  24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24), ncol  = 21, byrow = TRUE)
  layout(m_d)
  layout.show(25)
  
# Creating Visualization Matrix for Topology Graphs
  m_t <- matrix(c(29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
                  1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 0,
                  10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,7,
                  10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,7,
                  10,10,10,10,11,11,11,11,12,12,12,12,13,13,13,13,14,14,14,14,15,15,15,15,7,
                  16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,8,
                  16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,8,
                  16,16,16,16,17,17,17,17,18,18,18,18,19,19,19,19,20,20,20,20,21,21,21,21,8,
                  22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,27,27,27,27,9,
                  22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,27,27,27,27,9,
                  22,22,22,22,23,23,23,23,24,24,24,24,25,25,25,25,26,26,26,26,27,27,27,27,9,
                  28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
                  28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28), ncol=25, byrow=TRUE)
  layout(m_t)
  layout.show(29)
  
####################################################################
#   UNDIRECTED NETWORKS: CENTRALITY, CENTRALIZATION, & TOPOLOGY    #
####################################################################
  
# Formatting Undirected Files: Centrality
  Undirected_Networks <- Centrality_List[[1]] %>%
    filter(Measure != "OutDegree") %>%
    filter(Measure != "Total Degree") 
  
  Undirected_Networks$Measure <- as.character(Undirected_Networks$Measure)
  Undirected_Networks$imputation.type <- as.character(Undirected_Networks$imputation.type)
  
  Undirected_Networks <- Undirected_Networks  %>%
    mutate(Measure = ifelse(Measure == "InDegree", "Degree", Measure)) %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                                    ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  Undirected_Networks$Measure = factor(Undirected_Networks$Measure,levels=c("Degree", "Bon. Power Centrality", "Closeness Centrality","Betweeness Centrality"), ordered=TRUE)
  Undirected_Networks$imputation.type = factor(Undirected_Networks$imputation.type, levels=c("none", "reciprocated", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Undirected: Centrality Comparison
  centrality_labels <- c('Degree', 'Bon. Power\n Centrality', 'Closeness\nCentrality', 'Betweeness\nCentrality')
  
  x11(width=10.6806, height=7.30556)
  tst <- function() {
    layout(m_u)
    
    # Creating Column Headings
      for(i in seq_along(centrality_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.7, col = "black", font=2, family='serif')
        }else if(i == 4){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
    
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
    
    # Creating Cell Plots
      Undirected_Network <- Undirected_Networks[Undirected_Networks$net.name =="Biotech", ]
      centrality_labels <- as.character(unique(Undirected_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Undirected_Network[Undirected_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(centrality_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.35, 0.7)
          corr_measure <- measure[measure$Measure == centrality_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,0.7), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
               family='HersheySerif', las=1, main=' ')
          
          for (j in 2:length(y_axis)){
            abline(h =y_axis[[j]], col = "grey", lty = "dotted")
          }

          if (corr_measure[1,3] == 0.75) {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
          
          if (corr_measure[1,6] == "Degree") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }

          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = u_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = u_colors[[k]], pch=u_shapes[[k]], cex=1.3)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
    
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Symmetric', 'Simple Model-Based', 'Complex Model-Based'),
             lty=1, pch=u_shapes, col=u_colors, lwd=2, ncol=4,  cex=1.3, bty ="n")
      mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
      
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.94, y = 0, 'Figure 3. Predicted Bias for Centrality Measures for a Large, Undirected, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  tst()
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(g)
  
  p_1
  ggplot2::ggsave("Figure3_Undirected_Centrality Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92)
  rm(p_1, g, tst)
  dev.off()
  dev.off()
  
# Formatting Undirected Files: Centralization
  Undirected_Networks <- Centralization_List[[1]] %>%
    filter(Measure != "OutDegree STDS") %>%
    filter(Measure != "Total Degree STDS") 
  
  Undirected_Networks$Measure <- as.character(Undirected_Networks$Measure)
  Undirected_Networks$imputation.type <- as.character(Undirected_Networks$imputation.type)
  
  Undirected_Networks <- Undirected_Networks  %>%
    mutate(Measure = ifelse(Measure == "InDegree STDS", "Degree STDS", Measure)) %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                             ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  Undirected_Networks$Measure = factor(Undirected_Networks$Measure,levels=c("Degree STDS", "Bon. Power Centrality STDS", "Closeness Centrality STDS","Betweeness Centrality STDS"), ordered=TRUE)
  Undirected_Networks$imputation.type = factor(Undirected_Networks$imputation.type, levels=c("none", "reciprocated", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Undirected: Centralization Comparison
  centralization_labels <- c('Degree STDS', 'Bon. Power\n Centrality STDS', 'Closeness\nCentrality STDS', 'Betweeness\nCentrality STDS')
  
  x11(width=10.6806, height=7.30556)
  tst <- function() {
    layout(m_u)
  
    # Creating Column Headings
      for(i in seq_along(centralization_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 4){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,1))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Cell Plots
      Undirected_Network <- Undirected_Networks[Undirected_Networks$net.name =="Biotech", ]
      centralization_labels <- as.character(unique(Undirected_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Undirected_Network[Undirected_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(centralization_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.6, 1.2)
          corr_measure <- measure[measure$Measure == centralization_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,1.2), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
              family='HersheySerif', las=1, main=' ')
      
            for (j in 2:length(y_axis)){
              abline(h =y_axis[[j]], col = "grey", lty = "dotted")
            }
      
            if (corr_measure[1,3] == 0.75) {
              axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
            }else{
              axis(side=1, x_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
      
            if (corr_measure[1,6] == "Degree STDS") {
              axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 2, line = 3.55, at= 0.6, 'Bias', cex=1)
            }else{
              axis(side=2, y_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
      
            methods <- unique(corr_measure$imputation.type)
            for (k in seq_along(methods)) {
              lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                    lwd = 1, col = u_colors[[k]])
              points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                     lwd = 1, col = u_colors[[k]], pch=u_shapes[[k]], cex=1.3)
            }
            rm(corr_measure, methods)
          }
          rm(measure, y_axis, x_axis)
        }
  
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Symmetric', 'Simple Model-Based', 'Complex Model-Based'),
              lty=1, pch=u_shapes, col=u_colors, lwd=2, ncol=4,  cex=1.3, bty ="n")
      mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
  
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.9525, y = 0, 'Figure 6. Predicted Bias for Centralization Measures for a Large, Undirected, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  
  tst()
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(g)

  p_1
  ggplot2::ggsave("Figure6_Undirected_Centralization Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92)
  rm(p_1, g, tst)
  dev.off()
  dev.off()
  
# Formatting Undirected Files: Topology
  Undirected_Networks <- Topology_List[[1]] 
  
  Undirected_Networks$imputation.type <- as.character(Undirected_Networks$imputation.type)
  
  Undirected_Networks <- Undirected_Networks  %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                             ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  Undirected_Networks$imputation.type = factor(Undirected_Networks$imputation.type, levels=c("none", "reciprocated", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Undirected: Topology Comparison
  topology_labels <- c('Component\n Size', 'Bicomponent\n Size', 'Distance', 'Transitivity', 'Tau RC', 'CONCOR')

  x11(width=11.4, height=7.4)
  tst <- function() {
      layout(m_t)
  
    # Creating Column Headings
      for(i in seq_along(topology_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.7, col = "black", font=2, family='serif')
        }else if(i == 6){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.7, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.7, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Cell Plots
      Undirected_Network <- Undirected_Networks[Undirected_Networks$net.name =="Biotech", ]
      topology_labels <- as.character(unique(Undirected_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Undirected_Network[Undirected_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(topology_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.7, 1.4)
          corr_measure <- measure[measure$Measure == topology_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,1.4), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
               family='HersheySerif', las=1, main=' ')
      
          for (j in 2:length(y_axis)){
            abline(h =y_axis[[j]], col = "grey", lty = "dotted")
          }
      
          if (corr_measure[1,3] == 0.75) {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          if (corr_measure[1,6] == "Component Size") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.6, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, lwd.ticks = 0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = u_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                   lwd = 1, col = u_colors[[k]], pch=u_shapes[[k]], cex=1.3)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
  
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Symmetric', 'Simple Model-Based', 'Complex Model-Based'),
             lty=1, pch=u_shapes, col=u_colors, lwd=2, ncol=4,  cex=1.3, bty ="n")
      mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
  
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.855, y = 0, 'Figure 9. Predicted Bias for Topology Measures for a Large, Undirected, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  
  tst()
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(g)
  
  p_1
  ggplot2::ggsave("Figure9_Undirected_Topology Comparison_22Jan2020.pdf", width = 11.4, height = 7.4, device='pdf', dpi=600)
  rm(p_1, g, p_1, tst)
  dev.off()
  dev.off()
  
################################################################
#   DIRECTED NETWORKS: CENTRALITY, CENTRALIZATION, & TOPOLOGY  #
################################################################
  
# Formatting Directed Files: Centrality
  Directed_Networks <- Centrality_List[[2]] %>%
    filter(Measure != "OutDegree") 
  
  Directed_Networks$imputation.type <- as.character(Directed_Networks$imputation.type)
  
  Directed_Networks <- Directed_Networks  %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                                    ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  
  Directed_Networks$imputation.type = factor(Directed_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Directed: Centrality Comparison
  centrality_labels <- c('InDegree', 'Total\n Degree', 'Bon. Power\n Centrality', 'Closeness\nCentrality', 'Betweeness\nCentrality')
  
  x11(width=10.6806, height=7.30556)
  tst <- function() {
      layout(m_d)
  
    # Creating Column Headings
      for(i in seq_along(centrality_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 5){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, centrality_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Cell Plots
      Directed_Network <- Directed_Networks[Directed_Networks$net.name =="HighSc p13", ]
      centrality_labels <- as.character(unique(Directed_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Directed_Network[Directed_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(centrality_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.35, 0.7)
          corr_measure <- measure[measure$Measure == centrality_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,0.7), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
            family='HersheySerif', las=1, main=' ')
      
          for (j in 2:length(y_axis)){
            abline(h =y_axis[[j]], col = "grey", lty = "dotted")
          }
      
          if (corr_measure[1,3] == 0.75) {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          if (corr_measure[1,6] == "InDegree") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, cex.axis= 1.3, lwd.ticks = 0, tck=0, family = 'HersheySerif', las=1)
          }
      
          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
              lwd = 1, col = d_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
               lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.3)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
  
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
             lty=1, pch=d_shapes, col=d_colors, lwd=1.3, ncol=6,  cex=0.87, bty ="n")
      mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
  
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.93, y = 0, 'Figure 4. Predicted Bias for Centrality Measures for a Large, Directed, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(tst)
  
  p_1
  ggplot2::ggsave("Figure4_Directed_Centrality Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92, device='pdf')
  rm(p_1, g, tst)
  dev.off()

# Formatting Directed Files: Centralization
  Directed_Networks <- Centralization_List[[2]] %>%
    filter(Measure != "OutDegree STDS") 
  
  Directed_Networks$imputation.type <- as.character(Directed_Networks$imputation.type)
  
  Directed_Networks <- Directed_Networks  %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                             ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  Directed_Networks$imputation.type = factor(Directed_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Directed: Centralization Comparison
  centralization_labels <- c('InDegree STDS', 'Total\n Degree STDS', 'Bon. Power\n Centrality STDS', 'Closeness\nCentrality STDS', 'Betweeness\nCentrality STDS')
    
  x11(width=10.6806, height=7.30556)
  tst <- function() {
      layout(m_d)
  
    # Creating Column Headings
      for(i in seq_along(centrality_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 5){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, centralization_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0.7))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Cell Plots
      Directed_Network <- Directed_Networks[Directed_Networks$net.name =="HighSc p13", ]
      centralization_labels <- as.character(unique(Directed_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Directed_Network[Directed_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(centralization_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.4, 0.8)
          corr_measure <- measure[measure$Measure == centralization_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,0.8), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
              family='HersheySerif', las=1, main=' ')
      
          for (k in 2:length(y_axis)){
            abline(h =y_axis[[k]], col = "grey", lty = "dotted")
          }
      
          if (corr_measure[1,3] == 0.75) {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          if (corr_measure[1,6] == "InDegree STDS") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = d_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                   lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.3)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
  
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
             lty=1, pch=d_shapes, col=d_colors, lwd=2, ncol=6,  cex=0.87, bty ="n")
             mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
  
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.945, y = 0, 'Figure 7. Predicted Bias for Centralization Measures for a Large, Directed, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(tst)
  
  p_1
  ggplot2::ggsave("Figure7_Directed_Centralization Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92, device='pdf')
  rm(p_1, g, tst)
  dev.off()
  
# Formatting Directed Files: Topology
  Directed_Networks <- Topology_List[[2]]
  
  Directed_Networks$imputation.type <- as.character(Directed_Networks$imputation.type)
  
  Directed_Networks <- Directed_Networks  %>%
    mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                             ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
  Directed_Networks$imputation.type = factor(Directed_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
  
# Directed: Topology Comparison
  topology_labels <- c('Component\n Size', 'Bicomponent\n Size', 'Distance', 'Transitivity', 'Tau RC', 'CONCOR')
  
  x11(width=11.4, height=7.4)
  tst <- function() {
      layout(m_t)
  
    # Creating Column Headings
      for(i in seq_along(topology_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=0.57)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }else if(i == 6){
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          abline(v=1.43)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(h=-1.06)
          abline(h=1.06)
          text(x = 1, y = 0.15, topology_labels[[i]], cex = 1.5, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Row Headings
      for (i in seq_along(corr_labels)) {
        if (i == 1){
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else if(i == 3){
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          abline(h=-1.06)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }else{
          par(mar = c(0,0,0,0.6))
          plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
          abline(v=0.57)
          abline(v=1.43)
          text(x = 1, y = 0, corr_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
        }
      }
  
    # Creating Cell Plots
      Directed_Network <- Directed_Networks[Directed_Networks$net.name =="HighSc p13", ]
      topology_labels <- as.character(unique(Directed_Network$Measure))
      for (i in seq_along(corr_labels)) {
        measure <- Directed_Network[Directed_Network$missing.datacor == corr_labels[[i]], ]
        for (j in seq_along(topology_labels)) {
          x_axis <- c(0.1, 3.5, 7)
          y_axis <- c(0.0,  0.5, 1)
          corr_measure <- measure[measure$Measure == topology_labels[[j]], ]
          par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
          plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,1), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
               family='HersheySerif', las=1, main=' ')
      
          for (k in 2:length(y_axis)){
            abline(h =y_axis[[k]], col = "grey", lty = "dotted")
          }
      
          if (corr_measure[1,3] == 0.75) {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          if (corr_measure[1,6] == "Component Size") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
      
          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = d_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.2)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
  
    # Add Legend
      par(mar = c(0,0,0,0))
      plot.new()
      legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
             lty=1, pch=d_shapes, col=d_colors, lwd=2, ncol=6,  cex=1.1, bty ="n")
      mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
  
    # Adding Title
      par(mar = c(0,0,0,0))
      plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
      text(x = 0.8525, y = 0, 'Figure 10. Predicted Bias for Topology Measures for a Large, Directed, Moderately Centralized Network', cex = 1.5, col = "black", font=2, family='serif')
  }
  
  g <- cowplot::as_grob(tst)
  p_1 <- cowplot::ggdraw(tst)
  
  p_1
  ggplot2::ggsave("Figure10_Directed_Topology Comparison_22Jan2020.pdf", width = 11.4, height = 7.4, device='pdf', dpi=600)
  rm(p_1, g, tst)
  dev.off()
  
#############################
#   SIZE & CENTRALIZATION   #
#############################
  
  # Formatting Data for Centrality Comparison
    All_Networks <- Centrality_List[[2]] %>%
      filter(net.name == "RC Elite" | net.name == "HighSc p24" | net.name == "6th grade" | net.name == "Sorority") %>%
      filter(Measure == "InDegree" | Measure == "Bon. Power Centrality" | Measure == "Betweeness Centrality") %>%
      filter(missing.datacor == "0") %>%
      mutate(network.type = ifelse(net.name == "RC Elite", "Large Centralized",
                            ifelse(net.name == "HighSc p24", "Large Decentralized",
                            ifelse(net.name == "6th grade", "Small Centralized",
                            ifelse(net.name == "Sorority", "Small Decentralized", "NA"))))) 
  
    All_Networks$imputation.type <- as.character(All_Networks$imputation.type)
  
    All_Networks <- All_Networks %>%
      mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                               ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
  
    All_Networks$network.type = factor(All_Networks$network.type, levels=c("Large Centralized", "Small Centralized", "Large Decentralized", "Small Decentralized"), ordered=TRUE)
    All_Networks$imputation.type = factor(All_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
  
  # Centrality: Network Comparison
    network_labels <- c('Large\n Centralized', 'Small\n Centralized', 'Large\n Decentralized', 'Small\n Decentralized')
    n_measures <- c('InDegree', 'Bon. Power\n Centrality', 'Betweeness\nCentrality')
    
    x11(width=10.6806, height=7.30556)
    tst <- function() {
        layout(m_u)
    
      # Creating Column Headings
        for(i in seq_along(network_labels)) {
          if (i == 1){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=0.57)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else if(i == 4){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=1.43)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else{
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }
        }
    
      # Creating Row Headings
        for (i in seq_along(corr_labels)) {
          if (i == 1){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else if(i == 3){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=-1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else{
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }
        }
    
      # Creating Cell Plots
        network_labels <- c('Large Centralized', 'Small Centralized', 'Large Decentralized', 'Small Decentralized')
        n_measures <- as.character(unique(All_Networks$Measure))
        for (i in seq_along(n_measures)) {
          measure <- All_Networks[All_Networks$Measure == n_measures[[i]], ]
          for (j in seq_along(network_labels)) {
            x_axis <- c(0.1, 3.5, 7)
            y_axis <- c(0.0,  0.35, 0.7)
            corr_measure <- measure[measure$network.type == network_labels[[j]], ]
            par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
            plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,0.7), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
                 family='HersheySerif', las=1, main=' ')
        
          for (j in 2:length(y_axis)){
            abline(h =y_axis[[j]], col = "grey", lty = "dotted")
          }
        
          if (corr_measure[1,6] == 'Betweeness Centrality') {
            axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
          }else{
            axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
        
          if (corr_measure[1,9] == "Large Centralized") {
            axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
            mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
          }else{
            axis(side=2, y_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
          }
        
          methods <- unique(corr_measure$imputation.type)
          for (k in seq_along(methods)) {
            lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = d_colors[[k]])
            points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                  lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.3)
          }
          rm(corr_measure, methods)
        }
        rm(measure, y_axis, x_axis)
      }
    
      # Add Legend
        par(mar = c(0,0,0,0))
        plot.new()
        legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
                lty=1, pch=d_shapes, col=d_colors, lwd=2, ncol=6,  cex=0.87, bty ="n")
        mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)

      # Adding Title
        par(mar = c(0,0,0,0))
        plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
        text(x = 0.825, y = 0, 'Figure 5. Predicted Bias for Centrality Measures for Four Network Types', cex = 1.5, col = "black", font=2, family='serif')
    }
    
    g <- cowplot::as_grob(tst)
    p_1 <- cowplot::ggdraw(tst)
   
    p_1
    ggplot2::ggsave("Figure5_Directed_Centrality_Network Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92, device='pdf')
    rm(p_1, g, tst)
    dev.off()
    
  # Formatting Data for Centralization Comparison
    All_Networks <- Centralization_List[[2]] %>%
      filter(net.name == "RC Elite" | net.name == "HighSc p24" | net.name == "6th grade" | net.name == "Sorority") %>%
      filter(Measure == "InDegree STDS" | Measure == "Bon. Power Centrality STDS" | Measure == "Betweeness Centrality STDS") %>%
      filter(missing.datacor == "0") %>%
      mutate(network.type = ifelse(net.name == "RC Elite", "Large Centralized",
                            ifelse(net.name == "HighSc p24", "Large Decentralized",
                            ifelse(net.name == "6th grade", "Small Centralized",
                            ifelse(net.name == "Sorority", "Small Decentralized", "NA")))))
    
    All_Networks$imputation.type <- as.character(All_Networks$imputation.type)
    
    All_Networks <- All_Networks %>%
      mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                               ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
    
    
    All_Networks$network.type = factor(All_Networks$network.type, levels=c("Large Centralized", "Small Centralized", "Large Decentralized", "Small Decentralized"), ordered=TRUE)
    All_Networks$imputation.type = factor(All_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
    
  # Centralization: Network Comparison
    network_labels <- c('Large\n Centralized', 'Small\n Centralized', 'Large\n Decentralized', 'Small\n Decentralized')
    n_measures <- c('InDegree\n STDS', 'Bon. Power\n Centrality STDS', 'Betweeness\n Centrality STDS')
    
    x11(width=10.6806, height=7.30556)
    tst <- function() {
        layout(m_u)
    
      # Creating Column Headings
        for(i in seq_along(network_labels)) {
          if (i == 1){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=0.57)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else if(i == 4){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=1.43)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else{
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }
        }
    
      # Creating Row Headings
        for (i in seq_along(n_measures)) {
          if (i == 1){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else if(i == 3){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=-1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else{
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }
        }
    
      # Creating Cell Plots
        network_labels <- c('Large Centralized', 'Small Centralized', 'Large Decentralized', 'Small Decentralized')
        n_measures <- unique(as.character(All_Networks$Measure))
        for (i in seq_along(n_measures)) {
          measure <- All_Networks[All_Networks$Measure == n_measures[[i]], ]
          for (j in seq_along(network_labels)) {
            x_axis <- c(0.1, 3.5, 7)
            y_axis <- c(0.0,  0.6, 1.2)
            corr_measure <- measure[measure$network.type == network_labels[[j]], ]
            par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
            plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,1.2), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
                family='HersheySerif', las=1, main=' ')
        
            for (j in 2:length(y_axis)){
              abline(h =y_axis[[j]], col = "grey", lty = "dotted")
            }
        
            if (corr_measure[1,6] == 'Betweeness Centrality STDS') {
              axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
            }else{
              axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
        
            if (corr_measure[1,9] == "Large Centralized") {
              axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
            }else{
              axis(side=2, y_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
        
            methods <- unique(corr_measure$imputation.type)
            for (k in seq_along(methods)) {
              lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                    lwd = 1, col = d_colors[[k]])
              points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                    lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.3)
            }
            rm(corr_measure, methods)
          }
          rm(measure, y_axis, x_axis)
        }
    
      # Add Legend
        par(mar = c(0,0,0,0))
        plot.new()
        legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
               lty=1, pch=d_shapes, col=d_colors, lwd=0.87, ncol=6,  cex=0.87, bty ="n")
        mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
    
      # Adding Title
        par(mar = c(0,0,0,0))
        plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
        text(x = 0.8375, y = 0, 'Figure 8. Predicted Bias for Centralization Measures for Four Network Types', cex = 1.5, col = "black", font=2, family='serif')
  }
    
    g <- cowplot::as_grob(tst)
    p_1 <- cowplot::ggdraw(tst)
    
    p_1
    ggplot2::ggsave("Figure8_Directed_Centralization_Network Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92, device='pdf')
    rm(p_1,g,tst)
    dev.off()
    
  # Formatting Data for Topology Comparison
    All_Networks <- Topology_List[[2]] %>%
      filter(net.name == "RC Elite" | net.name == "HighSc p24" | net.name == "6th grade" | net.name == "Sorority") %>%
      filter(Measure == "Component Size" | Measure == "Distance" | Measure == "Transitivity") %>%
      filter(missing.datacor == "0") %>%
      mutate(network.type = ifelse(net.name == "RC Elite", "Large Centralized",
                            ifelse(net.name == "HighSc p24", "Large Decentralized",
                            ifelse(net.name == "6th grade", "Small Centralized",
                            ifelse(net.name == "Sorority", "Small Decentralized", "NA")))))
    
    All_Networks$imputation.type <- as.character(All_Networks$imputation.type)
    
    All_Networks <- All_Networks %>%
      mutate(imputation.type = ifelse(imputation.type == "imputation strategy 1", "simple model-based",
                                      ifelse(imputation.type == "imputation strategy 2", "complex model-based", imputation.type)))
    
    All_Networks$network.type = factor(All_Networks$network.type, levels=c("Large Centralized", "Small Centralized", "Large Decentralized", "Small Decentralized"), ordered=TRUE)
    All_Networks$imputation.type = factor(All_Networks$imputation.type, levels=c("none", "probabilistic", "reciprocated", "directed", "simple model-based", "complex model-based"), ordered=TRUE)
    
  # Centralization: Network Comparison
    network_labels <- c('Large\n Centralized', 'Small\n Centralized', 'Large\n Decentralized', 'Small\n Decentralized')
    n_measures <- c('Component\n Size', 'Distance', 'Transitivity')
  
    x11(width=10.6806, height=7.30556)
    tst <- function() {
        layout(m_u)
  
      # Creating Column Headings
        for(i in seq_along(network_labels)) {
          if (i == 1){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=0.57)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else if(i == 4){
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            abline(v=1.43)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }else{
            par(mar = c(0,0,0,0))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(h=-1.06)
            abline(h=1.06)
            text(x = 1, y = 0.15, network_labels[[i]], cex = 1.3, col = "black", font=2, family='serif')
          }
        }
      
      # Creating Row Headings
        for (i in seq_along(n_measures)) {
          if (i == 1){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else if(i == 3){
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            abline(h=-1.06)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }else{
            par(mar = c(0,0,0,0.7))
            plot(0, type='n', xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', family='HersheySerif', las=1, main=' ', bty='n')
            abline(v=0.57)
            abline(v=1.43)
            text(x = 1, y = 0, n_measures[[i]], cex = 1.3, col = "black", font=2, family='serif', srt=270)
          }
        }
      
      # Creating Cell Plots
        network_labels <- c('Large Centralized', 'Small Centralized', 'Large Decentralized', 'Small Decentralized')
        n_measures <- unique(as.character(All_Networks$Measure))
        for (i in seq_along(n_measures)) {
          measure <- All_Networks[All_Networks$Measure == n_measures[[i]], ]
          for (j in seq_along(network_labels)) {
            x_axis <- c(0.1, 3.5, 7)
            y_axis <- c(0.0,  0.5, 1)
            corr_measure <- measure[measure$network.type == network_labels[[j]], ]
            par(mar = c(2, 5, 1, 1), bty='n', family='HersheySerif')
            plot(0, type='n', xlim=c(0.1, 7), ylim=c(0,1), xlab=' ', ylab=' ', cex.axis=1.3, xaxt='n', yaxt = 'n', 
                 family='HersheySerif', las=1, main=' ')
          
            for (j in 2:length(y_axis)){
              abline(h =y_axis[[j]], col = "grey", lty = "dotted")
            }
          
            if (corr_measure[1,6] == 'Transitivity') {
              axis(side=1, x_axis, labels=axis_labels, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 1, line = 3, at= 3.5,'Percent Missing', cex=1)
            }else{
              axis(side=1, x_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
          
            if (corr_measure[1,9] == "Large Centralized") {
              axis(side=2, y_axis, cex.axis= 1.3, family = 'HersheySerif', las=1)
              mtext(side = 2, line = 3.55, at= 0.35, 'Bias', cex=1)
            }else{
              axis(side=2, y_axis, labels=FALSE, lwd.ticks =0, tck=0, cex.axis= 1.3, family = 'HersheySerif', las=1)
            }
          
            methods <- unique(corr_measure$imputation.type)
            for (k in seq_along(methods)) {
              lines(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                    lwd = 1, col = d_colors[[k]])
              points(corr_measure[corr_measure$imputation.type == methods[[k]],][[4]], corr_measure[corr_measure$imputation.type == methods[[k]],][[7]], 
                     lwd = 1, col = d_colors[[k]], pch=d_shapes[[k]], cex=1.3)
            }
            rm(corr_measure, methods)
          }
          rm(measure, y_axis, x_axis)
        }
      
      # Add Legend
        par(mar = c(0,0,0,0))
        plot.new()
        legend('center','groups', c('Listwise Deletion', 'Probabilistic', 'Symmetric', 'Asymmetric','Simple Model-Based', 'Complex Model-Based'),
               lty=1, pch=d_shapes, col=d_colors, lwd=2, ncol=6,  cex=0.87, bty ="n")
        mtext(side = 1, line=-1.5,'Overlapping Lines Indicate that the Methods Are Equally Biased', cex=1.25)
    
      # Adding Title
        par(mar = c(0,0,0,0))
        plot(0, type='n', xlab=' ', ylab=' ', xaxt='n', yaxt = 'n', cex.axis=1.3, family='HersheySerif', las=1, main=' ')
        text(x = 0.8225, y = 0, 'Figure 11. Predicted Bias for Topology Measures for Four Network Types', cex = 1.5, col = "black", font=2, family='serif')
  }
    
    g <- cowplot::as_grob(tst)
    p_1 <- cowplot::ggdraw(tst)
  
    p_1
    ggplot2::ggsave("Figure11_Directed_Topology_Network Comparison_22Jan2020.pdf", dpi=600, width = 8.9, height = 4.92, device='pdf')
    rm(p_1,g,tst)
    dev.off()
