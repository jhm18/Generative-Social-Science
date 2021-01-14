#Diffusion Simulation 
#Developed in SAS by James Moody
#Implemented in R by Jonathan H. Morgan
#23 December 2020

#DESCRIPTION

# This module implements a generalized networked diffusion project over a weighted (dynamic) multiplex graph. 
# The dynamic graph functionality, however, is not fully implemented.

#TERMS & DEFINITIONS

# Weights: Valued ties as multiple instances of a single edge, so the simulator calculates the probablity of transfer as probability of going across 
#          at least one of the instances: ptrans=1-((1-(prb))**edgeval), where edgeval is the value of the tie and prb is the transmission probability 
#          for a single interaction instance on that edge.

# The program loops over all active times (treated as discrete steps). 
# To treat the network as static, just make up constant edge times that span your simulation'S step range.

# Input: Weighted arclist of the form:{sendlab rcvlab starttime endtime value reltype}, where:
#        sendlab is the nodeID of the source (integer, 1:N)
#        rcvlab is the node id of the target (integer, 1:N)
#        starttime is when the edge begins  (Integer, 1:T)
#        endtime is when the edge ends	(Integer, 1:T)
#        value is the relational weight on the edge (any positive value)
#        reltype is an indicator for relation type (integer, 1:R)

# Model also includes: 
# spontaneous infection risk

# FUNCTION ARGUMENTS
  
# Edges: an edgelist of {sndnode rcvnode StartTime EndTime Value Type}.  

# Assumes a directed graph.
# If the graph is undirected, it should be stored as a double arc list. 
# (should be one entry from node1 to node2, a second from node2 to node 1, etc.).  

# Assumes there are 1-n nodelabels, and that these labels correspond to the order of the node-level file. 

# Type specifies each relation, 1:R

# ittervec: Controls the number of within-time infection opportunities.  

# Most data is collected in clumpy waves, so while the simulation loops over observed time-steps, we might want to allow some number of 
# within-observed-time transmission opportunities.  

# This also allows different timescales by relation, i.e. contact with friends might differ in frequency from contact with relatives.
# This is a 1xR matrix with one entry per relation type given in same order.
# If you want to use only observed times, set to 1.

# rel_iprob:  Relation-specific unit transmision probability.  
# This is the probability that the bit crosses from infected to susceptible over a value = 1 tie at a single transmission opportunity.  
# This is a 1xR matrix with entries between 0 and 1.

# seed: List of where the infection starts in the model. This is a 1 row matrix with as many seeds as you like.

# spotprob: Node-level vector of probability of "spontaneous" infection
# Captures the likelihood a node gets the bit from outside the observed system.

# durmean: Node-level vector of the length of time they are infectious.
# This is number of timesteps to remain infectious.  This is the mean of a poisson random draw -- so will have randomness added to it.

# durlatent: Node-level vector of the amount of time ego is in "E" state before infectious

# **recoverprob: Node-level vector of the likelihood that ego recovers from the disease.  
# Note: This is different from durmean, which is duration of infection.  

# recoverprob governs the outcome: die or not.

# sitype: Switch governing which susceptibility process will be modeled: SI, SIS, SIR.

# randdur: Switch, if 1 then duration/latent bits are randomized (poisson draw), 
# else treated as fixed constant on the input value.

# printt: Switch governing if iterative steps are printed to the console.

# R_net: Switch that determines if R network objects andinfection diffusion simulations will be 
# generated

# Pajek_net: Switch that outputs Pajek network and support files for network analysis in Pajek 
# and animations based on Pajek graphs.

# cellular_net: Switch that determines if a lattice plot will be generated. 
# Note, a lattice graph assume a grid-world network. 
# If your graph is not a grid-world, use the default value of 0.

# coordinates: The node id, x, and y coordiantes for the grid-world.

################
#   PACKAGES   #
################

#Installing Necessary Packages
list.of.packages <- c('network','sna', 'magick', 'ggplot2', 'ggplotify', 'animation', 'RColorBrewer', 'scales')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

library(network)      #Basic support utilities for the sna and statnet packages
library(sna)          #Used for descriptive network analysis
library(animation)    #Used to create animation
library(RColorBrewer) #Color Brewer used to generate colors for coloring components
library(scales)       #Used to convert SNA package coordinates into a custom scale for Pajek

#########################
#   SUPPORT FUNCTIONS   #
#########################

`%notin%` <- Negate(`%in%`)

###############
#   PRIMARY   #
###############
             
MR_EdgeDiff <- function(edges0, ittervec, rel_iprob, seed, spotprob, durmean, durlatent, 
                        recoverprob, sitype='SIR', randdur, printt = 1, R_net = 1, Pajek_net, 
                        cellular_net=0, coordinates=0) {

##############################
#   PERFORMING FILE CHECKS   #
##############################

# Applying Canonical Column Names & Making edge0 a Dataframe 
  edges0 <- as.data.frame(edges0)
  colnames(edges0) <-  c("n1", "n2", "value", "stime", "ftime", "type")

# FILE CHECKS

  checks_array <- vector('numeric', 3)

# Dimension Checks
  check1_pass <- c('Passed Column Number Check')
  check1_fail <- c('Expecting 6 Columns: Person i, Person j, Tie Value, Start Time, Finish Time, and Tie Type')
  ifelse(dim(edges0)[[2]] == 6, check1_pass, check1_fail)
  checks_array[[1]] <- ifelse(dim(edges0)[[2]] == 6, 1, 0)
  
# Type Checks
  check2_pass <- c('Passed Column Types Check')
  check2_fail <- c('All column values should either be numeric values or integers.')
  data_types <- vector('character', dim(edges0)[[2]])
  for (i in seq_along(data_types)) {
    data_types[[i]] <- class(edges0[[i]])
  }
  check2_types <- ifelse(data_types == 'character' | data_types == 'factor', 1, 0)
  ifelse(sum(check2_types) == 0, check2_pass, check2_fail)
  checks_array[[2]] <- ifelse(sum(check2_types) == 0, 1, 0)
  
# Time Check
  check3_pass <- c('All Start Times Occur Before Finish Times')
  check3_fail <- c('A start time occurs after a finish time.')
  time_array <- edges0$ftime - edges0$stime
  time_check <- ifelse(time_array < 0, 1, 0)
  time_check <- sum(time_check)
  ifelse(time_check == 0, check3_pass, check3_fail)
  checks_array[[3]] <- ifelse(time_check == 0, 1, 0)
  
# Duplicates Check
  check4_pass <- c('No Duplicate Rows by Sender, Target, Time, and Type')
  check4_fail <- c('Eliminated rows where there were duplicates in terms of sender, target, time, and type.')
  duplicates <- edges0[duplicated(edges0[c(1, 2, 4, 5, 6)]),]
  ifelse(dim(duplicates)[[1]] == 0, check4_pass, check4_fail)
  if (dim(duplicates)[[1]] == 0) {
    edges0 <- edges0[ , ]
  } else {
    edges0 <- edges0[!duplicated(edges0[c(1, 2, 3, 5, 6)]),]
  }
  
rm(check1_fail, check1_pass, check2_fail, check2_pass, check3_fail, check3_pass, check4_fail, check4_pass, check2_types, data_types, duplicates, time_array, time_check, i)
  
# Printing-Out File Check Results
 for (i in seq_along(checks_array)){
   passed <- c('Passed')
   failed <- c('Failed')
   ifelse(checks_array[[i]] == 1,  print(paste(passed, 'Check', i)), print(paste(failed, 'Check', i)))
   Sys.sleep(0.1)
   flush.console() 
 }

rm(passed, failed)

#########################################################
#   INIATING SIMULATION IF THE FILE PASSED FILE CHECKS  #
########################################################

if (sum(checks_array) == 3) {
  
  # Create Control Parameters, Vectors, & Matrices
    reltype <- unique(edges0$type)            #Identifying unique relation types (e.g., HIV transmission through sex and intravenious drug use)
    mintime <- min(edges0$stime)              #Identifying earliest start time
    maxtime <- max(edges0$ftime)              #Identifying latest finish time
    nodes <- unique(c(edges0$n1, edges0$n2))  #Creating nodeslist from edgelist
    seq_nodes <- seq(1, length(nodes), 1)
    nodes <- cbind(seq_nodes, nodes)
    colnames(nodes) <- c('id', 'nodes')
    n <- dim(nodes)[[1]]
    rm(seq_nodes)
  
  # Performing randur step
    if (randdur > 0) {
      durvec <- vector('numeric', length(durmean))
      for (i in seq_along(durvec)) {
        durvec[[i]] <- abs(rpois(1,durmean[[i]])) + 1
      }

      latvec <- vector('numeric', length(durlatent))
      for (i in seq_along(latvec)) {
        latvec[[i]] <- abs(rpois(1,durlatent[[i]])) + 1
      }
    } else {
      durvec <- durmean
      latvec <- durlatent
    }
    rm(durlatent, durmean, randdur)
  
  # Adding Node Characteristics
    nodes <- as.data.frame(cbind(nodes,latvec,  durvec, recoverprob, spotprob))
    colnames(nodes)[c(2:6)] <- c('node','tlat', 'trec', 'recover_prob', 'spontaneous_prob')
    rm(durvec, latvec, recoverprob, spotprob)
  
  # Creating Infection History
    InfHistStackUpdate <- as.data.frame(matrix(nrow=0, ncol=6, 0))
    colnames(InfHistStackUpdate) <- c("node", "inft", "tlat", "trec", "die", "source")
  
  # Creating Infection vector that will be updated over the course of the simulation
    infected <- as.data.frame(matrix(nrow=length(seed), ncol=4))
    colnames(infected) <- c('node', 'tlat', 'trec', 'source')
  
  # Populating infected with first values
    for(i in seq_along(seed)){
      row_values <- nodes[nodes[[2]] == seed[[i]], ]
      row_values <- row_values[c(2:4)]
      infected[i, ] <- c(row_values, 0)
      rm(row_values)
    }
  
  # Setting latency to 0 for seeds
    infected$tlat <- vector('numeric', length(infected$node)) 
  
  # Adding Infection Time: Time (0 indexed) + Infection Latency
    infected <- cbind(0, infected)
    colnames(infected)[[1]] <- c('inft')
  
  # Creating working edges file from edge0 that will be updated over the course of the simulation (Active Edges)
    edges <- edges0
    
  # Creating Susceptible Person's List that will be updated over the course of the simulation
    sus_set <- unique(c(edges$n1, edges$n2))
    sus_set <- sus_set[sus_set %notin% infected$node]
    
  # Creating Transmission Probabilities Matrix
    ptrans <- as.data.frame(cbind(reltype, rel_iprob, ittervec))
    rm(reltype, rel_iprob, ittervec)
      
  # Creating Empty List to Populate with an Infection Network each Time a Newly Infected Individuals Joins the Network
    inf_networks <- vector('list', maxtime)
    names(inf_networks) <- paste('Time', seq(1, length(inf_networks), 1))
      
  # Creating Empty List of Infection Network Attributes
    inf_attributes <- vector('list', maxtime)
    names(inf_attributes) <- paste('Time', seq(1, length(inf_attributes), 1))
      
  # INITIATING INFECTION PROCESS
    for (i in mintime:maxtime) {

##################
#  SUSCEPTIBLE   #  
##################

      # Spontaneous Infections from the set of nodes with a finish time equal or greater than the iteration time. 
        infedge <- edges[edges$stime <= i & edges$ftime >= i, ]
        inf_nodes <- unique(c(edges$n1, edges$n2))
    
      # Updating the potential infections list using the susceptible list
        inf_nodes <- inf_nodes[inf_nodes %in% sus_set]
    
      # Identifying Spontaneous Infections
        spotrsk <- vector('numeric', length(inf_nodes))
        for(j in seq_along(spotrsk)){
          spotrsk[[j]] <- rbinom(1, 1, nodes[j, 6])
        }
        inf_nodes <- cbind(inf_nodes, spotrsk)
        inf_nodes <- inf_nodes[inf_nodes[,2] == 1, ]
        if (dim(inf_nodes)[[1]] == 0) {
          infected <- infected[ , ]
        } else {
          inf_nodes <- nodes[nodes$node %in% inf_nodes, ]
          inf_nodes <- inf_nodes[c(2:4)]
          inf_nodes <- cbind(i, inf_nodes)
          inf_nodes$source <- vector('numeric', length(inf_nodes$node))
          colnames(inf_nodes)[c(1:2)] <- c('inft', colnames(infected)[[2]])
          inf_nodes$tlat <- vector('numeric', length(inf_nodes$tlat))
          infected <- rbind(infected, inf_nodes)
          infected <- infected[!duplicated(infected[c(2)]),]
          infected$tlat <- infected$inft + infected$tlat
        }
        rm(inf_nodes, spotrsk)
        
      # Printing List of Infected Individuals
        if (printt == 1) {
          print(paste('Time', i-1, 'Infections'))
          Sys.sleep(0.1)
          print(infected)
          flush.console() 
        } else {
          infected <- infected[ , ]
        }
    
      # Isolating Susceptible Alters from Sources who are currently infectious: Simulation is directed, so only looking for source edges
        inf_set <- infected[infected$tlat <= i, ]
        infedge <- edges[edges$n1 %in% inf_set$node, ]
        rm(inf_set)
    
      # Eliminating Alters Who Are No Longer Susceptible Using the Susceptible List (individuals who are dead or dead/recovered)
        sus_alters <- sus_set[sus_set %in% infedge$n2]
        infedge <- infedge[infedge$n2 %in% sus_alters, ]
        rm(sus_alters)
        
      # If there are no alters who are uninfected, then break the simulation 
      # The infection is caught in a pocket of the network
        if (nrow(infedge) == 0 ) {
          recovery_outcomes <- vector('numeric', nrow(infected))
          rset <- nodes[nodes$node %in% infected$node, ]
          for (j in seq_along(recovery_outcomes)) {
            recovery_outcomes[[j]] <- rbinom(1, 1, rset[j,5])
          }
          infected$die <- abs(recovery_outcomes - 1)
          infected <- infected[c(2, 1, 3, 4, 6, 5)]
          InfHistStackUpdate <- rbind(InfHistStackUpdate, infected)
          rm(rset, recovery_outcomes)

          print('None of the alters of those who are infected are themselves uninfected')
          Sys.sleep(0.1)
          print ('Thus,there are no new susceptible indviduals in reach.')
          break
        }else{
          infedge <- infedge[ , ]
        }
    
      # Eliminating dyads with a finish time less than the current iteration number (i.e., the relationship ended prior to this iteration)
        infedge <- infedge[infedge$ftime >= i, ]
    
      # Dropping dyads where both individuals are already infected (Resulting from Spontaneous Infections)
        infedge$both_infected <- ifelse(infedge$n2 %in% infected$node, 1, 0)
        infedge <- infedge[infedge$both_infected == 0, ]
    
      # Print Susceptible Alter List
        if (printt == 1) {
          print(paste('Time', i-1, 'Susceptible Alters'))
          Sys.sleep(0.1)
          print(infedge)
          flush.console() 
        } else {
          infedge <- infedge[ , ]
        }
    
##################
#   INFECTIOUS   #
##################
    
      # Note: The infection process assumes a directed edgelist
    
      # Creating list by relation types in order to calculate relation specific transmission probabilities
        relations_list <- vector('list', nrow(ptrans))
        names(relations_list) <- paste('Type', ptrans$reltype)
        for (j in seq_along(relations_list)) {
          relations_list[[j]] <- infedge[infedge$type == j, ]
        }
    
      # Set the transmission probability as function of:
      # 1)  The tie weight (Value), 
      # 2)  Relation specific probability (rel_iprob), 
      # 3)  Relation-specific timing (ittervec).
      # See the notes section for an example and explanation.
        for (j in seq_along(relations_list)) {
          relations_list[[j]]$probtran <- 1-((1-ptrans[j,2])**(relations_list[[j]]$value*ptrans[j,3]))
        }
    
      # Simulating infections as a weighted draw from a Bernoulli distribution
        for (j in seq_along(relations_list)) {
          trans <- vector('numeric', length(relations_list[[j]]$probtran))
          for (k in seq_along(relations_list[[j]]$probtran)) {
            trans[[k]] <- rbinom(1, 1, relations_list[[j]][k,8])
          }
          relations_list[[j]]$trans <- trans
          rm(trans)
        }

      # Formatting infections edgelist before printing updated infected list
        infedge <- do.call(rbind, relations_list)
        rownames(infedge) <- seq(1, nrow(infedge), 1)
        
      # Isolating the newly infected Rows
        newlyinf <- infedge[infedge$trans == 1, ]
      
      # Binding newly infected rows to the infected list if there are any new infected individuals
        if (nrow(newlyinf) > 0) {
        # Eliminating potential duplicate sources of infection resulting from the same person infecting their alter through multiple pathways
          newlyinf <- newlyinf[!duplicated(newlyinf[c(1, 2)]),]
        
        # Formatting to Add Time to Recovery & and Latency Times to Newly Infected Nodes
          newlyinf <- newlyinf[c(1,2)]
          newlyinf <- cbind(seq(1,nrow(newlyinf),1), newlyinf)
          colnames(newlyinf) <- c('Obs_Id', 'source', 'node')
        
        # Adding Node Characteristics
          newlyinf <- merge(newlyinf, nodes, by.x = "node", all.x = TRUE, all.y = FALSE)
          newlyinf <- newlyinf[order(newlyinf$Obs_Id), ]
          
        # Formatting to Bind to the Infected List
          newlyinf <- newlyinf[c(1, 5, 6, 3)]
          
        # Adding Infection Time 
          inft <- rep(i, nrow(newlyinf))
          newlyinf <- cbind(inft, newlyinf)
          
        # Adjusting Infection Latency and Time to Recovery to Account for the Infection Time
          newlyinf$tlat <- newlyinf$inft + newlyinf$tlat
          newlyinf$trec <- newlyinf$trec + newlyinf$tlat
          rm(inft)
          
        # Adding newly infected to the infected list
          infected <- rbind(infected, newlyinf)
          rownames(infected) <- seq(1, nrow(infected), 1)
          
        # Getting rid of potential duplicate sources based on time (earlier times selected)
          node_id <- sort(unique(infected$node))
          node_set <- vector('list', length(node_id))
          names(node_set) <- node_id
          for (j in seq_along(node_set)) {
            node_set[[j]] <- infected[infected$node == node_id[[j]], ]
            node_set[[j]] <- node_set[[j]][order(node_set[[j]]$inft), ]
            node_set[[j]] <- node_set[[j]][1, ]
          } 
          infected <- do.call(rbind, node_set)
          infected <- infected[order(infected$inft), ]
          rownames(infedge) <- seq(1, nrow(infedge), 1)
          rm(node_id, node_set)
        
        # Updating the Infection Networks List
          inf_networks[[i]] <- infected[c(1, 5, 2)]
          inf_networks[[i]] <- inf_networks[[i]][inf_networks[[i]]$source != 0, ]
          inf_networks[[i]] <- inf_networks[[i]][order(inf_networks[[i]]$source, inf_networks[[i]]$node), ]

        # Updating Infection Networks Attributes List
          inf_attributes[[i]] <- infected[c(1,5,2)]
          inf_nodes <- sort(unique(c(inf_attributes[[i]]$source, inf_attributes[[i]]$node)))
          inf_nodes <- inf_nodes[inf_nodes != 0]
          node_colors <- vector('character', length(inf_nodes))
          for (j in seq_along(node_colors)) {
            if (inf_nodes[[j]] %in% inf_networks[[i]]$source) {
              node_colors[[j]] <- c('brown')
            } else if (inf_nodes[[j]] %notin% inf_networks[[i]]$source & inf_nodes[[j]] %notin% inf_networks[[i]]$node) {
              node_colors[[j]] <- c('blue')
            } else {
              node_colors[[j]] <- c('white')
            }
          }
          
          inf_attributes[[i]] <- as.data.frame(cbind(inf_nodes, node_colors))
          colnames(inf_attributes[[i]]) <- c('node', 'color')
          rm(inf_nodes, node_colors)
      }else{
          infected <- infected[ , ]
      }
        
      # Printing List of Infected Individuals
        if (printt == 1) {
          print(paste('Updated Time', i, 'Infections'))
          Sys.sleep(0.1)
          print(infected)
          flush.console() 
        } else {
          infected <- infected[ , ]
        }
        
        rm(infedge, newlyinf, relations_list)
        
##################
#    RECOVERED   #  
##################
        
      # Identify individuals who are eligible for death or recovery
        rset <- infected[infected$trec == i, ]
        rset <- nodes[nodes$node %in% rset$node, ]
        
      # If any one is eligible for death/recovery, then initiate the recovery process and update.
        if (nrow(rset) != 0) {
        # Simulating recovery/death as a weighted draw from a Bernoulli distribution based on the node's recover_prob
          recovery_outcomes <- vector('numeric', nrow(rset))
          for (j in seq_along(recovery_outcomes)) {
            recovery_outcomes[[j]] <- rbinom(1, 1, rset[j,5])
          }
          rset <- cbind(rset,recovery_outcomes)
        
        # Isolating the dead from the recovered, eliminating the dead from active edges, and from the sus_set
          recovered <- rset[rset$recovery_outcomes == 1, ]
          if (nrow(rset) != nrow(recovered)) {
            dead <- rset[rset$recovery_outcomes != 1, ]
            edges <- edges[edges$n1 %notin% dead$node, ]
            edges <- edges[edges$n2 %notin% dead$node, ]
            sus_set <- sus_set[sus_set %notin% dead$node]
            rm(dead)
          }else{
            rset <- rset
          }
        
        # Updating Infection History
          rset <- infected[infected$trec == i, ]
          rset$die <- abs(recovery_outcomes - 1)
          rset <- rset[c(2, 1, 3, 4, 6, 5)]
          InfHistStackUpdate <- rbind(InfHistStackUpdate, rset)
          
        # Updating the infected list
          infected <- infected[infected$trec != i, ]
          rm(recovery_outcomes)
      }else{
          rset <- rset[,]
        }

###################
#   SUSCEPTIBLE   #
###################
    
      # If there are recovered and/or dead cases, determine their future susceptibility
        if (nrow(rset) != 0) {
        # SI & SIR Models: Anyone ever infected is no longer susceptible
          if (sitype == "SI" | sitype == "SIR") {
            sus_set <- sus_set[sus_set %notin% recovered$node]
            edges <- edges[edges$n1 %notin% recovered$node, ]
            edges <- edges[edges$n2 %notin% recovered$node, ]
        # SIS: Only those currently infected are not susceptible
          } else if (sitype == "SIS") {
            sus_set <- append(sus_set, recovered$node)
            sus_set <- sort(sus_set)
          }
        rm(recovered)
      }else{
          rset <- rset[,]
        }
        rm(rset)
        
      # Eliminate Dyads with Finish Times Equal to the Iteration Number
        edges <- edges[edges$ftime != i, ]
        
      # Update Susceptible List with Current Infected
        inf_nodes <- sort(unique(cbind(infected$source, infected$node)))
        inf_nodes <- inf_nodes[inf_nodes != 0]
        sus_set <- sus_set[sus_set %notin% inf_nodes]
        rm(inf_nodes)
        
      # Final update of the infection history (i = maxtime)
        if (i == maxtime) {
          infected$die <- 0
          infected <- infected[c(2, 1, 3, 4, 6, 5)]
          InfHistStackUpdate <- rbind(InfHistStackUpdate, infected)
        }else {
          InfHistStackUpdate <- InfHistStackUpdate[ , ]
        }
    }
 
#################################
#   GENERATING VISUALIZATIONS   #
#################################
    
  # GENERATING SUMMARY NETWORK (Used for both Animations and Visualizations)
    
    # Creating Summary Network Edgelist
      edges <- InfHistStackUpdate[c(2,6,1)]
      colnames(edges)[c(2,3)] <- c('n1', 'n2')
      edges$Obs_ID <- seq(1,nrow(edges),1)
    
    # Getting original values and checking that the infected edges are a subset of the all edges
      value <- merge(edges, edges0, by.x = c('n1', 'n2'), all.x = TRUE, all.y = FALSE)
    
    # Formatting edgelist in order to populate the visualization matrix
      edges <- value[order(value$Obs_ID), ]
      edges <- edges[c(3, 1, 2, 5)]
      edges[is.na(edges$value), ][[4]] <- 0
      rm(value)
    
    # Isolating infected nodes for the purposes of populating the visualization matrix
      inf_nodes <- sort(unique(c(edges$n1, edges$n2)))
      inf_nodes <- inf_nodes[inf_nodes != 0]
      inf_nodes <- as.data.frame(cbind(seq(1,length(inf_nodes),1), inf_nodes))
      colnames(inf_nodes) <- c('id', 'node')
    
    # Creating sna networks to isolate network components
      sna_network <- network::network.initialize(nrow(inf_nodes))
    
    # Mapping Sequential IDs
      el <- edges[edges$n1 != 0, ]
      el <- el[c(2,3)]
      el$Obs_ID <- seq(1, nrow(el), 1)
      source <- el[c(3, 1)]
      colnames(source)[[2]] <- colnames(inf_nodes)[[2]]
      source <- merge(source, inf_nodes, by.x = "node", all.x = TRUE, all.y = FALSE)
      source <- source[order(source$Obs_ID), ]
      source <- source[c(3)]
      colnames(source) <- c('source')
      target <- el[c(3, 2)]
      colnames(target)[[2]] <- colnames(inf_nodes)[[2]]
      target <- merge(target, inf_nodes, by.x = "node", all.x = TRUE, all.y = FALSE)
      target <- target[order(target$Obs_ID), ]
      target <- target[c(3)]
      colnames(target) <- c('target')
      el <- as.data.frame(cbind(source, target))
      el[] <- lapply(el, as.character)
      sna_network <- network::add.edges(sna_network, el[[1]],el[[2]])
      rm(source, target)
    
    # Plotting Summary Network to Get Coordinates
      par(mar = c(0,0,0,0),  family='HersheySerif')
      sna_plot <- plot(sna_network, mode = "kamadakawai", 
                       edge.col='grey', arrowhead.cex=0.25, edge.lwd = 0.5, 
                       vertex.col=c('brown'), vertex.cex=0.25, 
                       object.scale=0.02, vertices.last=TRUE, displayisolates = TRUE)
      x <- sna_plot[,1]
      y <- sna_plot[,2]
      inf_nodes <- cbind(inf_nodes, x, y)
      rm(sna_plot, x, y)
      
    # Outputting Summary Network's SNA File and plot to the global environment
      if (R_net == 1) {
        sna_infection_network <-  assign(x = "sna_infection_network", value = sna_network,.GlobalEnv) 
        
        out_degree <- sna::degree(sna_network, cmode="outdegree")
        out_degree <- out_degree + 1
        
        png("p_1.png", width = 899, height = 546)
          par(mar = c(0,0,0,0),  family='HersheySerif')
          plot(sna_network, mode = "kamadakawai", 
              edge.col='grey', arrowhead.cex=0.25, edge.lwd = 0.5, 
              vertex.col=c('brown'), vertex.cex=out_degree * 0.15, 
              object.scale=0.02, vertices.last=TRUE, displayisolates = TRUE)
          title('Infection Network', family='HersheySerif', cex.main=1.5, line=-3, adj=0)
        dev.off()
        
        g <- magick::image_read('p_1.png')
        file.remove('p_1.png')
        p_1 <- ggplotify::as.ggplot(g)
        sna_infection_plot <-  assign(x = "sna_infection_plot", value = p_1,.GlobalEnv)
        rm(out_degree, g, p_1)
      }else{
        sna_network <- sna_network
      }

    # Generating Pajek Outputs for Summary Network
      if (Pajek_net == 1){
        paj_net <- vector('list',4)
        vertices <- paste('*Vertices', nrow(inf_nodes))
        paj_net[[1]] <- vertices
        node_names <- gsub("\\b", '"', inf_nodes$node, perl=T)
        paj_net[[2]] <- paste(inf_nodes$id, node_names)
        paj_net[[3]] <- c('*Arcs')
        paj_net[[4]] <- paste(el$source, el$target)
        vertices <- append(paj_net[[1]], paj_net[[2]])
        arcs <- append(paj_net[[3]], paj_net[[4]])
        paj_net <- append(vertices, arcs)
        
        fileConn <- file(paste0('Infection_Network', ".net"))
        writeLines(unlist(paj_net), fileConn)
        close(fileConn)
        rm(paj_net, vertices, arcs, node_names, fileConn)
        
      }else{
        inf_nodes <- inf_nodes[,]
        el <- el[,]
      }
      rm(el)
     
  # INFECTION NETWORKS OVER TIME
        
    # Eliminating Null Networks & Attributes (Time Periods Where No New Infected Edges Were Added)
      inf_networks <- inf_networks[which(!sapply(inf_networks, is.null))]
      inf_attributes <- inf_attributes[which(!sapply(inf_attributes, is.null))]
      
    # Adding Latency Time to inf_attributes for the purposes of dynamic coloring
      for (j in seq_along(inf_attributes)) {
        t_nodes <- as.numeric(inf_attributes[[j]]$node)
        t_nodes <- InfHistStackUpdate[InfHistStackUpdate$node %in% t_nodes, ][c(1,4)]
        t_nodes$node <- as.character(t_nodes$node)
        t_nodes$inft <- as.numeric(gsub("[^0-9.]", "",  names(inf_attributes)[[j]]))
        inf_attributes[[j]] <- merge(inf_attributes[[j]], t_nodes, by.x = "node", all.x = TRUE, all.y = FALSE)
        rm(t_nodes)
      }
      
    # Stacking Attributes to Create Unfolding Network Visualization
      inf_a_stack <- vector('list', length(inf_attributes))
      for (j in 2:length(inf_a_stack)) {
        inf_a_stack[[j]] <- do.call(rbind, inf_attributes[c(1:j)])
      }
      names(inf_a_stack) <- names(inf_attributes)
      inf_a_stack[[1]] <- inf_attributes[[1]]
      inf_attributes <- inf_a_stack
      rm(inf_a_stack)
      
    # Remove duplicate nodes from the attributes file
    # (node, trec)
      for (j in seq_along(inf_attributes)) {
        inf_attributes[[j]] <- inf_attributes[[j]][order(-inf_attributes[[j]]$inft), ]
        inf_attributes[[j]] <- inf_attributes[[j]][!duplicated(inf_attributes[[j]][c("node","trec")]), ]
      }
      
    # Color nodes in the attributes file that exceed their recovery time white
      for (j in seq_along(inf_networks)) {
        inf_attributes[[j]]$color <- ifelse(inf_attributes[[j]]$trec <= inf_attributes[[j]]$inft, 'white', inf_attributes[[j]]$color)
      }
      
    # Adding xy coordinates to node attributes file
      for (j in seq_along(inf_attributes)) {
        t_nodes <- as.numeric(inf_attributes[[j]]$node)
        t_nodes <- inf_nodes[inf_nodes$node %in% t_nodes, ][c(2:4)]
        t_nodes$node <- as.character(t_nodes$node)
        inf_attributes[[j]] <- merge(inf_attributes[[j]], t_nodes, by.x = "node", all.x = TRUE, all.y = FALSE)
        rm(t_nodes)
      }
      
    # Applying Sequential Node Ids (Expected by Pajek, and easier to process by sna)
      for (j in seq_along(inf_attributes)) {
        id <- seq(1, nrow(inf_attributes[[j]]), 1)
        inf_attributes[[j]] <- cbind(id, inf_attributes[[j]])
        rm(id)
      }
    
    # Creating Stacked Edgelists Based on Infection History
      inf_n_stack <- vector('list', length(inf_networks))
      names(inf_n_stack) <- names(inf_networks)
      for (j in 1:length(inf_n_stack)) {
        t_edges <- InfHistStackUpdate[InfHistStackUpdate$inft <= j, ]
        t_edges <- t_edges[c(2,6,1)]
        t_edges <- t_edges[t_edges$source != 0, ]
        inf_n_stack[[j]] <- t_edges
        rm(t_edges)
      }
      inf_networks <- inf_n_stack
      rm(inf_n_stack)
      
    # Mapping Sequential IDs to Edgelist
      inf_networks <- lapply(inf_networks, function(x) x[c(2:3)])
      for (j in seq_along(inf_networks)){
        inf_networks[[j]][] <- lapply(inf_networks[[j]], as.character)
        inf_networks[[j]]$Obs_ID <- seq(1, nrow(inf_networks[[j]]), 1)
      }
      
      for (j in seq_along(inf_networks)){
        source <- inf_networks[[j]][c(1,3)]
        colnames(source)[[1]] <- colnames(inf_attributes[[j]])[[2]]
        source <- merge(source, inf_attributes[[j]], by.x = "node", all.x = TRUE, all.y = FALSE)
        source <- source[order(source$Obs_ID), ]
        source <- source[c(3)]
        colnames(source) <- colnames(inf_networks[[j]])[[1]]
      
        target <- inf_networks[[j]][c(2, 3)]
        colnames(target)[[1]] <- colnames(inf_attributes[[j]])[[2]]
        target <- merge(target, inf_attributes[[j]], by.x = "node", all.x = TRUE, all.y = FALSE)
        target <- target[order(target$Obs_ID), ]
        target <- target[c(3)]
        colnames(target) <- colnames(inf_networks[[j]])[[2]]
        inf_networks[[j]] <- cbind(source, target)
        inf_networks[[j]] <- inf_networks[[j]][order(inf_networks[[j]]$source, inf_networks[[j]]$node), ]
      
        inf_networks[[j]][] <- lapply(inf_networks[[j]], as.character)
        rm(source, target)
      }
      rm(sus_set)
    
    # Generating sna network objects if R_net = 1
      if (R_net == 1) {
      # Create sna style networks list
        sna_networks <- vector('list', length(inf_networks))
        names(sna_networks) <- names(inf_networks)
        for (j in seq_along(sna_networks)) {
          sna_networks[[j]] <- network.initialize(length(inf_attributes[[j]]$node))
        }
        for (j in seq_along(sna_networks)) {
          network.vertex.names(sna_networks[[j]]) <- inf_attributes[[j]]$node
        }
        
      # Adding Edges
        for (j in seq_along(sna_networks)) {
          sna_networks[[j]] <- network::add.edges(sna_networks[[j]], inf_networks[[j]]$source, inf_networks[[j]]$node)
        }
          
      # Calculate Out-Degree
        degree_list <- vector('list', length(sna_networks))
        names(degree_list) <- names(sna_networks)
        for(j in seq_along(degree_list)) {
          degree_list[[j]] <- sna::degree(sna_networks[[j]], cmode="outdegree")
          degree_list[[j]] <- (degree_list[[j]] + 1)
        }

      # Add Node Colors
        for (j in seq_along(sna_networks)) {
          network::set.vertex.attribute(sna_networks[[j]],"node_color", inf_attributes[[j]]$color) 
        }
        
      # Generate Network Plots for each Time Period When New Infections Occurred
        InfNetwork_Time_Plots <- vector('list', length(sna_networks))
        names(InfNetwork_Time_Plots) <- names(sna_networks)
        for (j in seq_along(InfNetwork_Time_Plots)) {
          png("p_1.png", width = 877, height = 676)
            par(mar = c(0,0,0,0),  family='HersheySerif')
            plot(sna_networks[[j]], coord=inf_attributes[[j]][c(6:7)], 
               edge.col='grey', arrowhead.cex=0.25, edge.lwd = 0.25, 
               vertex.col=inf_attributes[[j]]$color, vertex.cex=(degree_list[[j]]*0.15), #label = network.vertex.names(sna_networks[[j]]),#
               object.scale=0.02, vertices.last=TRUE, displayisolates = TRUE)
            title( paste(names(inf_networks)[[j]], 'Infection Network')  , family='HersheySerif', cex.main=1.5, line=-3, adj=0)
          dev.off()
        
          g <- magick::image_read('p_1.png')
          file.remove('p_1.png')
          p_1 <- ggplotify::as.ggplot(g)
          InfNetwork_Time_Plots[[j]] <- p_1
          rm(g, p_1)
          Sys.sleep(1)
        }
        rm(degree_list)
        
      # Outputting sna Temporal Networks
        sna_timeslice_networks <-  assign(x = "sna_timeslice_networks", value = sna_networks,.GlobalEnv) 
        
      # Create HTML Movie showing changes in the infection network over time
        animation::saveHTML({
          for(j in seq_along(InfNetwork_Time_Plots)) {                      
            plot(InfNetwork_Time_Plots[[j]], add=T)
          }
        }, htmlfile = "InfectonNetwork_OverTime.html", autoplay=FALSE, verbose=FALSE,
        ani.width = 877, ani.height=676, nmax=length(InfNetwork_Time_Plots))
      }else{
        inf_networks <- inf_networks
      }
      
     # Generating Pajek network files and macro if Pajek = 1
      if (Pajek_net == 1){
        # Moving Data into Pajek Format
          paj_networks <- vector('list', length(inf_networks))
          names(paj_networks) <- names(inf_networks)
          for (j in seq_along(paj_networks)) {
           paj_net <- vector('list',4)
            vertices <- paste('*Vertices', nrow(inf_attributes[[j]]))
            paj_net[[1]] <- vertices
            ic <- rep('ic', nrow(inf_attributes[[j]]))
            bc <- rep('bc', nrow(inf_attributes[[j]]))
            space <- rep("   ", nrow(inf_attributes[[j]]))
            id <- seq(1, nrow(inf_attributes[[j]]), 1)
            colors <- paste(toupper(substring(inf_attributes[[j]]$color, 1,1)), substring(inf_attributes[[j]]$color, 2), sep="", collapse=" ")
            colors <- strsplit(colors, ' ')[[1]]
            node_names <- gsub("\\b", '"', inf_attributes[[j]]$node, perl=T)
            paj_net[[2]] <- paste(id, node_names, round(scales::rescale(inf_attributes[[j]]$x, to=c(0.05,0.95)), digits=3), round(scales::rescale(inf_attributes[[j]]$y,to=c(0.05,0.95)), digits=3), space, ic, colors, bc, 'Black', sep = ' ')
            paj_net[[3]] <- c('*Arcs')
            paj_net[[4]] <- paste(inf_networks[[j]]$source, inf_networks[[j]]$node, sep = ' ')
            vertices <- append(paj_net[[1]], paj_net[[2]])
            arcs <- append(paj_net[[3]], paj_net[[4]])
            paj_networks[[j]] <- append(vertices, arcs)
            rm(paj_net, vertices, ic, bc, id, arcs, space, colors, node_names)
          }

          # Calculating Out-Degree for each node in each temporal network
            degree_list <- vector('list', length(paj_networks))
            names(degree_list) <- paste0('Time ',seq(1, length(degree_list), 1),': Out-Degree' )
            for (j in seq_along(degree_list)) {
              sources <- inf_attributes[[j]][inf_attributes[[j]]$id %in% inf_networks[[j]]$source, ]
              sources <- sort(unique(sources$id))
              targets <- inf_attributes[[j]][inf_attributes[[j]]$id %notin% inf_networks[[j]]$source, ]
              counts <- vector('numeric', length(sources))
              for (k in seq_along(counts)) {
                source <- inf_networks[[j]][inf_networks[[j]]$source == sources[[k]], ]
                counts[[k]] <- nrow(source)
                rm(source)
              }
              targets <- cbind(targets$id,rep(0, nrow(targets)))
              sources <- cbind(sources, counts) 
              out_degree <- as.data.frame(rbind(sources, targets))
              out_degree <- out_degree[order(out_degree$sources), ]
              out_degree$counts <- as.numeric(out_degree$counts) + 1 #Avoiding zero values for the purposes of visualization
              out_degree <- append(paste('*Vertices', nrow(out_degree)), out_degree$counts)
              degree_list[[j]] <- out_degree 
              rm(sources, targets, counts, out_degree)
            }
        
          # Writing Each Network and out-degree partition to a .paj File
            paj_file <- vector('list', length(paj_networks))
            for (j in seq_along(paj_file)) {
              net_name <- paste0('*Network ', names(paj_networks)[[j]])
              net_name <- append(net_name, " ")
              paj_net <- paj_networks[[j]]
              paj_net <- append(net_name, paj_net)
              paj_net <- append(paj_net, " ")
              paj_file[[j]] <- paj_net
              rm(net_name, paj_net)
            }
          
            partition_list <- vector('list', length(degree_list))
            for (j in seq_along(paj_file)) {
              clu_name <- paste0('*Vector ', names(inf_networks)[[j]], ', ', nrow(inf_attributes[[j]]), ' vertices')
              clu_name <- append(clu_name, " ")
              out_degree <- degree_list[[j]]
              out_degree <- append(clu_name, out_degree)
              out_degree <- append(out_degree, " ")
              partition_list[[j]] <- out_degree
              rm(clu_name, out_degree)
            }
        
            paj_file <- append(paj_file, partition_list)
        
            fileConn <- file(paste0('Infection_Nets', ".paj"))
            writeLines(unlist(paj_file), fileConn)
            close(fileConn)
            rm(paj_networks, partition_list, fileConn)
          
          # Creating directory to store Pajek images
            dir.create("paj_files")
        
          # Writing Macro for produce images for each time slice to animate
            paj_address <- list.files(pattern="*.paj",   full.names = TRUE, path=getwd())
            paj_address <- chartr("/", "\\", paj_address)
            net_list <- seq(1, length(inf_networks), 1)
            image_dir <- paste0(getwd(), "/", 'paj_files','/')
            image_dir <- chartr("/", "\\", image_dir)
            paj_macro <- vector('list', (9 + length(inf_networks)))
            paj_macro[[1]] <- c('NETBEGIN 1')
            paj_macro[[2]] <- c('CLUBEGIN 1')
            paj_macro[[3]] <- c('PERBEGIN 1')
            paj_macro[[4]] <- c('CLSBEGIN 1')
            paj_macro[[5]] <- c('HIEBEGIN 1')
            paj_macro[[6]] <- c('VECBEGIN 1')
            paj_macro[[7]] <- c(' ')
            paj_macro[[8]] <- paste0('% Reading Pajek Project File  ---    ',paj_address)
            paj_macro[[9]] <- paste('N 9999 RDPAJ', paste0('\"', paj_address, '\"'))
            
            for (j in 10:(9 + length(inf_networks))) {
              k <- j - 9
              image <- paste0(image_dir,'net_',k,'.jpg')
              image <- paste0("\"",image, "\"")
              paj_element <- vector('list', 2)
              paj_element[[1]] <- paste('E', k, 'DRAW 0', k, '0 0 0')
              paj_element[[2]] <- paste('E', k, 'JPEG 0', k, '0 0 0', image, '100 0')
              paj_element <- unlist(paj_element)
              paj_macro[[j]] <- paj_element
              rm(k, image, paj_element)
            }
            
            paj_macro <- unlist(paj_macro)
            
            # Writing-out Macro
            fileConn <- file(paste0('Infection_Nets', ".mcr"))
            writeLines(paj_macro, fileConn)
            close(fileConn)
            rm(paj_address, net_list, image_dir, fileConn, degree_list)
      }else{
        inf_networks <- inf_networks
      }
    
  # CELLULAR AUTOMATA: LATTICE GRAPH
    if (cellular_net == 1 & dim(coordinates)[[2]] >= 3) {
      # Isolating Connected Components from Summary Network
        cd <- sna::component.dist(sna_network, connected="weak")
        sna_network %v% "component" <- cd$membership   
        inf_nodes$component <- cd$membership    
        rm(cd)
        
      # Calculate Out-Degree 
        out_degree <- sna::degree(sna_network, cmode="outdegree")
        inf_nodes$out_degree <- out_degree
        rm(out_degree)
        
      # Creating Components List for the Purposes of Plotting the Sub-Networks
        components_list <- vector('list', length(unique(inf_nodes$component)))
        names(components_list) <- unique(inf_nodes$component)
        for (j in seq_along(components_list)) {
          components_list[[j]] <- inf_nodes[inf_nodes$component == j, ]
        }
        
      # Sort by Out-Degree
        for (j in seq_along(components_list)){
          components_list[[j]] <- components_list[[j]][order(-components_list[[j]]$out_degree), ]
        }
        
      # Adding Color and Coordinates to the Infected Nodes List
        component_colors <- brewer.pal(n = 11, name = 'RdYlBu')
        component_colors <- component_colors[c(11, 5, 2, 10, 4, 3, 9, 7, 8, 6)]
        component_colors <- c('brown', component_colors)
        component_colors <- as.data.frame(component_colors[1:length(components_list)])
        component_colors <- cbind(seq(1, length(components_list), 1), component_colors)
        component_colors$shape <- c(16)
        colnames(component_colors)[1:2] <- c('component', 'color')
        components_list <- do.call(rbind, components_list)
        colnames(components_list)[c(3:4)] <- c('plot_x', 'plot_y')
        components_list <- merge(components_list, component_colors, by.x = "component", all.x = TRUE, all.y = FALSE)
        components_list <- merge(components_list, coordinates, by.x = "node", all.x = TRUE, all.y = FALSE)
        components_list <- components_list[c(1,9,10,7,8)]
        coordinates <- merge(coordinates, components_list, by.x = c('node','x', 'y'), all.x = TRUE, all.y = FALSE)
        for (j in seq_along(coordinates$node)){
          if(is.na(coordinates[j,4]) == TRUE){
            coordinates[j,4] = 'black'
          }else{
            coordinates[j,4] =  coordinates[j,4]
          }
        }
        for (j in seq_along(coordinates$node)){
          if(is.na(coordinates[j,5]) == TRUE){
            coordinates[j,5] = 1
          }else{
            coordinates[j,5] =  coordinates[j,5]
          }
        }
        
      # Getting 0 Nodes
        zero_nodes <- edges[edges$n1 == 0, ][[3]]
        zero_list <- vector('list', length(zero_nodes)) 
        for(j in seq_along(zero_list)) {
          zero_coord <- coordinates[coordinates$node == zero_nodes[[j]], ]
          zero_coord$shape <- 15
          zero_list[[j]] <- zero_coord
          rm(zero_coord)
        }
        zero_list <- do.call(rbind, zero_list)
        coordinates <- rbind(zero_list, coordinates)
        coordinates <- coordinates[!duplicated(coordinates$node),]
        rm(zero_nodes, zero_list)
        
      # Plot Nodes from the Coordinates List
        png("p_1.png", width = 877, height = 676)
          layout(rbind(1,2), heights=c(9,1))  # put legend on bottom 1/8th of the chart
          par(mar = c(1,1,1,1),  family='HersheySerif')
          plot(type='n', x=dim(cellular_data)[[1]], y=dim(cellular_data)[[2]], xlim=c(1, dim(cellular_data)[[1]]), 
             ylim=c(1, dim(cellular_data)[[2]]), axes=FALSE, xlab=' ', ylab= '', bty='n')
        
          for(j in seq_along(coordinates$node)) (
            points(x=coordinates[j,2], y=coordinates[j,3], pch=coordinates[j,5], col=coordinates[j,4], cex=2)
          )
        
          edges <- edges[edges$value != 0, ]
          for ( j in seq_along(edges$n1)) {
            edge <- edges[j,c(2,3)]
            edge_list <- vector('list', 2)
              edge_list[[1]] <- coordinates[coordinates$node == edge[[1]], ][c(1:3)]
              edge_list[[2]] <- coordinates[coordinates$node == edge[[2]], ][c(1:3)]
              
              if(edge_list[[1]][[3]] == edge_list[[2]][[3]] & edge_list[[1]][[2]] >  edge_list[[2]][[2]]){
                edge_list[[1]][[2]] <- (edge_list[[1]][[2]] - 0.2)
                edge_list[[2]][[2]] <- (edge_list[[2]][[2]] + 0.2) 
              }else if (edge_list[[1]][[2]] == edge_list[[2]][[2]] & edge_list[[1]][[3]] > edge_list[[2]][[3]]) {
                edge_list[[1]][[3]] <- (edge_list[[1]][[3]] - 0.2)
                edge_list[[2]][[3]] <- (edge_list[[2]][[3]] + 0.2) 
              } else if (edge_list[[1]][[2]] == edge_list[[2]][[2]] & edge_list[[1]][[3]] < edge_list[[2]][[3]]) {
                edge_list[[1]][[3]] <- (edge_list[[1]][[3]] + 0.2)
                edge_list[[2]][[3]] <- (edge_list[[2]][[3]] - 0.2)
              } else {
                edge_list[[1]][[2]] <- (edge_list[[1]][[2]] + 0.2)
                edge_list[[2]][[2]] <- (edge_list[[2]][[2]] - 0.2) 
              }
            
            arrows(edge_list[[1]]$x, edge_list[[1]]$y, edge_list[[2]]$x, edge_list[[2]]$y, 
                 length = 0.09, col='grey42', code=2, lwd=2.5)
            
            rm(edge, edge_list)
          }
          
            # Adding Legend
              par(mar=c(0, 0, 0, 0))
              plot.new()
              legend('center','groups',c("Seeds", "Nodes"), pch=c(0, 1), ncol=2,  cex=1.3, bty ="n")
          dev.off()
          
          g <- magick::image_read('p_1.png')
          file.remove('p_1.png')
          p_1 <- ggplotify::as.ggplot(g)
          
          lattice_plot <-  assign(x = "lattice_plot", value = p_1,.GlobalEnv) 
          rm(g, p_1)
        }else{
          InfHistStackUpdate <- InfHistStackUpdate[,]
        } 
  
  # INFECTION GROWTH CURVE
      
    # Outputting Infection History
      rownames(InfHistStackUpdate) <- seq(1,nrow(InfHistStackUpdate),1)
      infection_history <-  assign(x = "infection_history", value = InfHistStackUpdate,.GlobalEnv) 
    
    # Calculating Counts for each Infection Time Period
      times <- sort(unique(InfHistStackUpdate$inft))
      counts <- vector('integer', length(times))
      count <- 0
      for(j in seq_along(counts)){
        t_count <- nrow(InfHistStackUpdate[InfHistStackUpdate$inft == times[[j]], ])
        count <- count + t_count
        counts[[j]] <- count
        rm(t_count)
      }
      infection_counts <- as.data.frame(cbind(times, counts))
      colnames(infection_counts) <- c('int', 'count')
      rm(times, counts, count)
      
    #Plotting
      png("p_1.png", width = 769, height = 526)
        par(mar = c(4.1, 5, 4.1, 2.1), bty='n', family='HersheySerif')
        plot(x=infection_counts[[1]], y=infection_counts[[2]], xlim=c(min(pretty(infection_counts[[1]])), max(pretty(infection_counts[[1]]))),
             ylim=c(min(pretty(infection_counts[[2]])), max(pretty(infection_counts[[2]]))),lwd = 2.3, 
             main=" ", xlab = " ", ylab = " ", family = 'HersheySerif', type='n',
             col = 'black',  cex.lab=1.5, xaxt = 'n', axes=FALSE)
        
        grid(lwd = 2)
        
        lines(infection_counts[[1]], infection_counts[[2]], lwd = 1, col = 'blue')

        points(infection_counts[[1]], infection_counts[[2]], pch=21, col='black', bg='grey')
        
        axis(side=1, pretty(infection_counts[[1]]), cex.axis= 1.3, family = 'HersheySerif', las=1)
        axis(side=2, pretty(infection_counts[[2]]), cex.axis= 1.3, family = 'HersheySerif', las=1)

        mtext(side = 1, line = 2.5, 'Infection Time', cex=1.5)
        mtext(side = 2, line = 3.5, 'Cumulative Count', cex=1.5)
      dev.off()
        
      g <- magick::image_read('p_1.png')
      file.remove('p_1.png')
      p_1 <- ggplotify::as.ggplot(g)
      
      growth_curve_plot <-  assign(x = "growth_curve_plot", value = p_1,.GlobalEnv) 
      rm(g, p_1)
      
  } else {
    print("Failed File Checks, Please Examine Log")
  }
}
  
#############
#   NOTES   #
#############
  
# Example: Calculating Transition Probabilities
  
  # The probability of infection for one exposure instance of one hour is 0.25
  # 3 exposures occurred
  # Each exposure instance was 2 hours
  # Effectively 6 exposures (3 * 2)
  
  # Transmission Probability = 1-((1-0.25)##(3*2))
  # Transmission Probability = 1-(0.75)##6
  # Transmission Probability =1-(0.17798)
  # Transmission Probability = 0.82202
  
  # probtran <- 1-((1-0.25)**(3*2))
  
  # Example where each exposure is calculated separately
  # trans <- vector('numeric', 1000)
  # for (i in seq_along(trans)) {
  #  t_iter <- vector('numeric', 6)
  #  for (j in seq_along(t_iter)) (
  #    t_iter[[j]] <- rbinom(1, 1, 0.25)
  #  )
  #  trans[[i]] <- ifelse(sum(t_iter) == 0, 0, 1)
  #  rm(t_iter)
  # }
  # mean(trans)
  
  # Example where the Combined Transmission Probability Is Used
  # trans <- vector('numeric', 1000)
  # for (i in seq_along(trans)) {
  #  trans[[i]] <- rbinom(1, 1, 0.82202)
  # }
  # mean(trans)
  
  # We get nearly equivalent outcomes which is as expected.

