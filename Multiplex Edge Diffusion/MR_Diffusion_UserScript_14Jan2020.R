#Multiple-Relation Diffusion Simulation User Script
#Jonathan H. Morgan
#23 December 2020

#Setting Work Directory
setwd("~/Desktop/DNAC/IDEANet/Data_Scripts/MR_EdgeDiffusion")
getwd()

#Options
options(stringsAsFactors = FALSE)

source("~/Desktop/DNAC/IDEANet/Data_Scripts/MR_EdgeDiffusion/MR_DiffusionSimulator_23Dec2020.R")

##############################
#   CELLULAR AUTOMATA DATA   #
##############################

# Note: Creating Cellular automata data for demonstration purposes. 
#       The simulation, in itself, is not limited to this kind of network.
#       For other kinds of networks or when doing a SIS model, make cellular_net 0.
#       The cellular automata visualization assumes that each node can have at max 4 alters.

# Creating Matrix
  cellular_data <- matrix(seq(1, 900, 1), nrow=30, ncol=30)

# Retrieving x,y Coordinates
  focus <- seq(1, 900, 1)
  coordinates <- vector('list', length(focus))
  names(coordinates) <- focus
  for (i in seq_along(focus)) {
    xy_coords <- which(cellular_data==focus[[i]], arr.ind=TRUE)
    xy_coords <- as.data.frame(xy_coords)
    colnames(xy_coords) <- c('x', 'y')
    coordinates[[i]] <- xy_coords
    rm(xy_coords)
  }
  coordinates <- do.call(rbind, coordinates)
  coordinates <- cbind(focus, coordinates)
  colnames(coordinates)[[1]] <- c('node')
  
# Creating Edgelist
  edges <- vector('list', length(focus))
  for(i in seq_along(edges)) {
    node <- coordinates[coordinates$node == focus[[i]], ]
    n_coord <- vector('list', 4)
      n_coord[[1]] <- c(node[[2]], node[[3]] + 1)
      n_coord[[2]] <- c(node[[2]] + 1, node[[3]])
      n_coord[[3]] <- c(node[[2]], node[[3]] - 1)
      n_coord[[4]] <- c(node[[2]] - 1, node[[3]])
    n_coord <- do.call(rbind, n_coord)
    n_coord <- n_coord[ n_coord[,1] != 0 & n_coord[,2] != 0, ]
    n_coord <- n_coord[ n_coord[,1] <= 30 & n_coord[,2] <= 30, ]
    alters <- vector('numeric', nrow(n_coord))
    for(j in seq_along(n_coord[,1])) {
      row_coord <- n_coord[j,]
      alter <- cellular_data[row_coord[[1]], row_coord[[2]]]
      alters[[j]] <- alter
      rm(row_coord, alter)
    }
    alters <- cbind(rep(node$node,length(alters)), alters)
    colnames(alters)[[1]] <- c('node')
    alters <- as.data.frame(alters)
    edges[[i]] <- alters
    rm(node, alters, n_coord)
  }
  edges1 <- do.call(rbind, edges)
  rm(edges)
  
# Adding Edge Values (Doing a Simple Value of 1 in this Case)
  value <- rep(1, nrow(edges1))
  edges1 <- cbind(edges1, value)
  colnames(edges1)[c(1,2)] <- c('n1', 'n2')
  rm(value)
  
# Creating Test Data                         
# Adding Starting Time, Finish Time, and Type to Simulate a Transmission Process
  stime <- rep(1, nrow(edges1))
  ftime <- round(runif(length(stime), 1, 100), digits=0)
  type <- (rbinom(size=c(1), n=length(stime), prob=0.4)) + 1
  edges1 <- cbind(edges1, stime, ftime, type)
  rm(stime, ftime, type, focus)

#################################
#   CREATING INPUT PARAMETERS   #
#################################

# GENERATING NODES LIST FROM EDGES FILE
  nodes <- unique(c(edges1$n1, edges1$n2))
  seq_nodes <- seq(1, length(nodes), 1)
  nodes <- cbind(seq_nodes, nodes)
  colnames(nodes) <- c('id', 'nodes')
  n <- dim(nodes)[[1]]

# SPECIFYING SIMULATION PARAMETERS
  ittervec <- c(1, 1)                       #Relation specific within-step time adjustment 
  rel_iprob <- c(0.25, 0.25)                #Relation specific weights
  seed <- c(3, 45)                          #Starting Seed Nodes
  spotprob <- matrix(nrow=n, ncol=1, 0.00)  #Vector for spontaneous probability of infection, 0 in this case*/
  durmean <- matrix(nrow=n, ncol=1, 10)     #Mean infection duration - Nx1 vector

  to_change <- sample(which(durmean == 10), 30)
  varied_values <- vector('numeric', length(to_change))
  for (i in seq_along(varied_values)) {
    varied_values[[i]]<- abs(round(rnorm(n=1, mean = 20, sd = 10), digits=0))
  }

  durmean[to_change] <- varied_values       #Adding a bit of variation to the duration times
  durlatent <- matrix(nrow=n, ncol=1, 1)    #Mean latency length - Nx1 vector 

  to_change <- sample(which(durlatent == 1), 30)
  varied_values <- vector('numeric', length(to_change))
  for (i in seq_along(varied_values)) {
    varied_values[[i]]<- abs(round(rnorm(n=1, mean = 10, sd = 1), digits=0))
  }

  durlatent[to_change] <- varied_values     #Adding a bit of variation to the latency times
  recoverprob <- matrix(nrow=n, ncol=1, .5) #Recovery probability, governing the likelihood of death (not used currently)
  sitype <- c("SIR")                        #Susceptibility process: SI, SIS, SIR: SI, SIS, SIR
  randdur <- 0                              #Randomize duration vectors, 0=no
  printt <- 1                               #Iterative steps are printed to the console, 1=yes
  R_net <- 1                                #Generate R network visualizations
  Pajek_net <- 1                            #Generate Pajek network objects and macros
  cellular_net <- 1                         #Generates cellular automata visualization

  rm(to_change, varied_values)

##########################
#   RUNNING SIMULATION   #
##########################

MR_EdgeDiff(edges1, ittervec, rel_iprob, seed, spotprob, durmean, durlatent, 
            recoverprob, sitype='SIR', randdur, printt = 1, R_net = 0, Pajek_net=0, 
            cellular_net=1, coordinates)

#########################
#   EXAMINING OUTPUTS   #
#########################
  
# Infection History
  infection_history
  
# Growth Curve of the Infection
  growth_curve_plot
  
# Summary Infection Network (Network over all Time Periods)
  sna_infection_plot
  
# If the input network was a lattice, then
  lattice_plot
  
# If R_net is 1, then the following sna network object is generated.
  summary(sna_infection_network)
  
# If R_net is 1, then the following list of cumulative time-slice networks is generated.
  sna_timeslice_networks
  
# If Pajek_net is 1, you will find a Pajek net file, 
# Infection_Network.net, of the cumulative infection network.
  
# If Pajek_net is 1, you will find a .paj file that lists the cumulative time-slice networks, 
# with their associated out-degree vectors.
  
# If Pajek_net is 1, you will find a directory, paj_files, that includes Pajek images for animation.
  
# INFECTION OVER TIME ANIMATIONS (PAJEK FILES)
# Note: This functionality is similar to the animations generated when the sna package is used to generate the graphs.
#       The difference is that Pajek images are used 
#       This functionality depends on first generating the plots in Pajek by playing Infection_Nets.mcr.
#       Infection_Nets.mcr is generated by the simulation if Pajek_net = 1 in the function call.

  # Importing Image into a List
    paj_dir <- paste0(getwd(),'/','paj_files')
    image_list <- list.files(pattern="*.jpg", path=paj_dir, full.names = TRUE)
    image_id <- vector('numeric', length(image_list))
    for (i in seq_along(image_id)) {
      matches <- regmatches(image_list[[i]], gregexpr("[[:digit:]]+", image_list[[i]]))
      image_id[[i]] <- as.numeric(unlist(matches))
      rm(matches)
    }
    image_list <- as.data.frame(image_list)
    image_list <- cbind(image_id, image_list)
    image_list <- image_list[order(image_list$image_id), ]
    rm(image_id)
    
    pajek_plots <- vector('list', nrow(image_list))
    names(pajek_plots) <- paste('Time', seq(1, length(pajek_plots), 1))
    for(i in seq_along(pajek_plots)) {
      pajek_plot <- magick::image_read(image_list[i,2])
      png("p_1.png", width = 877, height = 676)
        par(mar = c(0,0,0,0),  family='HersheySerif')
        plot(pajek_plot)
        title(names(pajek_plots)[[i]], family='HersheySerif', cex.main=1.5, line=-1, adj=0)
      dev.off()
      g <- magick::image_read('p_1.png')
      p_1 <- ggplotify::as.ggplot(g)
      file.remove('p_1.png')
      pajek_plots[[i]] <- p_1
      rm(pajek_plot, p_1, g)
    }
    
  # Create HTML Movie showing changes in the infection network over time
    animation::saveHTML({
      for(i in seq_along(pajek_plots)) { 
        plot(pajek_plots[[i]], add=T)
      }
    }, htmlfile = "InfectonNetwork_OverTime.html", autoplay=FALSE, verbose=FALSE,
    ani.width = 877, ani.height=676, nmax=length(pajek_plots))
  
