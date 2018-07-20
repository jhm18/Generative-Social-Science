#Jonathan H. Morgan
#4 July 2018
#GitHub Topology Analyses

#Remove: Removes objects from memory
rm(list = ls())

#Garbage Collection: Frees up memory, but preserves variables created in previous steps
gc()

setwd("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis")  
getwd()

################
#   PACKAGES   #
################

library(readr)        #Import csv and other delimited files
library(haven)        #Import SPSS, SAS, or Stata files
library(magrittr)     #Supports pipe (%>%) commands that allow you to perform multiple operations with one statement
library(dplyr)        #Manipulate data
library(tidyr)        #Additional functions for manipulating data
library(tibble)       #Allows for dynamic renaming of variables while using rlang
library(data.table)   #Older data manipulation tool, useful for string manipulation 
library(rlang)        #Useful for string manipulation using tidyverse tools
library(readtext)     #Used to read plain text files
library(stringr)
library(reshape)
library(ggplot2)      #Visualizing data
library(gridExtra)    #Supports facet plots
library(statnet)      #Network Analysis Software
library(ggnetwork)    #Network Visualization
library(plotly)       #Visualization, density plots
library(pipeR)        #Suppport the layering of visualizations with double pipe commands
library(lubridate)    #Used for the layering of visualizations
library(network)      #Component of Statnet, Useful for Pajek File Conversions
library(gridExtra)
library(grid)
library(ggpubr)       #Helps with arranging multiple plot types in one figure

##################
#    FUNCTIONS   #
##################

Network_Gen <- function(edge_data, source_data, node_data, Node_Count, network, InDegree) {
  
  edge_data <- as.matrix.network(source_data,matrix.type="edgelist")
  edge_data <- as.data.frame(edge_data)
  
  setDT(edge_data)
  cols <- (colnames(edge_data))
  columns <- c("Sender", "Target")
  edge_data <- (setnames(edge_data, old=cols, new=columns))
  
  edge_data <- as.data.frame(edge_data)
  
  node_data <- network.vertex.names(source_data)
  
  node_data <- as.data.frame(node_data)             %>%
    (add_rownames)                                  %>%
    mutate_if(is.character, as.numeric)             %>%
    select(node_data, rowname)              
  
  setDT(node_data)
  cols <- (colnames(node_data))
  columns <- c("Repository_ID", "Network_ID")
  node_data <- (setnames(node_data, old=cols, new=columns))
  
  node_data <- as.data.frame(node_data)             %>%
    mutate_if(is.factor,as.character)               %>%
    mutate_if(is.character, as.numeric)
  
  Node_Count <- NROW(na.omit(node_data))
  
  network <- network.initialize(Node_Count, directed = FALSE) 
  
  edge_data[,1]=as.character(edge_data[,1])
  edge_data[,2]=as.character(edge_data[,2])
  
  add.edges(network, edge_data[,1], edge_data[,2])
  
  network <-  network(network,directed=FALSE) 
  
}

Cluster2_Network <- Network_Gen(Cluster2_Edges, Cluster_2, Cluster2_Nodes, Node_Count, Cluster2_Network, InDegree)

InDegree <- function(InDegree, network) {
  
  InDegree <- degree(network, cmode="indegree")      #Computing the in-degree of each node
  InDegree <- InDegree * .15                                  #Scaling in-degree to avoid high in-degree nodes from crowding out the rest of the nodes
  
}

InDegree <- InDegree(InDegree, Cluster2_Network)


#####################################
#   IMPORTING and FORMATTING DATA   #
#####################################

#GETTING CLUSTER IDs
  #Remove brackets before importing cluster file.

DATA_DIR <- ("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/")
GitHub_Clusters <- readtext(paste0(DATA_DIR, "Nodes.txt"))

GitHub_Clusters <-  as.data.frame(GitHub_Clusters)              %>%
  select(text) 

Clusters <- as.data.frame(GitHub_Clusters)
                               
setDT(Clusters)
cols <- (colnames(Clusters))
columns <- c("ID")
Clusters <- (setnames(Clusters, old=cols, new=columns))

s <- strsplit(Clusters$ID, split = ",")
Clusters <- data.frame(ID = rep(Clusters$ID, sapply(s, length)), Joint_ID = unlist(s))

Clusters <- Clusters    %>%
  select(Joint_ID)      %>%
  separate(Joint_ID, c("Repository_ID", "Cluster_ID"), ":")   
                                
Clusters <- as.data.frame(sapply(Clusters, function(x) gsub("\"", "", x)))

Clusters <- Clusters %>%
  mutate_if(is.factor,as.character)     %>%
  mutate_if(is.character, as.numeric)   %>%
  mutate(Pajek_Cluster_ID = Cluster_ID + 1)

rm(s)

#GETTING FULL NETWORK DATA
GitHub_FullNet <- read.paj("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/sample3587_NetsimileDistanceGraph_Treshold12.net")

GitHub_Edges <- as.matrix.network(GitHub_FullNet,matrix.type="edgelist")

GitHub_Nodes <- network.vertex.names(GitHub_FullNet)

GitHub_Nodes <- as.data.frame(GitHub_Nodes)   %>%
  (add_rownames)                              %>%
  mutate_if(is.character, as.numeric)         %>%
  select(GitHub_Nodes, rowname)              

setDT(GitHub_Nodes)
cols <- (colnames(GitHub_Nodes))
columns <- c("Repository_ID", "Network_ID")
GitHub_Nodes <- (setnames(GitHub_Nodes, old=cols, new=columns))

GitHub_Nodes <- as.data.frame(GitHub_Nodes)   %>%
  mutate_if(is.factor,as.character)           %>%
  mutate_if(is.character, as.numeric)

#GETTING USER REPOSITORY AFFILIATIONS
DATA_DIR <- ("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/")
Repo_Networks <- readtext(paste0(DATA_DIR, "sample3587_Commitgraph_Nofork_Size8to20_Pajekformat.txt"))

Repo_Networks <-  as.data.frame(Repo_Networks)              %>%
  select(text) 

cols <- c("text")
for(col in cols) {
  Repo_Networks <- separate_rows_(Repo_Networks, col, sep = "_")
}

Repo_Networks  <- Repo_Networks[-c(1), ] 

Repo_Networks <-  as.data.frame(Repo_Networks)     %>%
  mutate_if(is.factor,as.character)  

cols <- c("Repo_Networks")
for(col in cols) {
  Repo_Networks <- separate_rows_(Repo_Networks, col, sep = "\\*")
}

#Vertices
vertices <- as.data.frame(Repo_Networks[grep("vertices", Repo_Networks$Repo_Networks),])
setDT(vertices)
cols <- (colnames(vertices))
columns <- c("Vertices")
vertices <- (setnames(vertices, old=cols, new=columns))

cols <- c("Vertices")
for(col in cols) {
  Nodes <- separate_rows_(vertices, col, sep = "ellipse")
}

Nodes <- as.data.frame(sub('vertices*', '', Nodes$Vertices))

setDT(Nodes)
cols <- (colnames(Nodes))
columns <- c("Vertices")
Nodes <- (setnames(Nodes, old=cols, new=columns))

Nodes <- as.data.frame(Nodes)       %>%
  separate(Vertices, c("Index", "Pajek_ID", "User_ID", "x_coord", "y_coord"), " ")

Nodes <- as.data.frame(Nodes)                 %>%
  (add_rownames)                              

s <- as.character(Nodes$Pajek_ID)

s <- as.data.frame(str_split_fixed(s, "\\s+", 2))       %>%
  (add_rownames)    

s <- s         %>%
  select(rowname, V2)

Nodes <- Nodes                      %>%
  left_join(s, by = c("rowname"))

Nodes <- as.data.frame(Nodes)   

Nodes <- Nodes  %>%
  mutate_if(is.factor,as.character)    %>%
  mutate_if(is.character,as.numeric)   %>%
  select(Index, Pajek_ID, User_ID, x_coord, y_coord, V2, rowname)

Nodes <- Nodes %>%
  gather(key = measure, value = ID, V2, Index,
         -Pajek_ID, -User_ID, -x_coord, -y_coord, -rowname, na.rm = TRUE) 

Nodes <- as.data.frame(Nodes)         %>%
  distinct(rowname, Pajek_ID, User_ID, x_coord, y_coord, ID) 

Nodes <- Nodes %>%
  gather(key = measure, value = User, User_ID, Pajek_ID,
         -ID, -x_coord, -y_coord, -rowname, na.rm = TRUE) 

Nodes <- as.data.frame(Nodes)    %>%
  filter (User != 0)  

Nodes <- Nodes                    %>%
  distinct(rowname, User, ID)     %>%
  dplyr::arrange(rowname)

#Merging Back Repository IDs
IDs <- as.data.frame(Repo_Networks[grep("ID", Repo_Networks$Repo_Networks),])
setDT(IDs)
cols <- (colnames(IDs))
columns <- c("Repository_IDs")
IDs <- (setnames(IDs, old=cols, new=columns))

#Create Count IDs
IDs <- as.data.frame(IDs)                 %>%
  (add_rownames)     

IDs <- IDs                                %>%
  mutate_if(is.character,as.numeric)      %>%
  dplyr::rename(Obs_ID = `rowname`)       

res <- numeric(length = length(Nodes$ID))
for (i in seq_along(Nodes$ID)) {
  res[i] <- ifelse(Nodes$ID[i] == 1, 1, 0)
}

s <- as.data.frame(cumsum(res))        %>%
  dplyr::rename(Obs_ID = `cumsum(res)`)

Nodes$Obs_ID <- s$Obs_ID

Nodes <- IDs                      %>%
  left_join(Nodes, by = c("Obs_ID"))

Nodes <- Nodes                      %>%
  select(Obs_ID, Repository_IDs, ID, User)  %>%
  dplyr::rename(Pajek_ID = `ID`,
                Network_ID = `Obs_ID`)

rm(s, IDs, vertices, i, res, DATA_DIR, col, cols, columns)

#Arcs
arcs <- as.data.frame(Repo_Networks[grep("arcs", Repo_Networks$Repo_Networks),])
setDT(arcs)
cols <- (colnames(arcs))
columns <- c("arcs")
arcs <- (setnames(arcs, old=cols, new=columns))

arcs <- as.data.frame(sub('arcs', '', arcs$arcs))

setDT(arcs)
cols <- (colnames(arcs))
columns <- c("arcs")
arcs <- (setnames(arcs, old=cols, new=columns))

arcs <- as.data.frame(arcs)       %>%
  (add_rownames)     

arcs <- arcs                               %>%
  mutate_if(is.character,as.numeric)        %>%
  dplyr::rename(Network_ID = `rowname`)   

cols <- c("arcs")
for(col in cols) {
  arcs <- separate_rows_(arcs, col, sep = "\n")
}

arcs <- as.data.frame(arcs)       %>%
  separate(arcs, c("Sender", "Target", "Weight")) 

arcs <- arcs                      %>%
  filter (Weight != "NA")  

#Merging Back Repository IDs
IDs <- as.data.frame(Repo_Networks[grep("ID", Repo_Networks$Repo_Networks),])
setDT(IDs)
cols <- (colnames(IDs))
columns <- c("Repository_IDs")
IDs <- (setnames(IDs, old=cols, new=columns))

#Create Count IDs
IDs <- as.data.frame(IDs)                 %>%
  (add_rownames)     

IDs <- IDs                                %>%
  mutate_if(is.character,as.numeric)      %>%
  dplyr::rename(Network_ID = `rowname`)  

Arcs <- IDs                      %>%
  left_join(arcs, by = c("Network_ID"))

rm(IDs, arcs)

######################
#   VISUALIZATIONS   #
######################

#Get Topographic Layouts for each Cluster
#Creating Clu file to subset network in Pajek

IDs <- Clusters           %>%
  select(Repository_ID, Pajek_Cluster_ID)

ID <- Nodes                %>%
  select(Repository_IDs, Network_ID)  %>%
  distinct(Repository_IDs, Network_ID)

Network_ID <- ID$Network_ID 

ID <- as.data.frame(sub('ID', '', ID$Repository_IDs))

setDT(ID)
cols <- (colnames(ID))
columns <- c("Repository_ID")
ID <- (setnames(ID, old=cols, new=columns))

ID <- as.data.frame(ID)  %>%
  mutate_if(is.factor,as.character)    %>%
  mutate_if(is.character,as.numeric)   

ID$Network_ID <- Network_ID

Cluster_ID <- IDs                      %>%
  left_join(ID, by = c("Repository_ID"))
  

Cluster_ID <- as.vector(Clusters$Pajek_Cluster_ID) 

blockmodeling::savevector(Cluster_ID, filename= "/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Clusters.clu")

rm(ID, IDs)

#*********************************************************IMPORTING and CONSTRUCTING CLUSTER NETWORKS*******************************************************************#

#Work on transforming into a loop
Cluster_1 <- read.paj("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Cluster_1_Network.net")
Cluster_2 <- read.paj("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Cluster_2_Network.net")
Cluster_3 <- read.paj("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Cluster_3_Network.net")
Cluster_4 <- read.paj("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Cluster_4_Network.net")

Cluster1_Network <- Network_Gen(Cluster1_Edges, Cluster_1, Cluster1_Nodes, Node_Count, Cluster1_Network, InDegree)
Cluster2_Network <- Network_Gen(Cluster2_Edges, Cluster_2, Cluster2_Nodes, Node_Count, Cluster2_Network, InDegree)
Cluster3_Network <- Network_Gen(Cluster3_Edges, Cluster_3, Cluster3_Nodes, Node_Count, Cluster3_Network, InDegree)
Cluster4_Network <- Network_Gen(Cluster4_Edges, Cluster_4, Cluster4_Nodes, Node_Count, Cluster4_Network, InDegree)

InDegree_1 <- InDegree(InDegree, Cluster1_Network)
InDegree_2 <- InDegree(InDegree, Cluster2_Network)
InDegree_3 <- InDegree(InDegree, Cluster3_Network)
InDegree_4 <- InDegree(InDegree, Cluster4_Network)

#**********************************************Creating Profile Visual Files**************************************************************************************#

Index <- NROW(na.omit(Clusters))
Index <- as.numeric(Index)
              
edges <- vector("list", Index)

for (i in seq_along(edges)){
  edges[[i]] <- data.frame(filter(Arcs, Network_ID == i ))
  names(edges)[[i]] <- paste0("edges",i)
}

edges <- lapply(edges, function(x) select(x, Sender, Target))
edges <- lapply(edges, function(x) mutate_if(x, is.numeric, as.character))

nodes <- vector("list", Index)

for (i in seq_along(nodes)){
  nodes[[i]] <- data.frame(filter(Nodes, Network_ID == i ))
  names(nodes)[[i]] <- paste0("nodes",i)
}

count <- lapply(nodes, function(x) NROW(na.omit(x)))

count <- unlist(count)

network.list <- list(seq_along(count))

for(i in seq_along(count)){
  network.list[i] <- lapply(network.list[i], function(x) network.initialize(count[i]))
}

for(i in seq_along(network.list)){
  network.list[i] <- lapply(network.list[i], function(x) add.edges(network.list[[i]], edges[[i]][,1], edges[[i]][,2]))
}

head(network.list)

save(network.list, file = "GitHubTest_NetworkList.Rda")
save(nodes, file = "GitHubTest_NodeList.Rda")

rm(network.list)

#***********************************************************CREATING INDICES FILE******************************************************************************************#
load("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/GitHubTest_NetworkList.Rda")  

#Number of Isolates
n_isolates <- lapply(network.list, function(x) isolates(x))
n_isolates <- lapply(n_isolates, function(x) as.data.frame(x))

for (i in seq_along(n_isolates)){
  names(n_isolates)[[i]] <- paste0("isolates",i)
}

isolates.list <- list(seq_along(n_isolates))

isolates.list <- lapply(n_isolates, function(x) NROW(na.omit(x)))
n_isolates <- unlist(isolates.list)
n_isolates <- as.vector(n_isolates)

head(n_isolates)
rm(isolates.list)

#Density
density <- lapply(network.list, function(x) gden(x))
density <- unlist(density)
density <- as.vector(density)
head(density)


#Transitivity
transitivity <- lapply(network.list, function(x) gtrans(x))
transitivity <- unlist(transitivity)
transitivity <- as.vector(transitivity)
head(transitivity)

#Average Path Length
geodesic <- lapply(network.list, function(x) geodist(x, inf.replace=0))

geo_dist = n_geo$gdist
geo_dist <- list(seq_along(geodesic))

for (i in seq_along(geodesic)){
  geo_dist[[i]] = geodesic[[i]]$gdist
  geo_dist[[i]] <- as.data.frame(geo_dist[[i]])
}

average_geodist <- lapply(geo_dist, function(x) mean(as.matrix(x)))
average_geodist <- unlist(average_geodist)
average_geodist <- as.vector(average_geodist)

#Size
network_size <- lapply(network.list, function(x) network.size(x))
network_size <- unlist(network_size)
network_size <- as.vector(network_size)
head(network_size)

#Dyad Count
dyadcount <- lapply(network.list, function(x) network.dyadcount(x))
dyadcount <- unlist(dyadcount)
dyadcount <- as.vector(dyadcount)
head(dyadcount)

#Krackhardt hierarchy scores
k_hierarchy <- lapply(network.list, function(x) hierarchy(x))
k_hierarchy <- unlist(k_hierarchy)
k_hierarchy <- as.vector(k_hierarchy)
head(k_hierarchy)

#centralization
  #The graph-level centrality score can be normalized by dividing by the maximum theoretical score for a graph with the same number of vertices, 
  #using the same parameters, e.g. directedness, whether we consider loop edges, etc.

#Indegree Centralization
#?centralization

indegree_centralization <- lapply(network.list, function(x) centralization(x, degree, cmode="indegree"))
indegree_centralization <- unlist(indegree_centralization)
indegree_centralization <- as.vector(indegree_centralization)
head(indegree_centralization)

#Outdegree Centralization
outdegree_centralization <- lapply(network.list, function(x) centralization(x, degree, cmode="outdegree"))
outdegree_centralization <- unlist(outdegree_centralization)
outdegree_centralization <- as.vector(outdegree_centralization)
head(outdegree_centralization)

#Betweeness Centralization
betweenness_centralization <- lapply(network.list, function(x) centralization(x, betweenness))
betweenness_centralization <- unlist(betweenness_centralization)
betweenness_centralization <- as.vector(betweenness_centralization)
head(betweenness_centralization)

rm(i, geo_dist, geodesic)

#Future Measures
  #Proportion of Ties within the Cluster by Repository
  #Degree/Weighted Degree of the Nodes in the Cluster in the entire network
  #Average k-core membereship of the nodes in the cluster in the entire network

#Creating Indices File
Indices <- Nodes %>%
  distinct(Network_ID, Repository_IDs)  %>%
  arrange(Network_ID)

Indices$Repository_IDs <- as.character(Indices$Repository_IDs)
IDs <- as.data.frame(sub('ID ', '', Indices$Repository_IDs))

setDT(IDs)
cols <- (colnames(IDs))
columns <- c("Repository_ID")
IDs <- (setnames(IDs, old=cols, new=columns))

Indices$Repository_ID <- IDs$Repository_ID

Indices <- Indices    %>%
  select(Network_ID, Repository_ID)  

Indices$Repository_ID <- as.character(Indices$Repository_ID)
Indices$Repository_ID <- as.numeric(Indices$Repository_ID)

Indices$network_size <- network_size
Indices$n_isolates <- n_isolates
Indices$dyadcount <- dyadcount
Indices$density <- density
Indices$average_geodist <- average_geodist
Indices$transitivity <- transitivity
Indices$k_hierarchy <- k_hierarchy
Indices$indegree_centralization <- indegree_centralization
Indices$outdegree_centralization <- outdegree_centralization
Indices$betweenness_centralization <- betweenness_centralization

Cluster_ID <- Clusters %>%
  select(Repository_ID, Cluster_ID)       %>%
  mutate_if(is.numeric, as.character)      %>%
  mutate_if(is.character, as.numeric)      

Indices <- Cluster_ID                      %>%
  full_join(Indices, by = c("Repository_ID"))

Indices <- Indices %>%
  select(Cluster_ID, Repository_ID, Network_ID, network_size, n_isolates, dyadcount, density, average_geodist, transitivity, k_hierarchy, indegree_centralization,
         outdegree_centralization, betweenness_centralization)

Indices$Cluster_ID <- as.factor(Indices$Cluster_ID)
Indices$Repository_ID <- as.factor(Indices$Repository_ID)
Indices$Network_ID <- as.factor(Indices$Network_ID)

save(Indices, file = "GitHubTest_Indices.Rda")

#********************************************************VISULALIZING CLUSTER NETWORKS************************************************************************************#

set.seed(3456)
g1 <-ggnetwork(Cluster1_Network) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    #geom_edges(color = "dodgerblue1", alpha=0.4) +
    #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
    stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
    scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
    scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
    geom_density2d(colour="gray8", bins=12) +
    theme_blank()

set.seed(3456)
g2 <- ggnetwork(Cluster2_Network) %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
      #geom_edges(color = "dodgerblue1", alpha=0.4) +
      #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
      stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
      scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
      scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
      geom_density2d(colour="gray8", bins=12) +
      theme_blank()

set.seed(3456)
g3 <- ggnetwork(Cluster3_Network) %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
      #geom_edges(color = "dodgerblue1", alpha=0.4) +
      #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
      stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
      scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
      scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
      geom_density2d(colour="gray8", bins=12) +
      theme_blank()

set.seed(3456)
g4 <- ggnetwork(Cluster4_Network) %>%
      ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
      #geom_edges(color = "dodgerblue1", alpha=0.4) +
      #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
      stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
      scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
      scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
      geom_density2d(colour="gray8", bins=12) +
      theme_blank()

#*****************************************************************Profile Viuals*********************************************************************************#
load("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/GitHubTest_Indices.Rda") 

#Scaling for Visualization
Indices$network_size_scaled <- (Indices$network_size - mean(Indices$network_size)) / sd(Indices$network_size) 
Indices$n_isolates_scaled <- (Indices$n_isolates - mean(Indices$n_isolates)) / sd(Indices$n_isolates) 
Indices$dyadcount_scaled <- (Indices$dyadcount - mean(Indices$dyadcount)) / sd(Indices$dyadcount) 
Indices$density_scaled <- (Indices$density - mean(Indices$density)) / sd(Indices$density) 
Indices$average_geodist_scaled <- (Indices$average_geodist - mean(Indices$average_geodist)) / sd(Indices$average_geodist) 
Indices$transitivity_scaled <- (Indices$transitivity - mean(Indices$transitivity)) / sd(Indices$transitivity) 
Indices$k_hierarchy_scaled <- (Indices$k_hierarchy - mean(Indices$k_hierarchy)) / sd(Indices$k_hierarchy) 
Indices$indegree_centralization_scaled <- (Indices$indegree_centralization - mean(Indices$indegree_centralization)) / sd(Indices$indegree_centralization) 
Indices$outdegree_centralization_scaled <- (Indices$outdegree_centralization  - mean(Indices$outdegree_centralization)) / sd(Indices$outdegree_centralization) 
Indices$betweenness_centralization_scaled <- (Indices$betweenness_centralization  - mean(Indices$betweenness_centralization)) / sd(Indices$betweenness_centralization) 

#Creating Stacked Indices File for Visualization
Indices <- Indices   %>%
  select(Cluster_ID, Repository_ID, Network_ID, network_size_scaled,n_isolates_scaled, dyadcount_scaled, density_scaled, average_geodist_scaled,  
         transitivity_scaled,k_hierarchy_scaled, indegree_centralization_scaled, outdegree_centralization_scaled, betweenness_centralization_scaled)

Indices_Stacked <- Indices %>%
  gather(key = measure, value = value, network_size_scaled, n_isolates_scaled, dyadcount_scaled, density_scaled, average_geodist_scaled, 
         transitivity_scaled, k_hierarchy_scaled, indegree_centralization_scaled, outdegree_centralization_scaled, betweenness_centralization_scaled,
         -Cluster_ID, -Repository_ID, -Network_ID, na.rm = TRUE) 

Indices_Stacked <- Indices_Stacked %>%
  arrange(measure, Network_ID)

Indices_Stacked <- Indices_Stacked %>%
  mutate(Measure_Label =  ifelse(measure == "network_size_scaled", "Network Size", 
                          ifelse(measure == "n_isolates_scaled", "Number of Isolates", 
                          ifelse(measure == "dyadcount_scaled", "Dyad Count", 
                          ifelse(measure == "density_scaled", "Density",
                          ifelse(measure == "average_geodist_scaled", "Average Geodesic", 
                          ifelse(measure == "transitivity_scaled", "Transitivity", 
                          ifelse(measure == "k_hierarchy_scaled", "Krackhardt’s Hierarchy",
                          ifelse(measure == "indegree_centralization_scaled", "InDegree Centralization",
                          ifelse(measure == "outdegree_centralization_scaled", "OutDegree Centralization", 
                          ifelse(measure == "betweenness_centralization_scaled", "Betweenness Centralization", "Missing")))))))))))

Indices_Stacked$measure = factor(Indices_Stacked$measure,levels=c("network_size_scaled", "n_isolates_scaled", "dyadcount_scaled", "density_scaled", "average_geodist_scaled", "transitivity_scaled", "k_hierarchy_scaled", 
                                                                   "indegree_centralization_scaled", "outdegree_centralization_scaled", "betweenness_centralization_scaled"),ordered=TRUE)


Indices_Stacked$Measure_Label = factor(Indices_Stacked$Measure_Label,levels=c("Network Size", "Number of Isolates", "Dyad Count", "Density", "Average Geodesic", "Transitivity", "Krackhardt’s Hierarchy", 
                                                                  "InDegree Centralization", "OutDegree Centralization", "Betweenness Centralization"),ordered=TRUE)


#Creating Cluster Specific Means
Indices_Scaled_Means <- group_by(Indices_Stacked, Cluster_ID, Measure_Label) %>% 
  summarise(Cluster_Measure_Mean = mean(value))

#Creating Cluster Specific STDs
Indices_Scaled_SEs <- group_by(Indices_Stacked, Cluster_ID, Measure_Label) %>% 
  summarise(Cluster_Measure_SD = sd(value)) %>%
  mutate(Cluster_Measure_SE = Cluster_Measure_SD/sqrt(n()))
  
#Merging Stacked Indices, Means, and STDs
Indices_Scaled_Descriptives <- Indices_Scaled_Means   %>%
  left_join(Indices_Scaled_SEs, by = c("Cluster_ID", "Measure_Label"))

Indices_Stacked <- Indices_Stacked   %>%
  left_join(Indices_Scaled_Descriptives, by = c("Cluster_ID", "Measure_Label"))

save(Indices_Stacked, file = "GitHubTest_Indices_Stacked.Rda")

#CREATING VISUALS
load("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/GitHubTest_Indices_Stacked.Rda") 

p1 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "0", ] %>>% ggplot() + 
  theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=10),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=10),
        axis.title.y = element_text(colour="grey20",size=16))   +
  geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
  geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
  geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  guides(fill=guide_legend(title="Measure")) +
  labs(y = "Standardized Value", x = "Measure") +
  ggtitle("Cluster 1")

p1

p2 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "1", ] %>>% ggplot() + 
  theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=10),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=10),
        axis.title.y = element_text(colour="grey20",size=16))   +
  geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
  geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
  geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  guides(fill=guide_legend(title="Measure")) +
  labs(y = "Standardized Value", x = "Measure") +
  ggtitle("Cluster 2")

p2

p3 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "2", ] %>>% ggplot() + 
  theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=10),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=10),
        axis.title.y = element_text(colour="grey20",size=16))   +
  geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
  geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
  geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  guides(fill=guide_legend(title="Measure")) +
  labs(y = "Standardized Value", x = "Measure") +
  ggtitle("Cluster 3")

p3

p4 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "3", ] %>>% ggplot() + 
  theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=10),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=10),
        axis.title.y = element_text(colour="grey20",size=16))   +
  geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
  geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
  geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  guides(fill=guide_legend(title="Measure")) +
  labs(y = "Standardized Value", x = "Measure") +
  ggtitle("Cluster 4")

p4

#Combined Figure
p5 <- Indices_Stacked %>>% ggplot() + 
  theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=10),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=10),
        axis.title.y = element_text(colour="grey20",size=16))   +
  geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
  geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
  geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
  guides(fill=guide_legend(title="Measure")) +
  labs(y = "Standardized Value", x = "Measure") +
  facet_grid(Cluster_ID ~ .) +
  ggtitle("Cluster Profile Comparison")

p5

############################################
#   CREATING COMBINED STRUCTURAL VISUALS   #
############################################

#Next Steps
  #Check to see if you can output .jpg rather than .tiff
  #Get URLS of the repositories
  #ADJUST X-AXIS LABELS to 4 for THE NEXT PASS!!!
  #Add the n of the cluster in the topographic figures
  #Calculate Community Centrality and Isolate these defining Nodes
    #Compare Linkcomm results to cluster graphs
    #Visualize the graph structure of the community central repositories.

#CREATING 4X4 LAYOUT FIGURE
gg_list <- list()

#Graph and Profiles

tiff("GitHub_Test_ClusterGraphs.tiff", width = 20, height = 16.6, units = 'in', res = 300)

for(i in 1:8) {
  # assign your ggplot call to the i'th position in the list
  set.seed(3456)
  g1 <-ggnetwork(Cluster1_Network) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    #geom_edges(color = "dodgerblue1", alpha=0.4) +
    #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
    stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
    scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
    scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
    geom_density2d(colour="gray8", bins=12) +
    theme_blank()
  
  p1 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "0", ] %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
          panel.grid.minor = element_line(colour = 'snow3'),
          panel.border = element_rect(color = "snow3", fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold"), 
          axis.text.x = element_text(colour="grey20",size=6),
          axis.title.x = element_text(colour="grey20",size=14),
          axis.text.y = element_text(colour="grey20",size=10),
          axis.title.y = element_text(colour="grey20",size=16))   +
    geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
    geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
    geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
    guides(fill=guide_legend(title="Measure")) +
    labs(y = "Standardized Value", x = "Measure") +
    ggtitle("Cluster 1")
  
  set.seed(3456)
  g2 <- ggnetwork(Cluster2_Network) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    #geom_edges(color = "dodgerblue1", alpha=0.4) +
    #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
    stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
    scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
    scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
    geom_density2d(colour="gray8", bins=12) +
    theme_blank()
  
  p2 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "1", ] %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
          panel.grid.minor = element_line(colour = 'snow3'),
          panel.border = element_rect(color = "snow3", fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold"), 
          axis.text.x = element_text(colour="grey20",size=6),
          axis.title.x = element_text(colour="grey20",size=14),
          axis.text.y = element_text(colour="grey20",size=10),
          axis.title.y = element_text(colour="grey20",size=16))   +
    geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
    geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
    geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
    guides(fill=guide_legend(title="Measure")) +
    labs(y = "Standardized Value", x = "Measure") +
    ggtitle("Cluster 2")
  
  set.seed(3456)
  g3 <- ggnetwork(Cluster3_Network) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    #geom_edges(color = "dodgerblue1", alpha=0.4) +
    #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
    stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
    scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
    scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
    geom_density2d(colour="gray8", bins=12) +
    theme_blank()
  
  p3 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "2", ] %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
          panel.grid.minor = element_line(colour = 'snow3'),
          panel.border = element_rect(color = "snow3", fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold"), 
          axis.text.x = element_text(colour="grey20",size=6),
          axis.title.x = element_text(colour="grey20",size=14),
          axis.text.y = element_text(colour="grey20",size=10),
          axis.title.y = element_text(colour="grey20",size=16))   +
    geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
    geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
    geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
    guides(fill=guide_legend(title="Measure")) +
    labs(y = "Standardized Value", x = "Measure") +
    ggtitle("Cluster 3")
  
  set.seed(3456)
  g4 <- ggnetwork(Cluster4_Network) %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
    #geom_edges(color = "dodgerblue1", alpha=0.4) +
    #geom_nodes(color = "snow3", size = 2, alpha=0.5) +   
    stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
    scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
    scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
    geom_density2d(colour="gray8", bins=12) +
    theme_blank()
  
  p4 <- Indices_Stacked[Indices_Stacked$Cluster_ID == "3", ] %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
          panel.grid.minor = element_line(colour = 'snow3'),
          panel.border = element_rect(color = "snow3", fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold"), 
          axis.text.x = element_text(colour="grey20",size=6),
          axis.title.x = element_text(colour="grey20",size=14),
          axis.text.y = element_text(colour="grey20",size=10),
          axis.title.y = element_text(colour="grey20",size=16))   +
    geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
    geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
    geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
    guides(fill=guide_legend(title="Measure")) +
    labs(y = "Standardized Value", x = "Measure") +
    ggtitle("Cluster 4")
  
  gg_list[[i]] <- g1
  p1
  g2
  p2
  g3
  p3
  g4
  p4
  
}

grid.arrange(g1, p1, g2, p2, g3, p3, g4, p4, nrow=4)

dev.off()

#CREATING 1X1 LAYOUT FIGURE
gg_list <- list()

#GitHub_Test_Profiles
tiff("GitHub_Test_Profiles.tiff", width = 15, height = 12.5, units = 'in', res = 300)

for(i in 1:1) {
  # assign your ggplot call to the i'th position in the list
  p5 <- Indices_Stacked %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
          panel.grid.minor = element_line(colour = 'snow3'),
          panel.border = element_rect(color = "snow3", fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold"), 
          axis.text.x = element_text(colour="grey20",size=6),
          axis.title.x = element_text(colour="grey20",size=14),
          axis.text.y = element_text(colour="grey20",size=10),
          axis.title.y = element_text(colour="grey20",size=16))   +
    geom_violin(mapping = aes(fill=Measure_Label, x=Measure_Label, y=value), alpha = .4) +
    geom_errorbar(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean, ymin=Cluster_Measure_Mean-Cluster_Measure_SE, ymax=Cluster_Measure_Mean+Cluster_Measure_SE),colour="black", width=.1) + 
    geom_point(mapping = aes(x = Measure_Label, y = Cluster_Measure_Mean), size=3, shape=21, fill="white") +
    geom_hline(yintercept=0) +
    scale_fill_manual(values=c("olivedrab4", "olivedrab3", "olivedrab2", "goldenrod2", "burlywood2", "burlywood3", "burlywood4", "dodgerblue2", "dodgerblue3", "dodgerblue4")) +
    guides(fill=guide_legend(title="Measure")) +
    labs(y = "Standardized Value", x = "Measure") +
    facet_grid(Cluster_ID ~ .) +
    ggtitle("Cluster Profile Comparison")
  gg_list[[i]] <- p5
}

grid.arrange(p5)

dev.off()
  
#############
#   TESTS   #
#############

db <- src_sqlite(tempfile(), create = TRUE)
iris2 <- copy_to(db, iris)
vec <- pull(iris2, Species)
head(vec)

str1 <- "word1.word2"

strsplit(str1, ".", fixed = TRUE)  ## Add fixed = TRUE
strsplit(str1, "[.]")              ## Make use of character classes
strsplit(str1, "\\.")      

spec_csv(system.file("extdata/mtcars.csv", package = "readr"))

spec_csv("/Users/jhmorgan/Desktop/GroupSimulator Paper/GitHub Network Analysis/Nodes.csv")

#Splitting based on delimiter and adding new rows
df <- read.table(textConnection("1|a,b,c\n2|a,c\n3|b,d\n4|e,f"), header = F, sep = "|", stringsAsFactors = F)

s <- strsplit(df$V2, split = ",")
data.frame(V1 = rep(df$V1, sapply(s, length)), V2 = unlist(s))

require(network)

par(mfrow=c(2,2))

test.net.1 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/GD/gd98/A98.net")
plot(test.net.1,main=test.net.1%n%'title')

test.net.2 <- read.paj("http://vlado.fmf.uni-lj.si/pub/networks/data/mix/USAir97.net")
# plot using coordinates from the file in the file
plot(test.net.2,main=test.net.2%n%'title',
     coord=cbind(test.net.2%v%'x',
                 test.net.2%v%'y'),
     jitter=FALSE)

data(emon)
as.edgelist(emon[[1]])
# contrast with unsorted columns of
as.matrix.network.edgelist(emon[[1]])

# Get the data directory from readtext
DATA_DIR <- system.file("extdata/", package = "readtext")

Test <- readtext(paste0(DATA_DIR, "/txt/UDHR/*"))

d <- data.frame(a=c(1:3), 
                b=c("name1, name2, name3", "name4", "name5, name6"),
                c=c("name7","name8, name9", "name10" ))

cols <- c("b", "c")
for(col in cols) {
  d <- separate_rows_(d, col)
}

a <- c("Alpha", "Beta", "Gamma", "Beeta", "Alpha", "beta") 
b <- c(1:6) 
df <- data.frame(Title = a, Vals = b) 

df[grep("eta", df$Title),] 

strsplit("my string is sad"," ")[[1]][-1]

gsub('([A-z]+) .*', '\\1', 'my string is sad')

Test <- as.data.frame(gsub('([A-z]+) .*', '\\1', Nodes$Vertices))

Test <- (strsplit(gsub("[^[:digit:]. ]", "", Nodes$Vertices), " +"))

str_count(Nodes$Vertices)

Test <- as.data.frame(str_count(Nodes$Vertices))

x <- 'aabb.ccdd'
sub('.*', '', x)

sub('bb.*', '', x)

sub('.*bb', '', x)

sub('\\..*', '', x)

sub('.*\\.', '', x)

before = data.frame(attr = c(1,30,4,6), type=c('foo_and_bar','foo_and_bar_2'))
str_split_fixed(before$type, "_and_", 2)

x<- c(1,3.0,3.1,3.2,1,1,2,3.0,3.1,3.2,4,4,5,6,5,3.0,3.1,3.2,
      3.1,2,1,4,6,4.0,4,3.0,3.1,3.2,5,3.2,3.0,4)

pattern <- c(3, 3.1, 3.2)
len1 <- seq_len(length(x) - length(pattern) + 1)
len2 <- seq_len(length(pattern))-1
sum(colSums(matrix(x[outer(len1, len2, '+')], 
                   ncol=length(len1), byrow=TRUE) == pattern) == length(len2))


as.data.table(df)[, count := sequence(.N), by = rleid(Value)][Value == "No", count := 0][]

a <- 1:10
b <- 1:10

res <- numeric(length = length(Nodes$ID))
for (i in seq_along(Nodes$ID)) {
  res[i] <- ifelse(Nodes$ID[i] == 1, 1, 0)
}

res

index<-sapply(1:length(res),function(x)sum(res[1:x]==res[x]))
cbind(res, index)

n <- 5
seq_len(n)

cumsum(res)

a <- 1:120
b <- a[seq(1, length(a), 2)]
b

cols <- c("arcs")
for(col in cols) {
  Test <- separate_rows_(Test, col, sep = "\n")
}

#Test Data: Contour Maps
contesto = c("M01", "M02", "M03", "M04", "M04a", "M05", "M06", "M08", "M09", "M10", "M11", "M12", "M13", "M14", "M15", "M16", "M17", "M18", "M19", "M20", "M21")
x = c(81.370,  85.814, 73.204, 66.478, 67.679, 59.632, 64.316, 90.258, 100.707, 89.829, 114.998, 119.922, 129.170, 142.501, 76.206, 30.090, 130.731, 74.885, 48.823,  48.463, 74.765)
y = c(255.659, 242.688, 240.526, 227.916, 218.668, 239.325, 252.777, 227.676, 217.828, 205.278, 216.747, 235.482, 239.205, 229.717, 213.144, 166.785, 219.989, 192.336, 142.645, 186.361, 205.698)
perc = c(22, 16, 33, 46, 15, 35, 23, 45, 58, 53, 15, 18, 36, 24, 24, 33, 56, 36, 32, 24, 16)

dataset = data.frame(contesto, x, y, perc) 

dataset2 <- with(dataset, dataset[rep(1:nrow(dataset), perc),])

ggplot(dataset2, aes(x, y)) + 
  stat_density2d(aes(alpha=..level.., fill=..level..), size=2, 
                 bins=10, geom="polygon") + 
  scale_fill_gradient(low = "yellow", high = "red") +
  scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
  geom_density2d(colour="black", bins=10) +
  geom_point(data = dataset) +
  guides(alpha=FALSE) + xlim(c(10, 160)) + ylim(c(120, 280))

#Combining Techniques
set.seed(12345)
ggnetwork(Cluster1_Network) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "lightgray") +
  geom_nodes(color = "snow3", size = InDegree) +       
  #geom_nodelabel_repel (color = Race, label = Race) +#   For networks with fewer nodes, we might want to label
  theme_blank() + 
  geom_density_2d(color="burlywood4", alpha = 0.7)

set.seed(12345)
ggnetwork(Cluster1_Network) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "snow3") +
  geom_nodes(color = "gainsboro", size = InDegree) +   
  stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=12, geom="polygon") + 
  scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
  scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
  geom_density2d(colour="gray8", bins=12) +
  theme_blank()

#Iterative Loops
#https://stackoverflow.com/questions/40952988/how-to-create-for-loop-for-multiple-dataset


clusters <- vector("list",2)

for (i in seq_along(clusters)){
  clusters[[i]] <- data.frame(x = runif(5),y = runif(5))
  names(clusters)[[i]] <- paste0("cluster",i)
}

names(clusters) <- paste0("cluster",seq_along(clusters))
list2env(clusters,envir = .GlobalEnv)

rm(cluster1, cluster2)

#Community Centrality Tests
  #Documentation: https://cran.r-project.org/web/packages/linkcomm/linkcomm.pdf
library(igraph)
library(linkcomm)

## Generate graph and extract link communities.
g <- swiss[,3:4]
lc <- getLinkCommunities(g)

## Calculate community centrality.
cc <- getCommunityCentrality(lc)

## Calculate community centrality using "commconn" measure.
cc <- getCommunityCentrality(lc, type = "commconn")

cc

