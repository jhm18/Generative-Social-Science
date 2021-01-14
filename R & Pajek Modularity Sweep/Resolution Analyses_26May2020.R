#Jonthan H. Morgan
#Resolution Analyses: R Implmentation and Extension of Jim Moody's modproc SAS macro (2.5.2018)
#26 May 2020

################
#   PACKAGES   #
################

#Install Packages Necessary for the Script if Missing
list.of.packages <- c('ggplot2', 'pipeR', 'magick', 'questionr', 'fossil', 'ggplotify', 'gridExtra', 'RColorBrewer')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)      #Primary plotting tool
library(ggplotify)    #Transform Images into GROBS
library(RColorBrewer) #Adds color palettes to use when visualizing by factor variables
library(fossil)       #Calculates Adjusted Rand Index (Hand Calculations Included, but fossill is more reliable)
library(pipeR)        #Suppport the layering of visualizations with double pipe commands
library(questionr)    #Generates weighted frequencies 
library(magick)       #Used to create image files for manipulation (Add Base R Elements)
library(gridExtra)    #Easy produce panel graphs and plot objects

rm(list.of.packages, new.packages)

################################
#   BASE FUNCTIONS AND LISTS   #
################################

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

colClean <- function(x){ colnames(x) <- sub(".*\\__", "g_", colnames(x)); x } 

Rep_Files = list.files(pattern="*.rep")

#################################
#   BASIC RESOLUTION ANALYSIS   #
#################################

basic_res <- function(rep_file) {
  rep_file <- readLines(rep_file)

  #Isolating Key Elements: Resolution, Number of Clusters, Modularity

  #Isolating Resolution Values
  Resolution_List <- vector('list', length(rep_file))
  for (i in seq_along(rep_file)){
    if (grepl('Resolution:', rep_file[[i]]) == TRUE){
      Resolution_List[[i]] <- rep_file[[i]]
      } else {
      Resolution_List[[i]] <- NULL
      }
  }

  #Eliminating NULL Elements
  Resolution_List[sapply(Resolution_List, is.null)] <- NULL

  Resolution <- as.numeric(trim(gsub('Resolution:', '', Resolution_List)))

  rm(Resolution_List)

  #Isolating the Number of Cluster per Solution
  Cluster_List <- vector('list', length(rep_file))
  for (i in seq_along(rep_file)){
    if (grepl('Number of Clusters:', rep_file[[i]]) == TRUE){
     Cluster_List[[i]] <- rep_file[[i]]
    } else {
      Cluster_List[[i]] <- NULL
    }
  }

  #Eliminating NULL Elements
  Cluster_List[sapply(Cluster_List, is.null)] <- NULL

  Clusters <- as.numeric(trim(gsub('Number of Clusters:', '', Cluster_List)))

  rm(Cluster_List)

  #Isolating Modularity Values
  Modularity_List <- vector('list', length(rep_file))
  for (i in seq_along(rep_file)){
    if (grepl('Modularity:', rep_file[[i]]) == TRUE){
      Modularity_List[[i]] <- rep_file[[i]]
    } else {
      Modularity_List[[i]] <- NULL
    }
  }

  #Eliminating NULL Elements
  Modularity_List[sapply(Modularity_List, is.null)] <- NULL

  Modularity <- as.numeric(trim(gsub('Modularity:', '', Modularity_List)))

  rm(Modularity_List)

  `Record Number` <- seq_along(Resolution)

  Basic_Resolution <- as.data.frame(cbind(`Record Number`,Resolution, Clusters, Modularity))

  rm(`Record Number`, Resolution, Clusters, Modularity, i)

  #Creating Basic Plot
  jitter <- ggplot2::position_jitter(width = 0.01, height = 0.01)
  spline_int <- as.data.frame(spline(Basic_Resolution$Resolution, Basic_Resolution$Clusters))
    colnames(spline_int)[1] <- 'Resolution'
    colnames(spline_int)[2] <- 'Clusters'

  p_1 <- Basic_Resolution %>>% ggplot() + 
    theme(panel.background = element_rect(fill = 'snow', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        panel.border = element_rect(color = "snow3", fill = NA, size = 1),
        plot.title = element_text(size = 20, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=13),
        axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=13),
        axis.title.y = element_text(colour="grey20",size=16),
        strip.text.x = element_text(size = 11, face="bold"),
        strip.text.y = element_text(size = 11, face="bold"),
        plot.caption=element_text(hjust=1, size=10),
        legend.title=element_text(size=12), 
        legend.text=element_text(size=11)) +
    geom_line(data=spline_int, mapping = aes(x = Resolution, y = Clusters), size=0.8) +
    geom_point(mapping = aes(x = Resolution, y = Clusters), position = jitter, shape=1) +
    scale_x_continuous(breaks=c(0.50, 0.75, 1.00, 1.25, 1.50, 1.75),  labels=c("0.5", "0.75", "1", "1.25", "1.5", "1.75")) +
    labs(y = "Number of Clusters") +
    ggtitle("Basic Resolution Analysis")

  #writing objects to the Global Environment
  `Basic Resolution` <-  assign(x = "Basic Resolution", value = p_1,.GlobalEnv) 
}

#######################
#   CHAMPS ANALYSIS   #
#######################
  #Modularity Processing
  #Heat Map Compare
  #Concensus Analysis

#Citation
#Weir, W., Emmons, S., Gibson, R., Taylor, D., & Mucha, P. (2017). Post-processing partitions to identify domains of modularity optimization. Algorithms, 10(3), 93.

#Quantities Aˆσ and Pˆσ are the respective within-community sums over Aij and Pij for partition σ.

#Terms
  #  Aij encodes all of the edges
  #  Pij specifies the within-layer null model contribution
  #  Cij describes the known interlayer connections

mod_proc <- function(net, clu) {
  
  #Plots elements for later
  colors <- c("#FF0000", "#FF4D00", "#FF6800", "#FFA200", "#F5BA00", "#E3D000", "#C3EE00", "#A7FF00", "#9CFF00", "#74FF00", "#6DFF00", "#1FFF4B", "#21FF55", 
              "#39DAD1", "#00FFAA", "#37DFCB", "#37CAE0", "#3084FF","#327CFF", "#325AFF", "#2F51FF", "#6000FF", "#9A00FF", "#AC00FE", "#A600FF", "#FF00AA", "#EA00C4")
  
  palette <- brewer.pal("Greys", n=9)

  #Reading Network In
  read_net(net)
  
  #Saving raw clu file for ARI analyses
  ARI_partition <- clu[-c(1)]
  ARI_partition <- colClean(ARI_partition)

  #Symmetrize Network
  sym_1 <- as.data.frame(cbind(as.numeric(ties$Weight), as.numeric(ties$`Person i`), as.numeric(ties$`Person j`)))   
  sym_2 <- as.data.frame(cbind(as.numeric(ties$Weight), as.numeric(ties$`Person j`), as.numeric(ties$`Person i`)))
  
  symedge <- as.data.frame(rbind(sym_1, sym_2))

  symedge <- symedge[!duplicated(symedge[c(2,3)]),]
    colnames(symedge)[1] <- 'value'
    colnames(symedge)[2] <- 'node1'
    colnames(symedge)[3] <- 'node2'
  
  symedge <- symedge[order(symedge[,2], symedge[,2] ), ]

  rm(sym_1, sym_2)

  #Getting Weighted Degree Information
  degree <- questionr::wtd.table(symedge$node1, weights=symedge$value)
  degree <- as.matrix(degree, rownames = TRUE)

  node1 <- as.numeric(row.names(degree))
  degree <- as.data.frame(cbind(node1, degree))
    colnames(degree)[[2]] <- 'n'

  #Formatting the Partition File Generated by read_clu: A matrix consisting of each partition solution
  clu <- colClean(clu)

  col_names <- row.names(t(clu))

  clu <- as.data.frame(clu[,col_names])
    colnames(clu)[1] <- 'node1'

  clu_t <- as.data.frame(t(clu[-c(1)]))
  
  #Recovering resolution parameter
  res_values <- colnames(clu)[-c(1)] 
  res <- vector('numeric', length(res_values))
  for (i in seq_along(res_values)){
    res_i <- (strsplit(res_values[[i]], '_'))[[1]]
    res_i <- res_i[res_i != ""]
    res_i <- as.numeric(res_i[[2]])
    res[i] <- res_i
    rm(res_i)
  }
  
  rm(res_values)
    
  clu_t <- as.data.frame(cbind(clu_t, res))

  #Isolating Unique Cases for Each Resolution Parameter 
  names <- colnames(clu_t)
  clu_t <- clu_t[!duplicated(clu_t[,c(names)]),]

  clu <- as.data.frame(t(clu_t))
  clu <- clu[-nrow(clu),] 
  clu <- as.data.frame(cbind(node1, clu))

  rm(names)

  #Merging Degree Measures with the Partition Matrix
  degree <-  merge(degree, clu, by = c("node1")) 

  #Getting the total degree count, m
  m <- sum(degree$n)

#***************************************************************************   -sumPij-   ********************************************************************************************#

  #Getting the unique list of groups for the partition
  partitions <- colnames(degree)
  partitions <- partitions[-c(1:2)]

  Pij_List <- vector('list', length(partitions))

  for (i in seq_along(partitions)){
    group <- unique(degree[,partitions[[i]]])
    group <- as.numeric(as.character(group))

    combinations <- vector('list', length(group))

    partition <- degree[, partitions[i]]
    partition <- as.numeric(as.character(partition))

    partition <- as.data.frame(cbind(degree$n, partition))
      colnames(partition)[1] <- 'degree'

    for (j in seq_along(group)){
      element <- partition[partition[[2]] == group[[j]], ]
    
      combinations[[j]] <- outer(element$degree, element$degree, '*')
      combinations[[j]] <- as.numeric(unlist(as.list(t(combinations[[j]]))))
    
      rm(element)
    }
      
    combinations <- as.numeric(unlist(combinations))
    combinations <- combinations/m
  
    Pij_List[[i]] <- sum(combinations)
  
    rm(group, partition, combinations)
  }

  sumPij <- as.numeric(unlist(Pij_List))

  rm(Pij_List, i, j)

#*******************************************************************   -sumAij and group count-   ************************************************************************************#

  Aij_List <- vector('list', length(partitions))
  Group_List <- vector('list', length(partitions))

  for (i in seq_along(partitions)){
    group <- unique(degree[,partitions[[i]]])
    group <- as.numeric(as.character(group))
  
    Group_List[[i]] <- length(group)

    sums <- vector('list', length(group))

    partition <- degree[, partitions[i]]
    partition <- as.numeric(as.character(partition))

    partition <- as.data.frame(cbind(degree$node1, partition))
      colnames(partition)[1] <- 'node1'

    for (j in seq_along(group)){
      element <- partition[partition[[2]] == group[[j]], ]
    
      edges <- symedge[symedge$node1 %in% element$node1 & symedge$node2 %in% element$node1, ]
    
      sums[[j]] <- sum(edges$value)
    
      rm(element, edges)
    }
  
    sums <- as.numeric(unlist(sums))
  
    Aij_List[[i]] <- sum(sums)
  
    rm(group, partition, sums)
  }

  sumAij <- as.numeric(unlist(Aij_List))
  grpN <- as.numeric(unlist(Group_List))

  rm(col_names, Aij_List, Group_List, i, j)

#********************************************************************   -modularity-   ***********************************************************************************************#

  #Getting resoultion values for the unique set of cluster
  res_values <- colnames(degree)[-c(1:2)]
  res <- vector('numeric', length(res_values))
  for (i in seq_along(res_values)){
    res_i <- (strsplit(res_values[[i]], '_'))[[1]]
    res_i <- res_i[res_i != ""]
    res_i <- as.numeric(res_i[[2]])
    res[i] <- res_i
    rm(res_i)
  }

  rm(res_values)

  #Creating Modularity Table
  modularity <- as.data.frame(cbind(partitions, sumPij, sumAij, grpN), stringsAsFactors=FALSE)

  #Calculating Modularity, Q, and the Slope
  modularity$modularity <-  (sumAij-(res*sumPij))/m
  modularity$Q <- (sumAij-(res*sumPij))
  modularity$slope <- -1*(sumPij)
  modularity$resolution <- res

#******************************************************************   -visualization-   **********************************************************************************************#

  #Sorting Modularity by Slope
  modularity <- modularity[order( modularity[,7] ),]

  #Identifying Unique Slope Values
  sweep <- ave(rep(1, nrow(modularity)), modularity[7], FUN = seq_along)

  sg2 <- numeric(length(sweep))
  count <- 0

  for (i in seq_along(sg2)){
    if (sweep[[i]] == 1){
      sg2[[i]] <- count + 1
    } else {
      sg2[[i]] <- count
    }
  
    count <- sg2[[i]]
  }

  modularity$sg2 <- sg2

  rm(sweep, count, sg2, i)

  modularity$SweepMarker_X <- modularity$resolution
  modularity$SweepMarker_Y <- modularity$Q

  #Transforming to numeric
  modularity$sumPij <- as.numeric(modularity$sumPij)
  modularity$sumAij <- as.numeric(modularity$sumAij)
  modularity$grpN <- as.numeric(modularity$grpN)

  rm(partitions, sumAij, sumPij, grpN, node1)

  #Isolating the Best Solutions for Visualtion (Highest Q for each Resolution Value)

  #Sorting by Resolution Value and in Descending Order by Q
  sort_asc <- "resolution"
  sort_desc <- c("Q")

  modularity <- modularity[do.call(order, c(as.list(modularity[sort_asc]), lapply(modularity[sort_desc], function(x) -xtfrm(x)))),]

  rm(sort_asc, sort_desc)

  #Deleting non-optimal values
  `Optimal Modularity` <- modularity[!duplicated(modularity$resolution),]

  #Ensuring the axses labels are not in scientific notation
  options(scipen=10000)

  #Creating Visualization
  zero <- c(0)
  labels <- as.character(`Optimal Modularity`$resolution)
  
  png("p_1.png", width = 745, height = 616) 
    # set graphical area
    par(bty="n", bg=palette[1], mar=c(5,7,1,5))
    plot(`Optimal Modularity`$resolution, `Optimal Modularity`$Q, xlim=c(0, 1.8),
       xlab = "Resolution Parameter Gamma", ylab = " ", family='HersheySerif',
       xaxt = 'n', yaxt = 'n', cex.lab=1.5)
  
    #Adding Slope Lines
    for (i in seq_along(`Optimal Modularity`$sumAij)){
      abline(a=`Optimal Modularity`$sumAij[i], `Optimal Modularity`$slope[i], col = 'snow2')
    }
  
    #Adding Droplines
    segments(`Optimal Modularity`$resolution, zero, `Optimal Modularity`$resolution, `Optimal Modularity`$Q)
  
    #Adding Resolution Value Points
    lines(`Optimal Modularity`$resolution, `Optimal Modularity`$Q, type = "p", pch = 19, col=colors)
  
    #Adding Labels
    text(`Optimal Modularity`$resolution, `Optimal Modularity`$Q, labels, cex=0.6, pos=3, col="black")
  
    #Specifying Axses
    axis(side=1,at=c(0, 0.50, 0.75, 1.00, 1.25, 1.50, 1.8), labels=c("0", "0.5", "0.75", "1", '1.25', "1.5", "1.8"), font = 2, family = 'HersheySerif')
    axis(side=2, font = 2, family='HersheySerif', las=1)
  
    #Adding Y-Axis Text
    mtext(side = 2, text = 'Quality Score (non-normalized)', line = 5, cex = 1.5, family='HersheySerif')
  
    #Adding Group Size
    par(new = T, mar = c(5,7,1,5))
    with(`Optimal Modularity`, plot(resolution, grpN, pch=4, axes=F, xlab=NA, ylab=NA, 
                                  cex=1.5, xlim=c(0, 1.8), col = 'darkolivegreen'))
    axis(side = 4, cex.axis = 1, font = 2, family = 'HersheySerif')
    mtext(side = 4, line = 3, cex=1.5, 'Number of Communities', family = 'HersheySerif')
  dev.off()
  
  g2 <- magick::image_read('p_1.png')
  file.remove('p_1.png')

  #Restoring scientific notation default
  options(scipen=0)  

  rm(zero, labels, m)

#**************************************************************   -Adjusted Rand Score Distribution by Partition-   *********************************************************************#

#Example Source: https://stats.stackexchange.com/questions/207366/calculating-the-adjusted-rand-index

#The purpose of the Adjusted Rand Score is to determine for the following relationships given chance.
  #The number of pairs of elements in  S S that are in the same subset in  X and in the same subset in  Y
  #The number of pairs of elements in S that are in different subsets in  X and in different subsets in Y
  #The number of pairs of elements in S that are in the same subset in X and in different subsets in Y
  #The number of pairs of elements in S that are in different subsets in X and in the same subset in Y

#Given a set S of n elements, and two groupings or partitions, the overlap between X and Y  described in the relationship above can be summarized in a contingency table,
#where cell elements indicate counts of shared elments between x and y.

#Adjusted Rand Index Contingency Table Components
  #nij is across the diagonal (i.e., when i = j)
  #ai is the row sums
  #bj is the column sums

#𝐴𝑅𝐼=∑𝑖𝑗(𝑛𝑖𝑗2)−[∑𝑖(𝑎𝑖2)∑𝑗(𝑏𝑗2)]/(𝑛2)12[∑𝑖(𝑎𝑖2)+∑𝑗(𝑏𝑗2)]−[∑𝑖(𝑎𝑖2)∑𝑗(𝑏𝑗2)]/(𝑛2)

#Example Matrix
  #       x1    x2    x3    Sums
  #y1     1     1     0      2
  #y2     1     2     1      4
  #y3     0     0     4      4
  #Sums   2     3     5

#Example (n\choose 2) is calculated as n(n-1)/2:
  #Identity: 1(0)/2 = 0, 2(1)/2 = 1, 4(3)/2 = 6 == 7
  #Row Sums: 2(1)/2 = 1, 4(3)/2 = 6, 4(3)/2 = 6 == 13
  #Column Sums: 2(1)/2 = 1, 3(2)/2 = 3, 5(4)/2 = 10 == 14

#Sum of the Adjacency Matrix: 10 which we use in the denominator C(10,2) or 10(9)/2 = 45

#Breakdown by Components: nij example C(1,2), C(2,2), C(4,2) 
  #∑𝑖𝑗(𝑛𝑖𝑗2)=(12)+(22)+(42)=7
  #∑𝑖(𝑎𝑖2)=(22)+(42)+(42)=13
  #∑𝑗(𝑏𝑗2)=(22)+(32)+(52)=14

#ARI 
  #ARI = 7-13*14/45 / (13+14)/2 - 13*14/45 = 0.313

#Note: The sum over i,j in n_{ij} is not only across the diagonal, but across the entire matrix. Thus a sum over ALL entries i and j choose n
    
  #Isolate Partition Sets to Compare
  
  #Getting resoultion values for the unique set of cluster
  res_values <- colnames(ARI_partition)[-c(1)]
  res_id <- vector('numeric', length(res_values))
  for (i in seq_along(res_values)){
    res_i <- (strsplit(res_values[[i]], '_'))[[1]]
    res_i <- res_i[res_i != ""]
    res_i <- as.numeric(res_i[[2]])
    res_id[i] <- res_i
    rm(res_i)
  }
  
  rm(res_values)

  partitions <- unique(res_id)

  aggregate_ARI <- vector('list', length(partitions))
  names(aggregate_ARI) <- paste0(unique(res))

  rm(res_id)

  #Looping through each resolution value and calculating the ARI for each partition set
  for (i in seq_along(partitions)){
    partition_set <- ARI_partition[,grepl(partitions[[i]], colnames(ARI_partition))]

    p_cols <- as.data.frame(as.matrix(t(combn(colnames(partition_set),2))))
    colnames(p_cols)[[1]] <- 'Partition 1'
    colnames(p_cols)[[2]] <- 'Partition 2'
  
    p_cols[] <- lapply(p_cols, as.character)
  
    `Partition Set` <- paste(p_cols[[1]], p_cols[[2]], sep="," )

      col_list <- vector('list', length(`Partition Set`))
      for (j in seq_along(col_list)){
        col_list[[j]] <- as.character(unlist(strsplit(`Partition Set`[[j]], ",")))
      }

    #Creating a vector to contain the ARI results for each partition pair
    res_ARI <- vector('list', nrow(p_cols))

    rm(p_cols, `Partition Set`, j)

      #Calculating the ARI for each partition pair 
      for (j in seq_along(col_list)){
        #Isolating the pair of partitions
        p_set <- partition_set[col_list[[j]]]
        p_set[] <- lapply(p_set, as.character)

        #Getting total and identity elements for future use
        #p_set <- xtabs(~p_set[[1]] + p_set[[2]] ,data= p_set)
        #identity <- as.numeric(diag(as.matrix(rbind(p_set))))
        #total <- as.numeric(sum(p_set))

        #Creating Contingency Table for Calculations
        #p_set <- partition_set[col_list[[j]]]
        #p_set[] <- lapply(p_set, as.character)
        #p_set <- addmargins(xtabs(~p_set[[1]] + p_set[[2]] ,data= p_set))
        #p_set <- data.frame(rbind(p_set))

        #Get Sums
        #number <- nrow(p_set) - 1
        #col_sums <- (p_set$Sum)[1:number]
  
        #number <- ncol(p_set) - 1
        #row_sums <- as.numeric((p_set[row.names(p_set) %in% 'Sum', ])[1:number])

        #Calculating nij, ai, and bj elements
        #p_mat <- vector('list', 3)
        #  p_mat[[1]] <- col_sums*(col_sums-1)/2
        #  p_mat[[2]] <- row_sums*(row_sums-1)/2
        #  p_mat[[3]] <- identity*(identity-1)/2

        #total <- (total*(total-1)/2)  
        #n_ij <- sum(p_mat[[3]])
        #a_i <- sum(p_mat[[2]])
        #b_j <- sum(p_mat[[1]])

        #Calculating ARI
        #nominator <- n_ij-b_j*a_i/total
        #denominator <- ((b_j+a_i)/2) - ((b_j*a_i)/total)

        #res_ARI[[j]] <- nominator/denominator
        res_ARI[[j]] <-  fossil::adj.rand.index(p_set[[1]], p_set[[2]])
        
        res_ARI <- rapply(res_ARI, f=function(x) ifelse(is.nan(x),1,x), how="replace")

        #rm(p_set, identity, total, number, col_sums, row_sums, p_mat, n_ij, a_i, b_j, nominator, denominator)
        rm(p_set)
      }

    #Calculating the Aggregate ARI statisics
    res_ARI <- as.numeric(unlist(res_ARI))
    mean_ARI <- mean(res_ARI)
    sd_ARI <- sd(res_ARI)
    se_ARI <- sd_ARI/sqrt(length(res_ARI))
    error <- qt(0.975,df=length(res_ARI)-1)*se_ARI
    Lower_CI <- mean_ARI - error
    Upper_CI <- mean_ARI + error

    aggregate_ARI[[i]] <- as.data.frame(cbind(mean_ARI, sd_ARI, se_ARI, Lower_CI, Upper_CI))

    rm(res_ARI, mean_ARI, sd_ARI, se_ARI, error, Lower_CI, Upper_CI, j, col_list)
  }

  #Making into data.farme
  aggregate_ARI <- do.call("rbind", aggregate_ARI)
  
  aggregate_ARI$Resolution <- as.numeric(row.names(aggregate_ARI))
  
  #Sorting for presentation purposes
  aggregate_ARI <- aggregate_ARI[order(aggregate_ARI[,6] ), ]
  
  #Adding Group Sizes to file
  aggregate_ARI$grpN <- `Optimal Modularity`$grpN
   colnames(aggregate_ARI)[[1]] <- 'Mean Adjusted Rand Score'
    
#***************************************************************************   -ARI Visualization-   *************************************************************************************#

  #Creating Polygon Dimensions for the confidence band
  plot(0)
  val.curve_1 <- xspline(aggregate_ARI[c(6,4)], shape = -0.5, draw = FALSE)
  val.curve_2 <- xspline(aggregate_ARI[c(6,5)], shape = -0.5, draw = FALSE)
  
  val.curve_1 <- data.frame(val.curve_1)
  val.curve_2 <- data.frame(val.curve_2)
  
  junk <- dev.off(which = dev.cur())
  
  val.diff <- abs(nrow( val.curve_1)- nrow( val.curve_2))
  if (nrow(val.curve_2) > nrow(val.curve_1)) {
    val.curve_2 <- val.curve_2[-seq((nrow(val.curve_2)-1), nrow(val.curve_2)-val.diff),]
  } else if (nrow(val.curve_2) < nrow(val.curve_1)) {
    val.curve_1 <- val.curve_1[-seq((nrow(val.curve_1)-1), nrow(val.curve_1)-val.diff),]
  } else {
    val.curve_2
  }
  
  png("p_2.png", width = 745, height = 616) 
    # set graphical area
    par(bty="n", bg=palette[1], mar=c(5,5,1,5), family = 'HersheySerif')

    palette(rainbow(9))
  
    plot(0, type='n', xlim=c(0, 1.8), ylim=c(0, 1),
       xlab = "Resolution Parameter Gamma", ylab = "Mean Adjusted Rand Score", xaxt = 'n', yaxt='n', cex.lab=1.5, family = 'HersheySerif')
    
    polygon(c(val.curve_1$x,rev(val.curve_1$x)), 
            c(val.curve_1$y,rev(val.curve_2$y)) ,
            col='gray94', border = NA)
    
    #Adding Mean Line
    lines(aggregate_ARI$Resolution, aggregate_ARI$`Mean Adjusted Rand Score`, col = "black", type = "l",  lty = 1, lwd = 2.3)

    #Adding points
    lines(aggregate_ARI$Resolution, aggregate_ARI$`Mean Adjusted Rand Score`, type = "p", pch = 19, col =colors, xaxt="n", yaxt="n")

    #Specifying Axses
    axis(side=1,at=c(0, 0.5, 0.75, 1, 1.50, 1.80), font = 2, family = 'HersheySerif')
    axis(side=2,at=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), font = 2, las=1, family = 'HersheySerif')
  
    # plot gridlines
    grid(lwd = 2)

    #Adding Group Size
    par(new = T, mar = c(5,5,1,5))
    with(aggregate_ARI, plot(Resolution, grpN, pch=4, axes=F, xlab=NA, ylab=NA, cex=1.5, xlim=c(0, 1.8), col = 'darkolivegreen'))
    axis(side = 4, cex.axis = 1, font = 2, family = 'HersheySerif')
    mtext(side = 4, line = 3, cex=1.5, 'Number of Communities')
  
    #Adding legend
    legend("bottomright", legend=c("Confidence Interval"), fill=c('snow2'), bty="n") 
  dev.off()

  #Re-Importing Image
  g3 <- magick::image_read('p_2.png')
  file.remove('p_2.png')

  rm(val.curve_1, val.curve_2, i)

  #Making GROBS and Creating Combined Summary Image
  g_2 <- image_annotate(g2, "Convex Hull of Modularity Lines", size = 20, gravity = "northwest", color = "black", font='sans', location = "+0-4")
  p_2 <- ggplotify::as.ggplot(g2)
  g_2 <- ggplotify::as.ggplot(g_2)

  g_3 <- image_annotate(g3, "Pairwise Adjusted Rand Scores by Resolution Value", size = 20, gravity = "northwest", color = "black", font='sans', location = "+0-4")
  p_3 <- ggplotify::as.ggplot(g3)
  g_3 <- ggplotify::as.ggplot(g_3)

  #Creating Panel Plots
  lay <- rbind(c(1,2))
  p_4 <- ggplotify::as.ggplot(gridExtra::grid.arrange(grobs = list(g_2, g_3), layout_matrix = lay))

  #writing objects to the Global Environment
  Modularity <-  assign(x = "Modularity", value = modularity,.GlobalEnv) 
  `Optimal Modularity` <-  assign(x = "Optimal Modularity", value = `Optimal Modularity`,.GlobalEnv) 
  `Aggregate ARI Statistics` <- assign(x = "Aggregate ARI Statistics", value = aggregate_ARI,.GlobalEnv) 
  `Convex Hull Optimality` <- assign(x = "Convex Hull Optimality", value = p_2,.GlobalEnv) 
  `ARI Partition Consensus` <- assign(x = "ARI Partition Consensus", value = p_3,.GlobalEnv) 
  `Modularity Profile` <-  assign(x = "Modularity Profile", value = p_4,.GlobalEnv) 

  #Removing plot objects
  rm(lay, g2, g_2, g3, g_3, degree,clu, clu_t, colors)
}