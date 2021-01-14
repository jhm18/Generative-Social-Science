#Jonathan H. Morgan
#ACT: Cosine Similarity Analyses
#30 August 2019

################
#   PACKAGES   #
################

#Install Packages Necessary for the Script if Missing
list.of.packages <- c('lattice', 'ggplot2', 'magick', 'ggplotify', 'gridExtra', 'ggrepel','svglite', 'MASS', 'scales')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(lattice)
library(ggplot2)
library(magick)
library(svglite)
library(ggplotify)
library(ggrepel)
library(gridExtra)
library(MASS)
library(RColorBrewer)
library(scales)

rm(list.of.packages, new.packages)

#################
#   FUNCTIONS   #
#################

#Specifying the Cosine Function
cosine <- function(x, y) {
  crossprod(x, y) / (sqrt(crossprod(x, x)) * sqrt(crossprod(y, y)))
}

#Angular cosine distance: https://www.itl.nist.gov/div898/software/dataplot/refman2/auxillar/cosdist.htm
angular_distance <- function(cosine) {
  (2 * acos(cosine))/ pi
}

#Generates a color palette for each distance increment
colfunc <- colorRampPalette(c("black", "white"))

#Trims white space from the labels 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#################################
#   Cultural Similarity Metric  #
#################################

act_cosine <- function(data, scale_min, scale_max, titles, contour_titles, barplot_lables) {
  #CONSTRUCTING SIMILARITY VECTORS

  #Round E, P, and A Vectors to the Closest 10th decimal place--not necessary in these analyses because this is already true in the data
  for (i in seq_along(data)){
    for (j in seq_along(data[[i]]) ){
      data[[i]][[j]][[2]] <- as.numeric(round(data[[i]][[j]][[2]], 2))
      data[[i]][[j]][[3]] <- as.numeric(round(data[[i]][[j]][[3]], 2))
      data[[i]][[j]][[4]] <- as.numeric(round(data[[i]][[j]][[4]], 2))
    }
  }

  #Generating List Elements
  terms <- data
  for (i in seq_along(terms)){
    for (j in seq_along(terms[[i]]) ){
      terms[[i]][[j]] <- unique(data[[i]][[j]][[1]])
    }
  }

  id <- vector('list', length(data))
  for (i in seq_along(id)){
    id[[i]] <- round(seq(scale_min[[i]], scale_max[[i]], by=0.1), 2)
  }

  vectors <- data
  for (i in seq_along(terms)){
    for (j in seq_along(terms[[i]])){
      vectors[[i]][[j]] <- vector('list', length(terms[[i]][[j]]))
    }
  }

  for (i in seq_along(vectors)){
    for (j in seq_along(vectors[[i]])){
      for (k in seq_along(terms[[i]][[j]])){
        vectors[[i]][[j]][[k]] <- as.data.frame(cbind(terms[[i]][[j]][[k]], id[[i]]), stringsAsFactors=FALSE)
      }
    }
  }

  for (i in seq_along(vectors)){
    for (j in seq_along(vectors[[i]])){
    vectors[[i]][[j]] <- do.call("rbind", vectors[[i]][[j]])
    }
  }

  #Creating cannonical data.frame
  cannonical <- vectors
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]])){
      colnames(cannonical[[i]][[j]]) <- c('Term ID', 'Value')
      cannonical[[i]][[j]][[2]] <- as.numeric(cannonical[[i]][[j]][[2]])
    }
  }

  rm(vectors, id)

  #Getting counts for each concept by value
  e_counts <- data
  for (i in seq_along(e_counts)){
    for (j in seq_along(e_counts[[i]]) ){
    e_counts[[i]][[j]] <- aggregate(e_count ~ data[[i]][[j]]$`Term ID` + data[[i]][[j]]$E, transform(data[[i]][[j]], e_count = 1), length)

    e_counts[[i]][[j]] <- e_counts[[i]][[j]][order(e_counts[[i]][[j]][[1]], e_counts[[i]][[j]][[2]]), ]

    colnames(e_counts[[i]][[j]]) <- c('Term ID', 'E', 'e_counts')
    }
  }

  p_counts <- data
  for (i in seq_along(e_counts)){
    for (j in seq_along(e_counts[[i]]) ){
    p_counts[[i]][[j]] <- aggregate(p_count ~ data[[i]][[j]]$`Term ID` + data[[i]][[j]]$P, transform(data[[i]][[j]], p_count = 1), length)
  
    p_counts[[i]][[j]] <- p_counts[[i]][[j]][order(p_counts[[i]][[j]][[1]], p_counts[[i]][[j]][[2]]), ]
  
    colnames(p_counts[[i]][[j]]) <- c('Term ID', 'P', 'p_counts')
    }
  }

  a_counts <- data
  for (i in seq_along(e_counts)){
    for (j in seq_along(e_counts[[i]]) ){
    a_counts[[i]][[j]] <- aggregate(a_count ~ data[[i]][[j]]$`Term ID` + data[[i]][[j]]$A, transform(data[[i]][[j]], a_count = 1), length)
  
    a_counts[[i]][[j]] <- a_counts[[i]][[j]][order(a_counts[[i]][[j]][[1]], a_counts[[i]][[j]][[2]]), ]
  
    colnames(a_counts[[i]][[j]]) <- c('Term ID', 'A', 'a_counts')
    }
  }

  #Merging count data.frames with cannonical

  #E
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]]) ){
    table <- cannonical[[i]][[j]]
      colnames(table)[[2]] <- 'E'
  
    table <- merge(x = table, y = e_counts[[i]][[j]], by = c('Term ID', 'E'), all.x = TRUE)

    table[is.na(table)] <- 0

    table <- table[order(table[[1]], table[[2]]), ]

    colnames(table)[[2]] <- 'Value'

    cannonical[[i]][[j]] <- merge(x = cannonical[[i]][[j]], y = table, by = c('Term ID', 'Value'), all.x = TRUE)

    cannonical[[i]][[j]] <- cannonical[[i]][[j]][order(cannonical[[i]][[j]][[1]], cannonical[[i]][[j]][[2]]), ]

    rm(table)
    }
  }

  #P
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]]) ){
      table <- cannonical[[i]][[j]][c(1:2)]
        colnames(table)[[2]] <- 'P'
    
      table <- merge(x = table, y = p_counts[[i]][[j]], by = c('Term ID', 'P'), all.x = TRUE)
    
      table[is.na(table)] <- 0
    
      table <- table[order(table[[1]], table[[2]]), ]
    
      colnames(table)[[2]] <- 'Value'
    
      cannonical[[i]][[j]] <- merge(x = cannonical[[i]][[j]], y = table, by = c('Term ID', 'Value'), all.x = TRUE)
    
      cannonical[[i]][[j]] <- cannonical[[i]][[j]][order(cannonical[[i]][[j]][[1]], cannonical[[i]][[j]][[2]]), ]
    
      rm(table)
    }
  }

  #A
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]]) ){
      table <- cannonical[[i]][[j]][c(1:2)]
        colnames(table)[[2]] <- 'A'
    
      table <- merge(x = table, y = a_counts[[i]][[j]], by = c('Term ID', 'A'), all.x = TRUE)
    
      table[is.na(table)] <- 0
    
      table <- table[order(table[[1]], table[[2]]), ]
    
      colnames(table)[[2]] <- 'Value'
    
      cannonical[[i]][[j]] <- merge(x = cannonical[[i]][[j]], y = table, by = c('Term ID', 'Value'), all.x = TRUE)
    
      cannonical[[i]][[j]] <- cannonical[[i]][[j]][order(cannonical[[i]][[j]][[1]], cannonical[[i]][[j]][[2]]), ]
    
      rm(table)
    }
  }

  #Creating Sum Variables for e, p, and a to normalize the counts for the puproses of comparison
  e_sum <- cannonical
  for (i in seq_along(e_sum)){
    for (j in seq_along(e_sum[[i]])){
        e_sum[[i]][[j]] <- aggregate(cannonical[[i]][[j]]$e_counts, by=list(Category=cannonical[[i]][[j]]$`Term ID`), FUN=sum)
        colnames(e_sum[[i]][[j]]) <- c('Term ID', 'e_sum')
    }   
  }

  p_sum <- cannonical
  for (i in seq_along(p_sum)){
    for (j in seq_along(p_sum[[i]])){
      p_sum[[i]][[j]] <- aggregate(cannonical[[i]][[j]]$p_counts, by=list(Category=cannonical[[i]][[j]]$`Term ID`), FUN=sum)
      colnames(p_sum[[i]][[j]]) <- c('Term ID', 'p_sum')
    }   
  }

  a_sum <- cannonical
  for (i in seq_along(a_sum)){
    for (j in seq_along(a_sum[[i]])){
      a_sum[[i]][[j]] <- aggregate(cannonical[[i]][[j]]$a_counts, by=list(Category=cannonical[[i]][[j]]$`Term ID`), FUN=sum)
      colnames(a_sum[[i]][[j]]) <- c('Term ID', 'a_sum')
    }   
  }

  #Merging sum_lists 
  sum_list <- cannonical
  for (i in seq_along(sum_list)){
    for (j in seq_along(sum_list[[i]]) ){
      sum_list[[i]][[j]] <-  merge(x = e_sum[[i]][[j]], y = p_sum[[i]][[j]], by = c('Term ID'), all.x = TRUE)
      sum_list[[i]][[j]] <-  merge(x = sum_list[[i]][[j]], y = a_sum[[i]][[j]], by = c('Term ID'), all.x = TRUE)
    }
  }

  #Merging Frequencies to generate Proportions
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]]) ){
      cannonical[[i]][[j]] <- merge(x = cannonical[[i]][[j]], y = sum_list[[i]][[j]], by = c('Term ID'), all.x = TRUE)
    }
  }

  rm(e_counts, e_sum, p_counts, p_sum, a_counts, a_sum, sum_list, terms)

  #Generating Proportions to Create Similarity Strings
  for (i in seq_along(cannonical)){
    for (j in seq_along(cannonical[[i]]) ){
      cannonical[[i]][[j]]$e_proportion <- cannonical[[i]][[j]]$e_counts/cannonical[[i]][[j]]$e_sum
      cannonical[[i]][[j]]$p_proportion <- cannonical[[i]][[j]]$p_counts/cannonical[[i]][[j]]$p_sum
      cannonical[[i]][[j]]$a_proportion <- cannonical[[i]][[j]]$a_counts/cannonical[[i]][[j]]$a_sum
    
      cannonical[[i]][[j]] <- cannonical[[i]][[j]][order(cannonical[[i]][[j]][[1]], cannonical[[i]][[j]][[2]]), ]
    }
  }

  #CALCULATING COSINE SIMILARITY
  #Calculate the cosine similarity for all concepts pairs for each dimension (intersection step already performed in the data steps) 
  
  names <- names(cannonical)
  cosine_list <- vector('list', length(cannonical))
  for (i in seq_along(cannonical)){
      cosine_list[[i]] <- as.data.frame(cbind(cannonical[[i]][[1]]$`Term ID`, cannonical[[i]][[1]]$Value, cannonical[[i]][[1]]$e_proportion, cannonical[[i]][[1]]$p_proportion,
                                              cannonical[[i]][[1]]$a_proportion, cannonical[[i]][[2]]$e_proportion, cannonical[[i]][[2]]$p_proportion,
                                              cannonical[[i]][[2]]$a_proportion), stringsAsFactors=FALSE)
      
      colnames(cosine_list[[i]]) <- c('Term ID', 'Value', 'Culture 1 E', 'Culture 1 P', 'Culture 1 A', 
                                      'Culture 2 E', 'Culture 2 P', 'Culture 2 A')
  }
                
  names(cosine_list) <- names
  rm(names)

  #Transforming Variables to be Numeric
  for (i in seq_along(cosine_list)){
    cosine_list[[i]][2:8] <- lapply(cosine_list[[i]][2:8], as.numeric)
  }

  #Calculating the cosine similarity of each concept pair by dimensions
  row_list <- cosine_list
  for (i in seq_along(cosine_list)){
    row_list[[i]] <- unique(cosine_list[[i]]$`Term ID`)
  }

  #E
  e_cosine <- row_list
  for (i in seq_along(e_cosine)){
    e_cosine[[i]] <- rep(0, length(row_list[[i]]))
  }

  for (i in seq_along(e_cosine)){
    for (j in seq_along(e_cosine[[i]]) ){
    e_cosine[[i]][[j]] <-  cosine(cosine_list[[i]][[3]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ],
                                  cosine_list[[i]][[6]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ])
    }
  }

  #P
  p_cosine <- row_list
  for (i in seq_along(p_cosine)){
    p_cosine[[i]] <- rep(0, length(row_list[[i]]))
  }

  for (i in seq_along(p_cosine)){
    for (j in seq_along(p_cosine[[i]]) ){
     p_cosine[[i]][[j]] <-  cosine(cosine_list[[i]][[4]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ],
                                  cosine_list[[i]][[7]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ])
    }
  }

  #A
  a_cosine <- row_list
  for (i in seq_along(a_cosine)){
    a_cosine[[i]] <- rep(0, length(row_list[[i]]))
  }

  for (i in seq_along(a_cosine)){
    for (j in seq_along(a_cosine[[i]]) ){
      a_cosine[[i]][[j]] <-  cosine(cosine_list[[i]][[5]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ],
                                  cosine_list[[i]][[8]][cosine_list[[i]][[1]] %in% row_list[[i]][[j]] ])
    }
  }

  #Constructing Pairwise Similarity Table
  similarity <- vector('list', length(cosine_list))
  for (i in seq_along(similarity)){
    similarity[[i]] <- as.data.frame(cbind(row_list[[i]], e_cosine[[i]], p_cosine[[i]], a_cosine[[i]]), stringsAsFactors=FALSE)
    colnames(similarity[[i]]) <- c('Term ID', 'cosine e', 'cosine p', 'cosine a')
  }
  names(similarity) <- names(cannonical)

  rm(row_list, e_cosine, p_cosine, a_cosine)

  #Transforming Variables to be Numeric
  for (i in seq_along(similarity)){
    similarity[[i]][2:4] <- lapply(similarity[[i]][2:4], as.numeric)
  }

  #Calculating Angular Distance
  for (i in seq_along(similarity)){
    similarity[[i]]$`e distance` <-  angular_distance(similarity[[i]][[2]])
    similarity[[i]]$`p distance` <-  angular_distance(similarity[[i]][[3]])
    similarity[[i]]$`a distance`  <-  angular_distance(similarity[[i]][[4]])
  }

  #Calculating Angular Similarity
  for (i in seq_along(similarity)){
    similarity[[i]]$`angular similarity e` <- 1 - similarity[[i]]$`e distance`
    similarity[[i]]$`angular similarity p` <- 1 - similarity[[i]]$`p distance`
    similarity[[i]]$`angular similarity a` <- 1 - similarity[[i]]$`a distance`
  }
  
  #Eliminating Any NaN Values
  for (i in seq_along(similarity)){
    similarity[[i]]  <- na.omit(similarity[[i]])
  }

  #Normalize distances: distancei/max(distance)
  e_max <- lapply(similarity, function(x) max(x$`e distance`))
  p_max <- lapply(similarity, function(x) max(x$`p distance`))
  a_max <- lapply(similarity, function(x) max(x$`a distance`))

  for (i in seq_along(similarity)){
    similarity[[i]]$`max e distance` <- e_max[[i]]
    similarity[[i]]$`max p distance` <- p_max[[i]]
    similarity[[i]]$`max a distance` <- a_max[[i]]
  }

  #Calculate the total distance for each concept divided by the total distance possible (3)
  for (i in seq_along(similarity)){
    similarity[[i]]$`epa distance` <- (similarity[[i]]$`e distance` + similarity[[i]]$`p distance` + similarity[[i]]$`a distance`)/3
  }

  sum_distance <- lapply(similarity, function(x) sum(x$`epa distance`))

  #Incorporating the Sum of Distances and Distance Proportion (sum of observed/total distance possible)
  for (i in seq_along(similarity)){
    similarity[[i]]$`Distance Sum` <- sum_distance[[i]]
    similarity[[i]]$`Distance Proportion` <- sum_distance[[i]]/nrow(similarity[[i]])
  }

  #Adding List Names in Preparation for Creating Stacked Data Set
  for (i in seq_along(similarity)){
    similarity[[i]] <- cbind(names(similarity)[[i]], similarity[[i]])
    colnames(similarity[[i]])[[1]] <- c('Compared Cultures')
  }
  
  #Trimming 'Term ID' to ensure that merging does not produce duplicates
  for (i in seq_along(similarity)){
    similarity[[i]]$`Term ID` <- trim(similarity[[i]]$`Term ID`)
  }

  rm(e_max, p_max, a_max, sum_distance, cosine_list, i, j)

######################
#   VISUALIZATIONS   #
######################

  #Creating Aggregate Statistics to Supplement Visualizations
  means <- vector('list', length(similarity))
  for (i in seq_along(similarity)){
    means[[i]] <- mean(similarity[[i]][[15]])
  }
  
  medians <- vector('list', length(similarity))
  for (i in seq_along(medians)){
    medians[[i]] <- median(similarity[[i]][[15]])
  }

  sd <- vector('list', length(similarity))
  for (i in seq_along(medians)){
    sd[[i]] <- sd(similarity[[i]][[15]])
  }
  
  se <- vector('list', length(similarity))
  for (i in seq_along(se)){
    se[[i]] <- sd[[i]]/sqrt(nrow(similarity[[i]]))
  }
 
  error <- vector('list', length(similarity)) 
  for (i in seq_along(error)){
    error[[i]] <- qt(0.975,df=nrow(similarity[[i]])-1)*se[[i]]
  }

  lower_ci <- vector('list', length(similarity))
  for (i in seq_along(lower_ci)){
    lower_ci[[i]] <- means[[i]] - error[[i]]
  }

  upper_ci <- vector('list', length(similarity))
  for (i in seq_along(upper_ci)){
    upper_ci[[i]] <- means[[i]] + error[[i]]
  }

  `95th Percentile` <- vector('list', length(similarity))
  for (i in seq_along(`95th Percentile`)){
    `95th Percentile`[[i]] <- quantile(similarity[[i]][[15]], 0.95)
  }

  `5th Percentile` <- vector('list', length(similarity))
  for (i in seq_along(`5th Percentile`)){
    `5th Percentile`[[i]] <- quantile(similarity[[i]][[15]], 0.05)
  }

  descriptives <- vector('list', length(similarity))
  for (i in seq_along(descriptives)){
    descriptives[[i]] <- as.data.frame(cbind(means[[i]], medians[[i]], sd[[i]], se[[i]],
                                       lower_ci[[i]], upper_ci[[i]], `5th Percentile`[[i]], `95th Percentile`[[i]]))
    colnames(descriptives[[i]]) <- c('mean', 'median', 'sd', 'se', 'lower ci', 'upper ci', '5th percentile', '95th percentile')
  }
  names(descriptives) <- names(similarity)
  
  rm(means, medians, sd, se, error, lower_ci, upper_ci, `5th Percentile`, `95th Percentile`)

#******************************************   -Distance Distribution Plots-   *************************************************#
  for (i in seq_along(titles)){
  #Getting Kernel Density
    viz_den <- density(similarity[[i]][[15]])

  #5th Percentile Polygon Range
   poly_range_5 <- density(similarity[[i]][[15]])$x > 0 & 
                   density(similarity[[i]][[15]])$x < descriptives[[i]]$`5th percentile`       

    #95 Percentile Polygon Range
    poly_range_95 <- density(similarity[[i]][[15]])$x >= descriptives[[i]]$`95th percentile` & 
                     density(similarity[[i]][[15]])$x < max(density(similarity[[i]][[15]])$x)

    viz_png <- paste0(titles[[i]],'.png')

    png(viz_png, width = 7.5, height = 4,  units = 'in', res=600) 
      #Defining Background and Dimensions
      par(bty="n", mar=c(5,5,1,5))
      plot(viz_den, xlim=c(0, 1), lwd = 2.3, main=" ", xlab = "Total Distance", ylab = "Density", family = 'serif',
            cex.lab=1.5,  xaxt = 'n')

      #Adding Shading for the 5th percentile
      polygon(c(0, density(similarity[[i]][[15]])$x[poly_range_5], descriptives[[i]]$`5th percentile`),               
          c(0, density(similarity[[i]][[15]])$y[poly_range_5], 0),                 
          col = "gray")

      #Adding Shading for the 95th percentile
      polygon(c(descriptives[[i]]$`95th percentile`, density(similarity[[i]][[15]])$x[poly_range_95], descriptives[[i]]$`95th percentile`),               
          c(0, density(similarity[[i]][[15]])$y[poly_range_95], 0),                 
          col = "gray")  

      #Adding Median Line
      abline(v = descriptives[[i]]$median, col = "dodgerblue4", lwd = 2)

      #Adding Mean Line
      abline(v = descriptives[[i]]$mean, col = "dodgerblue4", lty = 2, lwd = 2)

      #Adding Labels 
      Md.expression <- expression(paste(italic(Md)))

      text(descriptives[[i]]$median, (max(density(similarity[[i]][[15]])$y) - 0.5), Md.expression, cex = 1.5, pos = 2)

      text(descriptives[[i]]$mean, (max(density(similarity[[i]][[15]])$y) - 0.5), expression(bar(x)), cex = 1.5, pos = 4)

      #Specifying Axses
      axis(side=1,at=c(0, 0.1, 0.2, 0.3, 0.4, .5, 0.6, 0.7, 0.8, 0.9, 1), 
           labels=c("0", "0.1", "0.2", "0.3", '0.4', "0.5", "0.6", "0.7", "0.8", "0.9", "1"), font = 2, family = 'serif')

      axis(side=2, font = 2, family = 'serif')

      #Adding legend
      source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
      legend(x=0.8, y=max(density(similarity[[i]][[15]])$y), legend=c("5th and 95th Percentiles"), fill=c("gray"), 
             cex=1.3, box.cex=c(0.7,0.7), bty="n", x.intersp=0.2, xjust=0.2, xpd=TRUE)

      #Adding Title
      title(titles[[i]], adj=0, family='serif')
    dev.off()

    #Re-Importing Image
    p_1 <- magick::image_read(viz_png)

    file.remove(viz_png)

    rm(viz_den, poly_range_5, poly_range_95)

    p_1 <- ggplotify::as.ggplot(p_1)

    assign(x = titles[[i]], value = p_1,.GlobalEnv)   # save iteration to the uniquely named R object

    rm(p_1, Md.expression, viz_png)
  }

#********************************************************   -Scatter Contour Plot-   **************************************************************************#

  for (i in seq_along(titles)){  
    #Creating Colors Attribute
    k <- as.numeric(length(unique(round(similarity[[i]][[15]], 1))))
    my.cols <- colfunc(k)

    #Creating Labels
    label <- similarity[[i]][similarity[[i]][[15]] <= descriptives[[i]]$`5th percentile` | similarity[[i]][[15]] >= descriptives[[i]]$`95th percentile`, ]
    label <- label[order(label$`epa distance`), ]
    label <- label[c(1:7,(nrow(label)-7):(nrow(label))), ]
    label$label <-  sub("i_", " ", label[[2]]) 
    label$label <-  sub("b_", " ", label$label) 
    label$label <-  sub("m_", " ", label$label) 
    label$label <- trim(label$label)
    label <- label[c(2, 18)]

    #Merging Labels Back In
    viz_data <- merge(x = similarity[[i]], y = label, by = c('Term ID'), all.x = TRUE)
    
    #Removing Potentially Duplicated Rows
    viz_data <- viz_data[!duplicated(viz_data[c("Term ID")]),]

    #Scaling distances for the purposes of visualization
    x <- scale(similarity[[i]]$`e distance`, center=TRUE, scale=TRUE)
    y <- scale(similarity[[i]]$`p distance`, center=TRUE, scale=TRUE)
    activity <- scale(similarity[[i]]$`a distance`, center=TRUE, scale=TRUE)
    label <- viz_data$label

    viz_data <- as.data.frame(cbind(x, y, activity), stringsAsFactors=FALSE)
      viz_data[] <- lapply(viz_data[1:3], as.numeric)
      viz_data$label <- label
      colnames(viz_data) <- c('e distance scaled', 'p distance scaled', 'a distance scaled', 'label')
  
    #Getting rid of _ to make the labels a bit cleaner
    viz_data$label <-  sub("_", " ", viz_data$label) 
    viz_data$label <- trim(viz_data$label)
    rm(x, y, activity, label)

    #Adding Reference Object to Anchor Text
    bottom <- -3.95
    right <- 3.5
    info <- c('Points Sized by Activity Distance (Scaled)')
    zero <- 0
    y_max <- 4
    y_min <- -4
    x_max <- 4
    x_min <- -4

    #Compute 2D kernel density, see MASS book, pp. 130-131 as meausure of status gradient
    z <- kde2d(viz_data$`e distance scaled`, viz_data$`p distance scaled`, n=length(viz_data$`e distance scaled`))

    viz_png <- paste0(titles[[i]],'.png')
  
    png(viz_png, width = 12, height = 7,  units = 'in', res=600)
      layout(matrix(1:2,nrow=1),widths=c(0.9,0.1))
      par(bty="n", mar=c(5,5,1,5), xpd=TRUE)
      plot(jitter(viz_data$`e distance scaled`, factor=1.5), jitter(viz_data$`p distance scaled`, factor=1.9), type = "p", pch=19, col=my.cols, 
      cex=viz_data$`a distance scaled`, cex.lab=1.5,  family = 'serif', xlab="Evaluation Distance (Scaled)", ylab="Potency Distance (Scaled)", 
      ylim=c(-5, 5), xlim=c(-5, 5))
    
      #plot gridlines
      grid(lwd = 2)
    
      #Adding Guidelines
      segments(zero, y_min, zero, y_max)
      segments(x_min, zero, x_max, zero)

      contour(z, drawlabels=FALSE, nlevels=k, col='blue', add=TRUE)

      #text(viz_data$`e distance scaled`, viz_data$`p distance scaled`, viz_data$label, cex=0.7, pos=2, col="black", offset = 1)
      text(right, bottom, info, cex=0.8, pos=1, col="black")

      axis(side=1, font = 2, family = 'serif')
      axis(side=2, font = 2, family = 'serif')

      title(titles[[i]], adj=0, family='serif')

      #Adding Legend
      xl <- 1
      yb <- 1
      xr <- 1.5
      yt <- 2

      par(mar=c(5,0.5,1,0.5))
      plot(NA,type="n", main="Total Distance", xlab = " ", ylab = " ", family = 'serif',
           xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n", cex.main=0.9, adj = 0)

      rect(
            xl,
            head(seq(yb,yt,(yt-yb)/k),-1),
            xr,
            tail(seq(yb,yt,(yt-yb)/k),-1),
            col=my.cols
      )

      mtext(k:1,side=2,at=tail(seq(yb,yt,(yt-yb)/k),-1)-0.05,las=2,cex=0.8)
    dev.off()
  
    #Re-Importing Image
    p_2 <- magick::image_read(viz_png)
  
    file.remove(viz_png)
  
    rm(k, my.cols, bottom,  right, info, zero, x_min, x_max, y_min, y_max, xl, xr, yb, yt, z)
  
    p_2 <- ggplotify::as.ggplot(p_2)
 
    #Ordering viz_data to check that scaling is sensible
    viz_data <- viz_data[order(viz_data[[1]], viz_data[[2]]), ]

    #Computing Graph Positions to Add Text Annotations
    e_graph_distance <- scales::rescale(viz_data$`e distance scaled`, to = c(0.2, 0.745)) 
    p_graph_distance <- scales::rescale(viz_data$`p distance scaled`, to = c(0.23, 0.84))
  
    #Creating Graph Distance Data.Frame to Generate Annotations
    graph_distance <- as.data.frame(cbind(e_graph_distance, p_graph_distance))
    graph_distance$label <- viz_data$label
  
    #Adding Annotations with geom_text_repel
    p_2 <- p_2 +
           geom_text_repel(data=graph_distance, aes(x=graph_distance[[1]], y=graph_distance[[2]], label = graph_distance[[3]]), force=0.3) 
  
    #Resaving with annotations so the annotations data does not have to be outputted
    png(viz_png, width = 12, height = 7,  units = 'in', res=600)
      lay <- rbind(c(1))
      grid.arrange(grobs = list(p_2), layout_matrix = lay)
    dev.off()
  
    p_2 <- magick::image_read(viz_png)
  
    p_2 <- ggplotify::as.ggplot(p_2)
  
    file.remove(viz_png)
  
    #Assigning to the Global Environment
    assign(x = contour_titles[[i]], value = p_2,.GlobalEnv)   # save iteration to the uniquely named R object
  
    rm(p_2, e_graph_distance, p_graph_distance, graph_distance, lay, viz_data)
  }

#************************************************   -Summary Bar Chart-   **********************************************************#
   #Creating Summary Data Set
  dist_summary <- vector('list', length(similarity))
  for (i in seq_along(dist_summary)){  
    dist_summary[[i]]  <- unique(similarity[[i]][[17]])
  }

  dist_summary <- as.data.frame(do.call("rbind", dist_summary))

  #Getting Labels and Proportions
  dist_labels <- lapply(titles, function(x) x)
  dist_labels <- as.character(unlist(dist_labels))

  dist_summary <- cbind(dist_labels, dist_summary)
    colnames(dist_summary) <- c('Label', 'Distance Proportion')
  
  #Round Distance Proportions for the Purposes of Visualization
  dist_summary$`Distance Proportion` <- round(dist_summary[[2]], 2)

  #Getting Sample Size for the Y axis
  sample_size <- vector('list', length(similarity))
  for (i in seq_along(sample_size)){  
    sample_size[[i]]  <- nrow(similarity[[i]])
  }

  sample_size <- as.numeric(unlist(sample_size))
  dist_summary$`sample size` <- sample_size

  #Creating X and Y Matrices to Be Able to Easily Annotate the Chart
  sample_size <- matrix(sample_size, nrow = 1, ncol= length(sample_size))
  y <- matrix(sample_size, nrow = 1, ncol= length(sample_size))

  png('Sample Comparison', width = 11, height = 5.06,  units = 'in', res=600)
    par(bty="n", mar=c(5,5,2.5,6)) 
    x <- barplot(sample_size[, 1:length(titles)], las = 1, xlab = "Country Comparisons", ylab = "Number of Compared Concepts", 
         col='gray', density=c(5,10,20,30) , angle=c(0,45,90,11), 
         ylim=c(0, (max(dist_summary[[3]]) + 0.05*max(dist_summary[[3]]))), 
         cex.lab = 1.5, width=dist_summary$`Distance Proportion`,
         cex.main = 1.5, family = 'serif', axes = FALSE, names.arg=barplot_lables, cex.names=1.1, font=2)
        
    axis(2, font = 2, family = 'serif')

    text(x,y+(max(dist_summary[[3]])*0.03),labels=as.character(dist_summary[[2]]), col='blue')

    title('Sample Distances', adj=0, family='serif')
    title(sub = "The number above the bar indicates the observed distance to the total distance possible.", cex.sub = 0.75, adj = 0) 
  dev.off()

  p_3 <- magick::image_read('Sample Comparison')

  p_3 <- ggplotify::as.ggplot(p_3)

  file.remove('Sample Comparison')

  #Assigning to the Global Environment
  assign(x = c('Culture Comparison'), value = p_3,.GlobalEnv)   # save iteration to the uniquely named R object

  rm(p_3, dist_summary, sample_size, dist_labels, i, x, y, viz_png)

  #Stacking  and Assigning Similarity for output
  similarity_data <- do.call("rbind", similarity) 
  assign(x = c('Culture Similarity Scores'), value = similarity_data,.GlobalEnv)   # save iteration to the uniquely named R object
  assign(x = c('similarity'), value = similarity, .GlobalEnv)
}
