#ACT Modal Analysis Function
#Jonathan H. Morgan
#18 May 2020

options(stringsAsFactors = FALSE)

#Modes Function
find_modes<- function(x) {
  modes <- NULL
  for ( i in 2:(length(x)-1) ){
    if ( (x[i] > x[i-1]) & (x[i] > x[i+1]) ) {
      modes <- c(modes,i)
    }
  }
  if ( length(modes) == 0 ) {
    modes = 'This is a monotonic distribution'
  }
  return(modes)
}

#Processing Function for Modes Analysis
  #concept: The list of concepts to be analyzed
  #concept_data: A 2 column data set where the first column is the list of concept and the second column is the values associated with the concept
  #name: Name of the output dataset

process_modes <- function(concept, concept_data, name) {
  #Creating a unique list of concepts from the evaluation data
  concepts <- as.character(unique(concept))
  concepts <- concepts[!is.na(concepts)]

  #Creating empty output dataset to populate with values
  output <- data.frame(matrix(ncol = 6, nrow = length(concepts)))
  colnames(output) <- c('concept', 'median', 'modal frequency', 'modal centers', 'mode n', 'modal means')
  
  output$concept <- concepts
  
  for (i in seq_along(output$concept)){
    concept <- concept_data[concept_data[[1]] %in% output$concept[[i]], ]
    concept <- as.numeric(concept[[2]])
    concept <- concept[!is.na(concept)]
  
    modes_indices <- find_modes(density(concept)$y) #you need to try it on the y axis
    
    mode <- density(concept)$x[modes_indices] #the actual modes
    
    #Identifying cut-point between the modes if more than one mode exists
    if (length(mode) > 1){
      cut_points <- vector('list', (length(mode)-1))
      pairs <- cbind(mode[-length(mode)], mode[-1])
      
      density <- density(concept)
      density <- as.data.frame(cbind(density$x, density$y), stringAsFactor=FALSE)
      colnames(density) <- c('x', 'y')
      
      for (j in seq_along(cut_points)){
        p_density <- density[density[[1]] >= pairs[j, 1] & density[[1]] <= pairs[j, 2], ]
        min_y <- min(p_density$y)
        cut_point <- p_density[p_density[[2]] == min_y, ]
        cut_points[[j]] <- cut_point$x
        rm(p_density, min_y, cut_point)
      }
      
      concept_min <- min(concept)
      concept_max <- max(concept)
      
      cut_points <- append(concept_min, cut_points)
      cut_points <- append(cut_points, concept_max)
      x_values <- as.numeric(cut_points[c(1:(length(cut_points)-1))])
      y_values <- as.numeric(cut_points[c(2:length(cut_points))])
      cut_points <- as.data.frame(cbind(x_values, y_values))
      
      distributions <- vector('list', length(mode))
      for (j in seq_along(distributions)){
        iteration <- seq(1, length(distributions), 1)
        if (j == min(iteration) ) {
          distributions[[j]] <- concept[concept >= cut_points[j, 1] & concept < cut_points[j,2]]
        } else if (j == max(iteration) ) {
          distributions[[j]] <- concept[concept > cut_points[j, 1] & concept <= cut_points[j,2]]
        } else {
          distributions[[j]] <- concept[concept > cut_points[j, 1] & concept <= cut_points[j,2]]
        }
        rm(iteration)
      }
      
      lengths <- vector('list', length(distributions))
      for (j in seq_along(lengths)){
        lengths[[j]] <- length(distributions[[j]])
      }
      
      #These Steps were to get a Centered Mean, but when the modes can be so small this makes less sense.
      
      #quartiles <- vector('list', length(distributions))
      #for (j in seq_along(quartiles)){
      #  quartiles[[j]] <- vector ('list', 2)
      #}
      
      #for (j in seq_along(distributions)){
      # quartiles[[j]][[1]] <- as.numeric(quantile(distributions[[j]], 0.25))
      #  quartiles[[j]][[2]] <- as.numeric(quantile(distributions[[j]], 0.75))
      #}
      
      #for (j in seq_along(distributions)){
      #  distributions[[j]] <- distributions[[j]][distributions[[j]] >= quartiles[[j]][[1]] & distributions[[j]] <= quartiles[[j]][[2]]]
      #}
      
      means <- vector('list', length(distributions))
      for (j in seq_along(means)){
        means[[j]] <- mean(distributions[[j]])
      }
      
      rm(pairs, cut_points, x_values, y_values)
      
    } else {
      distributions <- vector('list', length(mode))
      for (j in seq_along(distributions)){
        distributions[[j]] <- concept
      }
      
      lengths <- vector('list', length(distributions))
      for (j in seq_along(lengths)){
        lengths[[j]] <- length(distributions[[j]])
      }
      
      #quartiles <- vector('list', length(distributions))
      #for (j in seq_along(quartiles)){
      #  quartiles[[j]] <- vector ('list', 2)
      #}
      
      #for (j in seq_along(distributions)){
      #  quartiles[[j]][[1]] <- as.numeric(quantile(distributions[[j]], 0.25))
      #  quartiles[[j]][[2]] <- as.numeric(quantile(distributions[[j]], 0.75))
      #}
      
      #for (j in seq_along(distributions)){
      #  distributions[[j]] <- distributions[[j]][distributions[[j]] >= quartiles[[j]][[1]] & distributions[[j]] <= quartiles[[j]][[2]]]
      #}
      
      means <- vector('list', length(distributions))
      for (j in seq_along(means)){
        means[[j]] <- mean(distributions[[j]])
      }
      
    }
    #End of Concept Condition
    
    freq <- length(mode)
    mode <- paste(mode,collapse=" ")
    modal_feq <- paste(lengths, collapse= ' ')
    means <- paste(means,collapse=" ")
    
    output[i,2] <- median(concept)
    output[i,3] <- modal_feq
    output[i,4] <- mode
    output[i,5] <- freq
    output[i,6] <- means
    
    rm(concept, modes_indices, mode, freq, means, modal_feq, distributions)
  }
  #End of I-Loop
  
  #Formatting Steps
  modal_freq <- strsplit(output$`modal frequency`," ")
  
  # maximum length of the list items
  maxL <- max(sapply(modal_freq, length))
  
  # contstruct data.frame with NAs as fills
  modal_freq <- data.frame(do.call(rbind, lapply(modal_freq, function(i) c(i, rep(NA, maxL-length(i))))))
  
  #Naming Columns
  col_n <- seq(1:ncol(modal_freq))
  for (i in seq_along(col_n)){
    colnames(modal_freq)[[i]] <- paste('mode', i, "frequency")
  }
  
  modes <- strsplit(output$`modal centers`," ")
  
  # maximum length of the list items
  maxL <- max(sapply(modes, length))
  
  # contstruct data.frame with NAs as fills
  modes <- data.frame(do.call(rbind, lapply(modes, function(i) c(i, rep(NA, maxL-length(i))))))
  
  #Naming Columns
  col_n <- seq(1:ncol(modes))
  for (i in seq_along(col_n)){
    colnames(modes)[[i]] <- paste('modal center', i)
  }
  
  modal_means <- strsplit(output$`modal means`," ")
  
  # maximum length of the list items
  maxL <- max(sapply(modal_means, length))
  
  # contstruct data.frame with NAs as fills
  modal_means  <- data.frame(do.call(rbind, lapply(modal_means, function(i) c(i, rep(NA, maxL-length(i))))))
  
  #Naming Columns
  col_n <- seq(1:ncol(modal_means))
  for (i in seq_along(col_n)){
    colnames(modal_means)[[i]] <- paste('modal mean', i)
  }

  concept_modes <- cbind(output[1], output[2], modal_freq, modes, output[5], modal_means)
  colnames(concept_modes)[[1]] <- c('Concept')
  
  assign(x = name, value = concept_modes,.GlobalEnv)   # save iteration to the uniquely named R object
  
  rm(concepts, concept_data, density, modal_freq, modal_means, output, col_n, concept_max, concept_min, maxL, lengths, modes, name, concept_modes)
}
