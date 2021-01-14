#Stylized Base R Sankey
#Jonathan H. Morgan
#3 January 2029

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Home Machine
setwd("~/Desktop") 
getwd()

################
#   PACKAGES   # 
################

library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggplotify)
library(magick)

#################
#   FUNCTIONS   #
#################

#Home Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_13Dec2019.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

#Work Utilities
source("~/Desktop/FHP Micro Projects/R Plot Utilities_13Dec2019.R")
source("~/Desktop/FHP Micro Projects/util.R")

###############################
#   CREATING SYNTHETIC DATA   #
###############################

#Generating a fake sample of 100 people
sample <- seq(1, 100, by=1)

#Time 1 Groups
Merge_Co <- as.data.frame(sample[c(1:50)])
Merge_Co$Group_1 <- 'Merge Co.'
colnames(Merge_Co)[[1]] <- c('ID')

Standard_Co <- as.data.frame(sample[c(51:85)])
Standard_Co$Group_1 <- 'Standard Co.'
colnames(Standard_Co)[[1]] <- c('ID')

Luxury_Co <- as.data.frame(sample[c(86:100)])
Luxury_Co$Group_1 <- 'Luxury Co.'
colnames(Luxury_Co)[[1]] <- c('ID')

data <-  rbind.data.frame(Merge_Co, Standard_Co, Luxury_Co)

#Time 2 Groups
ss <- sample(1:5,size=nrow(data),replace=TRUE,prob=c(0.5, 0.2, 0.1, 0.1, 0.1))
p_1 <- data[ss==1,]
p_1$Group_2 <- 'Position 1'

p_2 <- data[ss==2,]
p_2$Group_2 <- 'Position 2'

p_3 <- data[ss==3,]
p_3$Group_2 <- 'Position 4'

p_4 <- data[ss==4,]
p_4$Group_2 <- 'Position 5'

p_5 <- data[ss==5,]
p_5$Group_2 <- 'Position 3'

#Stacking Time 1 and Time 2 Groups
data <- rbind(p_1, p_2, p_5, p_3, p_4)
data <- data[order(data$ID), ]

#Time 3 Groups
ss <- sample(1:15, size=nrow(data), replace=TRUE,prob=c(0.25, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05,
                                                        0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05))

#Generating Group Subsets
p_list <- vector('list', 15)
for (i in seq_along(p_list)){
  p_list[[i]] <- data[ss==i,]
}

#Adding Group ID variable
for (i in seq_along(p_list)){
  p_list[[i]]$Group_3 <- paste0('Position_', i)
}

#Stacking Time 1, Time 2, and Time 3 Groups
data <- rbind(p_list[[1]], p_list[[2]], p_list[[3]], p_list[[4]], p_list[[5]], 
              p_list[[6]], p_list[[7]], p_list[[8]], p_list[[9]], p_list[[10]], 
              p_list[[11]], p_list[[12]], p_list[[13]], p_list[[14]], p_list[[15]])

data <- data[order(data$ID), ]

rm(sample, Merge_Co, Standard_Co, Luxury_Co, p_1, p_2, p_3, p_4, p_5, p_list, i, ss)

###############################
#   CREATING AGGREGATE DATA   #
###############################

data_1 <- dplyr::group_by(data, Group_1) %>% 
  dplyr::count()
data_1$proportion_t1 <- data_1$n/100
data_1 <- data_1[order(-data_1$n), ]
data_1$t1_cusum <- cumsum(data_1$proportion_t1)/sum(data_1$proportion_t1)
data_1$group1_id <- as.numeric(row.names(data_1))
colnames(data_1)[[2]] <- c('t1_n')

group_2 <- group_by(data, Group_2) %>% 
  dplyr::count()
group_2$proportion <- group_2$n/100
group_2 <- group_2[order(-group_2$n), ]
group_2$t2_cusum <- cumsum(group_2$proportion)/sum(group_2$proportion)
group_2$group2_id <- as.numeric(rownames(group_2))
colnames(group_2)[[2]] <- c('t2_n')

data_2 <- group_by(data, Group_1, Group_2) %>% 
  dplyr::count()
data_2$proportion_t2 <- data_2$n/100
data_2 <- data_2[order(-data_2$n), ]

data_2 <- merge(data_2, group_2, by="Group_2",all.x=TRUE) 
data_2 <- data_2[order(-data_2$t2_cusum), ]
colnames(data_2)[[3]] <- c('t1_t2_n')

Time_1 <- merge(data_2, data_1, by="Group_1",all.x=TRUE) 
Time_1$path_id <- as.numeric(row.names(Time_1))
Time_1 <- Time_1[c(13, 12, 8, 1, 2, 9, 5, 3, 10, 4, 6, 11, 7)]
Time_1 <- Time_1[order(-Time_1$t1_cusum, -Time_1$t2_cusum), ]

#Creating y variables
Time_1$y_1 <- Time_1$t1_cusum-(Time_1$proportion*(Time_1$t1_cusum*Time_1$proportion_t1))
Time_1$y_4 <- Time_1$t2_cusum - (Time_1$proportion_t2*Time_1$t2_cusum)
Time_1$y_2 <- Time_1$y_1 - ((1-Time_1$proportion_t1) * Time_1$proportion_t2)
Time_1$y_3 <- ifelse(Time_1$y_1 >= Time_1$y_2, (Time_1$y_2/Time_1$y_1)*Time_1$y_4, (Time_1$y_1/Time_1$y_2)*Time_1$y_4)

#Creating y_0 and y_1 variables
g1_x_0 <- vector('list', nrow(data_1))
for (i in seq_along(g1_x_0)){
  x_0 <- Time_1 %>%
    dplyr::filter(group1_id == i)
  x_0 <- x_0[nrow(x_0), ]
  x_0 <- x_0[c(2, 4, 12, 16)]
  colnames(x_0)[[4]] <- c('y_1')
  x_0$y1_ref <- x_0$t1_cusum
  x_0$y2_ref <- x_0$y_1
  g1_x_0[[i]] <- x_0
  rm(x_0)
}  

g1_x_0 <- do.call(rbind.data.frame, g1_x_0)

g2_x_0 <- vector('list', nrow(group_2))
for (i in seq_along(g2_x_0)){
  x_0 <- Time_1 %>%
    dplyr::filter(group2_id == i)
  x_0 <- x_0[nrow(x_0), ]
  x_0 <- x_0[c(3, 5, 13, 17)]
  x_0$y1_ref <- x_0$t2_cusum
  x_0$y2_ref <- x_0$y_3
  g2_x_0[[i]] <- x_0
  rm(x_0)
}  

g2_x_0 <- do.call(rbind.data.frame, g2_x_0)

#CREATING TIME 2 DATASETS

group_3 <- group_by(data, Group_3) %>% 
  dplyr::count()
group_3$proportion <- group_3$n/100
group_3 <- group_3[order(-group_3$n), ]
group_3$t3_cusum <- cumsum(group_3$proportion)/sum(group_3$proportion)
group_3$group3_id <- as.numeric(rownames(group_3))
colnames(group_3)[[2]] <- c('t3_n')

data_3 <- group_by(data, Group_2, Group_3) %>% 
  dplyr::count()
data_3$proportion_t3 <- data_3$n/100
data_3 <- data_3[order(-data_3$n), ]

data_3 <- merge(data_3, group_3, by="Group_3",all.x=TRUE) 
data_3 <- data_3[order(-data_3$t3_cusum), ]
colnames(data_3)[[3]] <- c('t2_t3_n')

colnames(group_2)[[3]] <- c('t2_proportion')
Time_2 <- merge(data_3, group_2, by="Group_2",all.x=TRUE) 
Time_2$path_id <- as.numeric(row.names(Time_2))
Time_2 <- Time_2[c(13, 12, 8, 1, 2, 9, 5, 3, 10, 4, 6, 11, 7)]
Time_2 <- Time_2[order(-Time_2$t2_cusum, -Time_2$t3_cusum), ]

#Creating y variables
Time_2$y_1 <- Time_2$t2_cusum-(Time_2$proportion*(Time_2$t2_cusum*Time_2$t2_proportion))
Time_2$y_4 <- Time_2$t3_cusum - (Time_2$proportion_t3*Time_2$t3_cusum)
Time_2$y_2 <- Time_2$y_1 - ((1-Time_2$t2_proportion) * Time_2$proportion_t3)
Time_2$y_3 <- ifelse(Time_2$y_1 >= Time_2$y_2, (Time_2$y_2/Time_2$y_1)*Time_2$y_4, (Time_2$y_1/Time_2$y_2)*Time_2$y_4)

#Create Company IDs for Interval 2
group1_id <- group_by(data, Group_1, Group_2, Group_3) %>% 
  dplyr::count()
g2_g3 <- Time_2[c(1, 4:5, 14, 16, 17, 15)]
group1_id <- merge(g2_g3, group1_id, by=c("Group_2","Group_3"), all.x=TRUE, all.y=FALSE)
group1_id <- merge(group1_id, data_1, by=c("Group_1"), all.x=TRUE)
g1_g2_g3 <- group1_id
group1_id <- group1_id[order(group1_id$path_id, -group1_id$group1_id), ]
group1_id <- group1_id[c(4, 13)]

Path_ID <- seq(1, nrow(group1_id), by=1)
group1_id$Path_ID <- Path_ID

#Creating y_1 and y_2 variables
t_3 <- Time_1[c(1,3)]
t_3 <- merge(t_3, g2_x_0, by="group2_id",all.x=TRUE) 
t_3$x <- 1
t_3 <- t_3[c(2, 1, 3, 8, 4, 5, 6, 7)]
colnames(t_3)[1:6] <-  c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

g3_x_0  <- group_by(data, Group_1, Group_2, Group_3) %>% 
  dplyr::count()
g1_g2 <- Time_1[c(1, 4:5)]
colnames(t_3)[[1]] <- c('path_id')
t_3 <- merge(g1_g2, t_3, by=c("path_id"), all.x=TRUE)
t_3 <- t_3[c(2:4, 6:10)]

g2_g3 <- Time_2[c(1, 4:5)]
g3_x_0  <- merge(g2_g3, g3_x_0 , by=c("Group_2","Group_3"), all.x=TRUE, all.y=FALSE)
g3_x_0   <- merge(g3_x_0 , t_3, by=c("Group_1","Group_2"), all.x=TRUE, all.y=FALSE)
g3_x_0 <-  g3_x_0[order(g3_x_0$path_id), ]
g3_x_0$Path_ID <- Path_ID

rm(g1_g2, g2_g3)

g4_x_0 <- vector('list', nrow(group_3))
for (i in seq_along(g4_x_0)){
  x_0 <- Time_2 %>%
    dplyr::filter(group3_id == i)
  x_0 <- x_0[nrow(x_0), ]
  x_0 <- x_0[c(3, 5, 13, 17)]
  x_0$y1_ref <- x_0$t3_cusum
  x_0$y2_ref <- x_0$y_3
  g4_x_0[[i]] <- x_0
  rm(x_0)
}  

g4_x_0 <- do.call(rbind.data.frame, g4_x_0)

#Creating Long Data Set for Interval 2
t_0 <- g3_x_0[c(12, 6, 3, 7, 8:11)]
colnames(t_0)[1:6] <-  c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

t_1 <- Time_2[c(1:2, 4)]
t_1 <-  merge(t_1, g1_g2_g3, by=c("path_id"), all.x=TRUE)
t_1$x <- 1.15
t_1$Path_ID <- Path_ID
t_1 <- t_1[c(17, 1, 11, 2, 15, 3, 16, 7, 8)]

t_1_y <- vector('list', length(unique(t_1$path_id)))
for (i in seq_along(t_1_y)){
  path <- t_1 %>%
    dplyr::filter(path_id == i)
  path$y1_ref <- path$y_1
  path$y2_ref <- path$y_2
  
  path$y_diff <- path$y_1 - path$y_2
  path$group_prop <- path$n/sum(path$n)
  path$cusmsum <- cumsum(path$group_prop)/sum(path$group_prop)
  path <- path[order(-path$cusmsum), ]
  path$obs_id <- as.numeric(row.names(path))
  
  path$y_1 <- ifelse(path$cusmsum == 1, path$y_1, path$y_2 + path$y_diff*path$cusmsum)
  path$y_2 <- ifelse(path$obs_id == 1, path$y2, path$y_1 - path$y_diff*path$group_prop)
  
  path <- path[order(-path$y_1), ]
  path <- path[-c(15)]
  
  t_1_y[[i]] <- path
  rm(path)
}

t_1 <- do.call(rbind.data.frame, t_1_y)
rm(t_1_y)

t_1 <- t_1[order(-t_1$y_1, -t_1$y_2), ]
t_1 <- t_1[c(1, 4, 6:11)]
colnames(t_1)[1:6] <- c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

t_2 <- Time_2[c(1,3, 5)]
t_2 <-  merge(t_2, g1_g2_g3, by=c("path_id"), all.x=TRUE)
t_2$x <- 1.70
t_2$Path_ID <- Path_ID
t_2 <- t_2[c(17, 1, 11, 2, 3, 16, 10, 9)]

t_2_y <- vector('list', length(unique(t_2$path_id)))
for (i in seq_along(t_2_y)){
  path <- t_2 %>%
    dplyr::filter(path_id == i)
  path$y1_ref <- path$y_4
  path$y2_ref <- path$y_3
  
  path$y_diff <- path$y_4 - path$y_3
  path$group_prop <- path$n/sum(path$n)
  path$cusmsum <- cumsum(path$group_prop)/sum(path$group_prop)
  path$obs_id <- as.numeric(row.names(path))
  
  path$y_1 <- ifelse(path$cusmsum == 1, path$y_4, path$y_3 + path$y_diff*path$cusmsum)
  path$y_2 <- ifelse(path$obs_id == 1, path$y_3, path$y_1 - path$y_diff*path$group_prop) 
  
  path <- path[order(-path$y_1), ]
  path <- path[-c(14)]
  
  t_2_y[[i]] <- path
  rm(path)
}

t_2 <- do.call(rbind.data.frame, t_2_y)
rm(t_2_y)

t_2 <- t_2[order(-t_2$y_1,-t_2$y_2), ]
t_2 <- t_2[c(1, 4:6, 14:15, 9:10)]
colnames(t_2)[1:6] <- c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

t_3 <- Time_2[c(1,3)]
t_3 <- merge(t_3, g4_x_0, by="group3_id",all.x=TRUE) 
t_3 <-  merge(t_3, g1_g2_g3, by=c("path_id"), all.x=TRUE)
t_3$x <- 2.05
t_3$Path_ID <- Path_ID
t_3 <- t_3[c(21,1, 19,2, 17, 3, 18, 20, 4, 5)]
t_3 <- t_3[order(-t_3$path_id, t_3$group1_id), ]
t_3$y_diff <- t_3$t3_cusum - t_3$y_3.x

t_3_y <- vector('list', length(unique(t_3$group3_id)))
for (i in seq_along(t_3_y)){
  loc <- t_3 %>%
    dplyr::filter(group3_id == i)
  
  prop <- dplyr::group_by(loc, group1_id, group3_id) %>% 
    dplyr::count()
  prop$prop <- prop$n/sum(prop$n)
  prop <- prop[c(1,4)]
  
  loc <- merge(loc, prop, by=c('group1_id'),all.x=TRUE) 
  loc$cusmsum <- cumsum(loc$prop)/sum(loc$prop)
  rm(prop)
  
  loc$obs_id <- as.numeric(rownames(loc))
  loc$y1_ref <- loc$t3_cusum
  loc$y2_ref <- loc$y_3.x
  
  loc$y_1 <- ifelse(loc$cusmsum == 1, loc$t3_cusum, loc$y_3.x + loc$y_diff*loc$cusmsum)
  loc$y_2 <- ifelse(loc$obs_id == 1, loc$y_3.x, loc$y_1 - loc$y_diff*loc$prop)
  loc <- loc[-c(1, 13, 12)]
  
  t_3_y[[i]] <- loc
  rm(loc)
}

t_3 <- do.call(rbind.data.frame, t_3_y)
rm(t_3_y)

t_3 <- t_3[c(1, 3, 5, 7, 14:15, 12:13)]
colnames(t_3)[1:6] <-  c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

Interval_2 <- rbind.data.frame(t_0, t_1, t_2, t_3)
Interval_2$y_porportion <- Interval_2$y_2/Interval_2$y_1

rm(t_0, t_1, t_2, t_3, g3_x_0, g4_x_0, data_2, data_3)

#Creating Time Datasets for Combined Intervals: 1 and 2
key <- Time_1[c(4:5, 14, 16, 15, 17)]
colnames(key)[3:6] <- c('i1_y1', 'i1_y2', 'i1_y4', 'i1_y3') 
key <-  merge(g1_g2_g3, key, by=c('Group_1', 'Group_2'),all.x=TRUE) 
key <- key[order(key$path_id, -key$group1_id), ]
key <- merge(key, group1_id, by=c('path_id', 'group1_id'),all.x=TRUE) 

#Creating Long Data Set for Interval 1
t_0 <- key[c(18, 2)]
t_0 <- merge(t_0, g1_x_0, by="group1_id",all.x=TRUE) 
t_0$x <- 0
t_0 <- t_0[c(2, 1, 3, 8, 4:7)]
colnames(t_0)[1:6] <-  c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

t_1 <- key[c(18, 2:3)]
t_1$x <- 0.25
t_1$y_1 <- key$i1_y1
t_1$y_2 <- key$i1_y2
t_1 <- t_1[order(-t_1$y_1), ]
colnames(t_1) <- c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')
t_1$y1_ref <- t_1$y_1
t_1$y2_ref  <- t_1$y_2

t_2 <- key[c(18, 1, 4)]
t_2$x <- 0.75
t_2$y_1 <- key$i1_y4
t_2$y_2 <- key$i1_y3
t_2 <- t_2[order(-t_2$y_1), ]
colnames(t_2) <- c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')
t_2$y1_ref <- t_2$y_1
t_2$y2_ref  <- t_2$y_2

t_3 <- key[c(18, 4)]
t_3 <- merge(t_3, g2_x_0, by="Group_2",all.x=TRUE) 
t_3$x <- 0.95
t_3 <- t_3[c(2, 3, 1, 8, 4, 5, 6:7)]
colnames(t_3)[1:6] <-  c('Path_ID','ID', 'Group', 'x', 'y_1', 'y_2')

Interval_1 <- rbind.data.frame(t_0, t_1, t_2, t_3)
Interval_1$y_porportion <- Interval_1$y_2/Interval_1$y_1

rm(t_0, t_1, t_2, t_3, key, g1_g2_g3, g1_x_0, g2_x_0)

Interval <- rbind.data.frame(Interval_1, Interval_2)

#####################
#   VISUALIZATION   #
#####################

colors_index <- group1_id

curve_list <- vector('list', length(Path_ID))
for (i in seq_along(Path_ID)){
  path <- Interval %>%
    dplyr::filter(Path_ID == i)
  
  xy_1 <- path[c(4, 5)]
  xy_2 <- path[c(4, 6)]
  xy_3 <- path[c(4, 7)]
  xy_4 <- path[c(4, 8)]
  
  #Spline Fitting
  plot(0)
  val.curve_1 <- xspline(xy_1, shape = -0.5, draw = FALSE)
  val.curve_2 <- xspline(xy_2, shape = -0.5, draw = FALSE)
  val.curve_3 <- xspline(xy_3, shape = -0.5, draw = FALSE)
  val.curve_4 <- xspline(xy_4, shape = -0.5, draw = FALSE)
  val.curve_1 <- data.frame(val.curve_1)
  val.curve_2 <- data.frame(val.curve_2)
  val.curve_3 <- data.frame(val.curve_3)
  val.curve_4 <- data.frame(val.curve_4)
  
  junk <- dev.off(which = dev.cur())
  
  val.diff <- abs(nrow( val.curve_4)- nrow( val.curve_1))
  if (nrow(val.curve_4) > nrow(val.curve_1)) {
    val.curve_4 <- val.curve_4[-seq((nrow(val.curve_4)-1), nrow(val.curve_4)-val.diff),]
  } else if (nrow(val.curve_4) < nrow(val.curve_1)) {
    val.curve_1 <- val.curve_1[-seq((nrow(val.curve_1)-1), nrow(val.curve_1)-val.diff),]
  } else {
    val.curve_4
  }
  
  val.diff <- abs(nrow( val.curve_2)- nrow( val.curve_1))
    if (nrow(val.curve_2) > nrow(val.curve_1)) {
      val.curve_2 <- val.curve_2[-seq((nrow(val.curve_2)-1), nrow(val.curve_2)-val.diff),]
    } else if (nrow(val.curve_2) < nrow(val.curve_1)) {
      val.curve_1 <- val.curve_1[-seq((nrow(val.curve_1)-1), nrow(val.curve_1)-val.diff),]
    } else {
      val.curve_2
    }
  
  val.diff <- abs(nrow( val.curve_3)- nrow( val.curve_1))
    if (nrow(val.curve_3) > nrow(val.curve_1)) {
      val.curve_3 <- val.curve_3[-seq((nrow(val.curve_3)-1),nrow(val.curve_3)-val.diff),]
    } else if (nrow(val.curve_3) < nrow(val.curve_1)) {
      val.curve_1 <- val.curve_1[-seq((nrow(val.curve_1)-1),nrow(val.curve_3)-val.diff),]
      val.curve_2 <- val.curve_2[-seq((nrow(val.curve_2)-1),nrow(val.curve_3)-val.diff),]
    } else {
      val.curve_3
    }
  
  val.diff <- abs(nrow( val.curve_4)- nrow( val.curve_1))
    if (nrow(val.curve_4) > nrow(val.curve_1)) {
      val.curve_4 <- val.curve_4[-seq((nrow(val.curve_4)-1),nrow(val.curve_4)-val.diff),]
    } else if (nrow(val.curve_4) < nrow(val.curve_1)) {
      val.curve_1 <- val.curve_1[-seq((nrow(val.curve_1)-1),nrow(val.curve_4)-val.diff),]
      val.curve_2 <- val.curve_2[-seq((nrow(val.curve_2)-1),nrow(val.curve_4)-val.diff),]
      val.curve_3 <- val.curve_3[-seq((nrow(val.curve_3)-1),nrow(val.curve_4)-val.diff),]
    } else {
      val.curve_4
    }
  
  curve_list[[i]] <- val.curve_1
  curve_list[[i]]$y_2 <- val.curve_2$y
  curve_list[[i]]$y1_ref <- val.curve_3$y
  curve_list[[i]]$y2_ref <- val.curve_4$y
  colnames(curve_list[[i]])[2] <- c('y_1')
  
  colors <- c(0, 0, 0, 0)
  if (colors_index$group1_id[[i]] == 1) { 
    colors <- c(0, 0, 1, 0.7)
  } else if (colors_index$group1_id[[i]] == 2) {
    colors <- c(0.4, 0, 0.8, 0.5)
  } else {
    colors <- c(1, 0, 0, 0.3) 
  }
  
  curve_list[[i]]$Path_ID <- i
  curve_list[[i]]$red <- colors[[1]]
  curve_list[[i]]$green <- colors[[2]]
  curve_list[[i]]$blue <- colors[[3]]
  curve_list[[i]]$alpha <- colors[[4]]
  
  rm(path, xy_1, xy_2, xy_3, xy_4, val.curve_1, val.curve_2, val.curve_3, val.curve_4, val.diff, junk, colors)
}

rm(colors_index)

#Plotting
png("p_1.png", width=1121, height = 566)
  par(mar = c(3, 9, 3, 3.5), family='serif')  
  plot(0, type='n', xlim=c(0, 2.5), ylim=c(0, 1.1), xlab=' ', ylab=' ', cex.lab=1.5, family='serif', 
       axes=FALSE, bty='n')

  #Adding Gridlines
  grid(lwd = 2)

  #Adding Axis and Margin Text
  mtext(side=1, c('Time 1', 'Time 2'), at=c(1, 2), line=0.1, cex=1.5)
  mtext(side=3, c('Positions', 'Positions'), at=c(1, 2), line=0.1, cex=1.5)
  mtext(side=2, c('Merge Co.', 'Standard Co.', 'Luxury Co.'), at=c(0.5, 0.85, 1), line=0.1, 
        cex=1.5, las=1)

  #Adding Reference Line
  abline(v=0.95, lty=1, col="black")
  abline(v=1.05, lty=1, col="black")
  abline(v=1.95, lty=1, col="black")
  abline(v=2.05, lty=1, col="black")

  #Adding Cumulative Percentage Axis Labels
  y_axis <- seq(0, 1, by=0.2)
  axis(4, y_axis, cex.axis = 1, family = 'serif')
  mtext(side=4, c('Cumulative Proportion'), at=0.5, line=2, cex=1.3)
  
  #Adding Reference Flows
  for (i in seq_along(curve_list)){
    polygon(c(curve_list[[i]]$x,rev(curve_list[[i]]$x)), 
            c(curve_list[[i]]$y1_ref,rev(curve_list[[i]]$y2_ref)) ,
            col='gray94', border = NA)
  }
  
  #Adding Flows: 
  for (i in seq_along(curve_list)){
    polygon(c(curve_list[[i]]$x,rev(curve_list[[i]]$x)), 
            c(curve_list[[i]]$y_1,rev(curve_list[[i]]$y_2)) ,
            col=rgb(curve_list[[i]]$red, curve_list[[i]]$green, 
                    curve_list[[i]]$blue, curve_list[[i]]$alpha), border = NA)
  }
  
  #Adding Position Labels (Extracting Numeric Parts of Position Labels)
  t1_cusum <- data_1$t1_cusum
  t2_cusum <- group_2$t2_cusum
  t2_id <-   as.numeric(gsub("Position ", "", group_2$Group_2))
  t3_cusum <- group_3$t3_cusum
  t3_id <- as.numeric(gsub("Position_", "", group_3$Group_3))

  for (i in seq_along(data_1$t1_cusum)){
    lines(x=c(-0.05, 0.05), y=c(t1_cusum[[i]], t1_cusum[[i]]), col = 'black', lwd=1.5)
  }

  for (i in seq_along(t2_cusum)){
    points(x=1, y=t2_cusum[[i]], pch=16, col='grey', cex=6)
  }
  for (i in seq_along(t2_cusum)){
    text(x=1, y=t2_cusum[[i]], t2_id[[i]], col = 'black', cex=1.5)
  }

  for (i in seq_along(t3_cusum)){
    points(x=2, y=t3_cusum[[i]], pch=16, col='grey', cex=6)
  }
  for (i in seq_along(t3_cusum)){
    text(x=2, y=t3_cusum[[i]], t3_id[[i]], col = 'black', cex=0.90)
  }

  rm(t1_cusum, t2_cusum, t2_id, t3_cusum, t3_id, y_axis)
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("Stylized Sankey_9Jan2019.pdf", dpi=600)
