#Jonathan H. Morgan
#ACT: Cosine Similarity Analyses
#2 August 2019

#Clearing Old Data
rm(list = ls())
gc()

#Home Directory
setwd('/Users/jhmorgan/Desktop/ACT_Intersectionality/ACT_Cross Culture Analyses/')
getwd()

#Setting Output directory for writing CSVs
DATA.DIR <- ("/Users/jhmorgan/Desktop/ACT_Intersectionality/ACT_Cross Culture Analyses/Cosine Analyses")

################
#   PACKAGES   #
################
library(readr)        #Import csv and other delimited files
library(ggplot2)      #Visualization Package

source('/Users/jhmorgan/Desktop/ACT_Intersectionality/ACT_Cross Culture Analyses/ACT_Cosine Function_2Aug2019.R')

##########################
#   FULL INTERSECTIONS   #
##########################

setwd('./ACT Final Sets')
getwd()

#Reading in Egypt and Morrocco
Egypt_Base <- read_csv('./Egypt/Egypt.csv', col_names = TRUE)
Morocco_Base <- read_csv('./Morocco/Morocco.csv', col_names = TRUE)
UGA_Base <- read_csv('./FullSurveyor/FullSurveyor.csv', col_names = TRUE)

#Resetting the Path to Cosine Analyses Folder
setwd('../Cosine Analyses')
getwd()

names <- c('UGA', 'Egypt', 'Morocco')
Base_Data <- vector('list', 3)
  Base_Data[[1]] <- UGA_Base
  Base_Data[[2]] <- Egypt_Base
  Base_Data[[3]] <- Morocco_Base
names(Base_Data) <- names

#Dropping Socio-Demographic and Other Variables all surveyor data set
Base_Data[[1]] <- Base_Data[[1]][c(1:2, 25:29)]

rm(UGA_Base, Egypt_Base, Morocco_Base)

#Creating Identities, Beahviors, and modifiers Lists and Subsetting
Identities <- lapply(Base_Data, function(x) x[grep("i_", x$term_ID), ])
Behaviors <- lapply(Base_Data, function(x) x[grep("b_", x$term_ID), ])
Modifiers <- lapply(Base_Data, function(x) x[grep("m_", x$term_ID), ])

#Combining Lists to Generate a List of Identities, Behaviors, and Modifiers
names <- c('UGA', 'Egypt', 'Morocco')
EPA <- vector('list', 3)
  EPA[[1]] <- as.data.frame(rbind(Identities[[1]], Behaviors[[1]], Modifiers[[1]]))
  EPA[[2]] <- as.data.frame(rbind(Identities[[2]], Behaviors[[2]], Modifiers[[2]]))
  EPA[[3]] <- as.data.frame(rbind(Identities[[3]], Behaviors[[3]], Modifiers[[3]]))
names(EPA) <- names

rm(Identities, Behaviors, Modifiers, names)

#Subsetting Lists for Convenience
epa_names <- c('Term ID', 'E', 'P', 'A')
EPA <-  lapply(EPA, function(x) as.data.frame(cbind(x$term_ID, x$E, x$P, x$A), stringsAsFactors = FALSE))

for (i in seq_along(EPA)){
  colnames(EPA[[i]]) <- epa_names
  EPA[[i]][[2]] <- as.numeric(EPA[[i]][[2]])
  EPA[[i]][[3]] <- as.numeric(EPA[[i]][[3]])
  EPA[[i]][[4]] <- as.numeric(EPA[[i]][[4]])
}

MTurk_Base <- read_csv('./AllMTurk_Compare.csv', col_names = TRUE)
colnames(MTurk_Base)[[1]] <- 'Term ID'

UGA_Norm <- read_csv('./AllSurveyor_Compare.csv', col_names = TRUE)
colnames(UGA_Norm)[[1]] <- 'Term ID'

#Identifying the Unique Terms List to Create a list of Intersections
UGA_Terms <- unique(EPA[[1]]$`Term ID`)
MTurk_Terms <- unique(MTurk_Base$`Term ID`)
Egypt_Terms <- unique(EPA[[2]]$`Term ID`)
Morocco_Terms <- unique(EPA[[3]]$`Term ID`)

#Creating the Intersections and List
US_Intersect <- intersect(UGA_Terms, MTurk_Terms)
`US/Egypt Intersect` <- intersect(UGA_Terms, Egypt_Terms)
`US/Morocco Intersect` <- intersect(UGA_Terms, Morocco_Terms)
`Egypt/Morocco Intersect` <- intersect(Egypt_Terms, Morocco_Terms)

intersection_names <- c('U.S. Intersect', 'US/Egypt Intersect', 'US/Morocco Intersect', 'Egypt/Morocco Intersect')
Intersections <- vector('list', 4)
Intersections[[1]] <- US_Intersect
Intersections[[2]] <- `US/Egypt Intersect`
Intersections[[3]] <- `US/Morocco Intersect`
Intersections[[4]] <- `Egypt/Morocco Intersect`
names(Intersections) <- intersection_names

rm(`US/Egypt Intersect`, `US/Morocco Intersect`, `Egypt/Morocco Intersect`, UGA_Terms, MTurk_Terms, Egypt_Terms, Morocco_Terms, Base_Data)


Pairs <- vector('list', 4)
for (i in seq_along(Pairs)){
  Pairs[[i]] <- vector('list', 2)
}
names(Pairs) <- intersection_names

for (i in seq_along(Pairs)){
  for (j in seq_along(Pairs[[i]])){
    names(Pairs[[i]])[[j]] <- paste0("Dictionary_", j)
  }
}

#US Pairs
Pairs$`U.S. Intersect`$Dictionary_1 <- UGA_Norm[UGA_Norm$`Term ID` %in% Intersections$`U.S. Intersect`, ]
Pairs$`U.S. Intersect`$Dictionary_2 <- MTurk_Base[MTurk_Base$`Term ID` %in% Intersections$`U.S. Intersect`, ]

Pairs$`U.S. Intersect`$Dictionary_1 <- Pairs$`U.S. Intersect`$Dictionary_1[order(Pairs$`U.S. Intersect`$Dictionary_1[[1]]), ]
Pairs$`U.S. Intersect`$Dictionary_2 <- Pairs$`U.S. Intersect`$Dictionary_2[order(Pairs$`U.S. Intersect`$Dictionary_2[[1]]), ]

#US/Egypt
Pairs$`US/Egypt Intersect`$Dictionary_1 <- EPA$UGA[EPA$UGA$`Term ID` %in% Intersections$`US/Egypt Intersect`, ]
Pairs$`US/Egypt Intersect`$Dictionary_2 <- EPA$Egypt[EPA$Egypt$`Term ID` %in% Intersections$`US/Egypt Intersect`, ]

Pairs$`US/Egypt Intersect`$Dictionary_1 <- Pairs$`US/Egypt Intersect`$Dictionary_1[order(Pairs$`US/Egypt Intersect`$Dictionary_1[[1]]), ]
Pairs$`US/Egypt Intersect`$Dictionary_2 <- Pairs$`US/Egypt Intersect`$Dictionary_2[order(Pairs$`US/Egypt Intersect`$Dictionary_2[[1]]), ]

#US/Morocco 
Pairs$`US/Morocco Intersect`$Dictionary_1 <- EPA$UGA[EPA$UGA$`Term ID` %in% Intersections$`US/Morocco Intersect`, ]
Pairs$`US/Morocco Intersect`$Dictionary_2 <- EPA$Morocco[EPA$Morocco$`Term ID` %in% Intersections$`US/Morocco Intersect`, ]

Pairs$`US/Morocco Intersect`$Dictionary_1 <- Pairs$`US/Morocco Intersect`$Dictionary_1[order(Pairs$`US/Morocco Intersect`$Dictionary_1[[1]]), ]
Pairs$`US/Morocco Intersect`$Dictionary_2 <- Pairs$`US/Morocco Intersect`$Dictionary_2[order(Pairs$`US/Morocco Intersect`$Dictionary_2[[1]]), ]

#Egypt/Morocco
Pairs$`Egypt/Morocco Intersect`$Dictionary_1 <- EPA$Egypt[EPA$Egypt$`Term ID` %in% Intersections$`Egypt/Morocco Intersect`, ]
Pairs$`Egypt/Morocco Intersect`$Dictionary_2 <-  EPA$Morocco[EPA$Morocco$`Term ID` %in% Intersections$`Egypt/Morocco Intersect`, ]

Pairs$`Egypt/Morocco Intersect`$Dictionary_1 <- Pairs$`Egypt/Morocco Intersect`$Dictionary_1[order(Pairs$`Egypt/Morocco Intersect`$Dictionary_1[[1]]), ]
Pairs$`Egypt/Morocco Intersect`$Dictionary_2 <- Pairs$`Egypt/Morocco Intersect`$Dictionary_2[order(Pairs$`Egypt/Morocco Intersect`$Dictionary_2[[1]]), ]

#Making the table just a bit more interpretable and saving
us_names <- c('UGA', 'MTurk')
ue_names <- c('UGA', 'Egypt')
um_names <- c('UGA', 'Morocco')
em_names <- c('Egypt', 'Morocco')

names(Pairs$`U.S. Intersect`) <- us_names
names(Pairs$`US/Egypt Intersect`) <- ue_names
names(Pairs$`US/Morocco Intersect`) <- um_names
names(Pairs$`Egypt/Morocco Intersect`) <- em_names

rm(EPA, Intersections, epa_names, us_names, ue_names, um_names, em_names, intersection_names, i, j)

#Getting rid of NA values (not the greatest way to do this, but ce' la vie)
Pairs$`U.S. Intersect`$UGA <- na.omit(Pairs$`U.S. Intersect`$UGA)
Pairs$`U.S. Intersect`$MTurk <- na.omit(Pairs$`U.S. Intersect`$MTurk)

Pairs$`US/Egypt Intersect`$UGA <- na.omit(Pairs$`US/Egypt Intersect`$UGA)
Pairs$`US/Egypt Intersect`$Egypt <- na.omit(Pairs$`US/Egypt Intersect`$Egypt)

Pairs$`US/Morocco Intersect`$UGA <- na.omit(Pairs$`US/Morocco Intersect`$UGA)
Pairs$`US/Morocco Intersect`$Morocco <- na.omit(Pairs$`US/Morocco Intersect`$Morocco)

Pairs$`Egypt/Morocco Intersect`$Egypt <- na.omit(Pairs$`Egypt/Morocco Intersect`$Egypt)
Pairs$`Egypt/Morocco Intersect`$Morocco <- na.omit(Pairs$`Egypt/Morocco Intersect`$Morocco)

rm(MTurk_Base, UGA_Norm)

#Saving Pairs File
#save(Pairs, file='./M_Culture_Pairs.Rdata')

########################
#   APPLES-TO-APPLES   #
########################
#In this analysis, we compare only concepts shared by all four data sets.
setwd('./Apples-to-Apples')
getwd()

#Creating Pairs Data Set 
Pairs_2 <- vector('list', 4)
intersection_names <- c('US_Intersect', 'US/Egypt Intersect', 'US/Morocco Intersect', 'Egypt/Morocco Intersect')

#Creating 
for (i in seq_along(Pairs_2)){
  Pairs_2[[i]] <- vector('list', 2)
}
names(Pairs_2) <- intersection_names

#Making the table just a bit more interpretable and saving
us_names <- c('UGA', 'MTurk')
ue_names <- c('UGA', 'Egypt')
um_names <- c('UGA', 'Morocco')
em_names <- c('Egypt', 'Morocco')

names(Pairs_2$US_Intersect) <- us_names
names(Pairs_2$`US/Egypt Intersect`) <- ue_names
names(Pairs_2$`US/Morocco Intersect`) <- um_names
names(Pairs_2$`Egypt/Morocco Intersect`) <- em_names

rm(intersection_names, em_names, ue_names, um_names, us_names)

#Reading in US_Intersect
Pairs_2[[1]]$UGA <- read_csv('./SurveyorMutualNorm.csv', col_names = TRUE)
Pairs_2[[1]]$MTurk <- read_csv('./MTurkMutualNorm.csv', col_names = TRUE)

#Reading in US/Egypt Intersect
Pairs_2[[2]]$UGA <- read_csv('./USSurveyorRatings.csv', col_names = TRUE)
Pairs_2[[2]]$Egypt <- read_csv('./EgyptRatings.csv', col_names = TRUE)

#Reading in US/Morocco Intersect
Pairs_2[[3]]$UGA <- read_csv('./USSurveyorRatings.csv', col_names = TRUE)
Pairs_2[[3]]$Morocco <- read_csv('./MoroccoRatings.csv', col_names = TRUE)

#Reading in US/Egypt Intersect
Pairs_2[[4]]$Egypt <- read_csv('./EgyptRatings.csv', col_names = TRUE)
Pairs_2[[4]]$Morocco <- read_csv('./MoroccoRatings.csv', col_names = TRUE)

#Getting rid of NA values (not the greatest way to do this, but ce' la vie)
Pairs_2$US_Intersect$UGA <- na.omit(Pairs_2$US_Intersect$UGA)
Pairs_2$US_Intersect$MTurk <- na.omit(Pairs_2$US_Intersect$MTurk)

Pairs_2$`US/Egypt Intersect`$UGA <- na.omit(Pairs_2$`US/Egypt Intersect`$UGA)
Pairs_2$`US/Egypt Intersect`$Egypt <- na.omit(Pairs_2$`US/Egypt Intersect`$Egypt)

Pairs_2$`US/Morocco Intersect`$UGA <- na.omit(Pairs_2$`US/Morocco Intersect`$UGA)
Pairs_2$`US/Morocco Intersect`$Morocco <- na.omit(Pairs_2$`US/Morocco Intersect`$Morocco)

Pairs_2$`Egypt/Morocco Intersect`$Egypt <- na.omit(Pairs_2$`Egypt/Morocco Intersect`$Egypt)
Pairs_2$`Egypt/Morocco Intersect`$Morocco <- na.omit(Pairs_2$`Egypt/Morocco Intersect`$Morocco)

#Formatting term_ID to be Term ID
for (i in seq_along(Pairs_2)){
  for (j in seq_along(Pairs_2[[i]]) ){
  colnames(Pairs_2[[i]][[j]])[[1]] <- 'Term ID'
  }
}

#Saving Pairs File
#save(Pairs_2, file='./Culture_Pairs_2.Rdata')

###########################################
#   PAIRWISE COSINE SIMILARITY ANALYSES   #
###########################################

#COSINE SIMILARITY FUNCTION ARGUMENTS

#Loading Input Data
load('Culture_Pairs_2.Rdata')

#Creating Scale Variables: Necessary because not all EPA dictionaries are scale from -4.3 to 4.3
scale_min <- vector('list', length(Pairs))
for (i in seq_along(Pairs)){
  scale_min[[i]] <-  min(Pairs[[i]][[1]]$A)
}

scale_max <- vector('list', length(Pairs)) 
for (i in seq_along(Pairs)){
  scale_max[[i]] <-  max(Pairs[[i]][[1]]$A)
}

#Creating a lists of titles to iterate through when generating the plots, a list for the distribution plots and one for the countour plots
titles <- vector('list', 4)
  titles[[1]] <- c('U.S. Samples Comparison')
  titles[[2]] <- c('U.S. vs. Egypt Sample Comparison')
  titles[[3]] <- c('U.S. vs. Morocco Sample Comparison')
  titles[[4]] <- c('Egypt vs. Morocco Sample Comparison')

contour_titles <- vector('list', length(titles))
  contour_titles[[1]] <- c('U.S. Samples Comparison: Dimensions')
  contour_titles[[2]] <- c('U.S. vs. Egypt Sample Comparison: Dimensions')
  contour_titles[[3]] <- c('U.S. vs. Morocco Sample Comparison: Dimensions')
  contour_titles[[4]] <- c('Egypt vs. Morocco Sample Comparison: Dimensions')

barplot_lables <- vector('list', length(titles))
  barplot_lables[[1]] <- c('U.S. Comparison')
  barplot_lables[[2]] <- c('U.S. vs. Egypt')
  barplot_lables[[3]] <- c('U.S. vs. Morocco')
  barplot_lables[[4]] <- c('Egypt vs. Morocco')
  
data <- Pairs

#Running act_cosine function
act_cosine(Pairs, scale_min, scale_max, titles, contour_titles, barplot_lables)

#########################
#   REVIEWING RESULTS   #
#########################

rm(barplot_lables, contour_titles, titles, scale_max, scale_min)

#Looking at the visuals
`U.S. Samples Comparison`
`U.S. vs. Egypt Sample Comparison`
`U.S. vs. Morocco Sample Comparison`
`Egypt vs. Morocco Sample Comparison`

`U.S. Samples Comparison: Dimensions`
`U.S. vs. Egypt Sample Comparison: Dimensions`
`U.S. vs. Morocco Sample Comparison: Dimensions`
`Egypt vs. Morocco Sample Comparison: Dimensions`

`Culture Comparison`

#Exporting Plots and Data Sets
ggsave(`U.S. Samples Comparison`,file='U.S. Samples Comparison.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`U.S. vs. Egypt Sample Comparison`,file='U.S. vs. Egypt Sample Comparison.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`U.S. vs. Morocco Sample Comparison`,file='U.S. vs. Morocco Sample Comparison.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`Egypt vs. Morocco Sample Comparison`,file='Egypt vs. Morocco Sample Comparison.png', width = 20, height = 20, units = "cm", dpi=600)

ggsave(`U.S. Samples Comparison: Dimensions`,file='U.S. Samples Comparison: Dimensions.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`U.S. vs. Egypt Sample Comparison: Dimensions`,file='U.S. vs. Egypt Sample Comparison: Dimensions.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`U.S. vs. Morocco Sample Comparison: Dimensions`,file='U.S. vs. Morocco Sample Comparison: Dimensions.png', width = 20, height = 20, units = "cm", dpi=600)
ggsave(`Egypt vs. Morocco Sample Comparison: Dimensions`,file='Egypt vs. Morocco Sample Comparison: Dimensions.png', width = 20, height = 20, units = "cm", dpi=600)

ggsave(`Culture Comparison`,file='Culture Comparison.png', width = 20, height = 20, units = "cm", dpi=600)

#Exporting List Components 
DATA.DIR <- ("/Users/jhmorgan/Desktop")

#Pairs Data and Cannonical Data

#U.S.
UGA_Label <- names(Pairs[[1]])[[1]]
MTurk_Label <-  names(Pairs[[1]])[[2]]

#Pairs
Pairs[[1]][[1]] <- cbind(UGA_Label, Pairs[[1]][[1]])
  colnames(Pairs[[1]][[1]])[[1]] <- c('Sample')
  Pairs[[1]][[1]]$Sample <- as.character(Pairs[[1]][[1]]$Sample)
  
Pairs[[1]][[2]] <- cbind(MTurk_Label, Pairs[[1]][[2]])
  colnames(Pairs[[1]][[2]])[[1]] <- c('Sample')
  Pairs[[1]][[2]]$Sample <- as.character(Pairs[[1]][[2]]$Sample)
  
`U.S. Pairs` <- rbind(Pairs[[1]][[1]], Pairs[[1]][[2]])

write_csv(`U.S. Pairs`, file.path(DATA.DIR, "U.S. Intersect.csv"))

#Cannonical
cannonical[[1]][[1]] <- cbind(UGA_Label, cannonical[[1]][[1]])
  colnames(cannonical[[1]][[1]])[[1]] <- c('Sample')
  cannonical[[1]][[1]]$Sample <- as.character(cannonical[[1]][[1]]$Sample)
  
cannonical[[1]][[2]] <- cbind(MTurk_Label, cannonical[[1]][[2]])
  colnames(cannonical[[1]][[2]])[[1]] <- c('Sample')
  cannonical[[1]][[2]]$Sample <- as.character(cannonical[[1]][[2]]$Sample)
  
`U.S. Cannonical` <- rbind(cannonical[[1]][[1]], cannonical[[1]][[2]])
  
write_csv(`U.S. Cannonical`, file.path(DATA.DIR, "U.S. Intersect Formatted.csv"))

#Egypt vs. UGA

#Pairs
UGA_Label <- names(Pairs[[2]])[[1]]
Egypt_Label <-  names(Pairs[[2]])[[2]]

Pairs[[2]][[1]] <- cbind(UGA_Label, Pairs[[2]][[1]])
  colnames(Pairs[[2]][[1]])[[1]] <- c('Sample')
  Pairs[[2]][[1]]$Sample <- as.character(Pairs[[2]][[1]]$Sample)

Pairs[[2]][[2]] <- cbind(Egypt_Label, Pairs[[2]][[2]])
  colnames(Pairs[[2]][[2]])[[1]] <- c('Sample')
  Pairs[[2]][[2]]$Sample <- as.character(Pairs[[2]][[2]]$Sample)

`US_Egypt Pairs` <- rbind(Pairs[[2]][[1]], Pairs[[2]][[2]])
  
write_csv(`US_Egypt Pairs`, file.path(DATA.DIR, "U.S. and Egypt Intersect.csv"))

#Cannonical
cannonical[[2]][[1]] <- cbind(UGA_Label, cannonical[[2]][[1]])
  colnames(cannonical[[2]][[1]])[[1]] <- c('Sample')
  cannonical[[2]][[1]]$Sample <- as.character(cannonical[[2]][[1]]$Sample)

cannonical[[2]][[2]] <- cbind(Egypt_Label, cannonical[[2]][[2]])
  colnames(cannonical[[2]][[2]])[[1]] <- c('Sample')
  cannonical[[2]][[2]]$Sample <- as.character(cannonical[[2]][[2]]$Sample)
  
`US_Egypt Cannonical` <- rbind(cannonical[[2]][[1]], cannonical[[2]][[2]])
  
write_csv(`US_Egypt Cannonical`, file.path(DATA.DIR, "U.S. and Egypt Intersect Formatted.csv"))
  
#Morocco vs. UGA

UGA_Label <- names(Pairs[[3]])[[1]]
Morocco_Label <-  names(Pairs[[3]])[[2]]

#Pairs
Pairs[[3]][[1]] <- cbind(UGA_Label, Pairs[[3]][[1]])
  colnames(Pairs[[3]][[1]])[[1]] <- c('Sample')
  Pairs[[3]][[1]]$Sample <- as.character(Pairs[[3]][[1]]$Sample)

Pairs[[3]][[2]] <- cbind(Morocco_Label, Pairs[[3]][[2]])
  colnames(Pairs[[3]][[2]])[[1]] <- c('Sample')
  Pairs[[3]][[2]]$Sample <- as.character(Pairs[[3]][[2]]$Sample) 
  
`US_Morocco Pairs` <- rbind(Pairs[[3]][[1]], Pairs[[3]][[2]])
  
write_csv(`US_Morocco Pairs`, file.path(DATA.DIR, "U.S. and Morocco Intersect.csv"))

#Cannonical
cannonical[[3]][[1]] <- cbind(UGA_Label, cannonical[[3]][[1]])
  colnames(cannonical[[3]][[1]])[[1]] <- c('Sample')
  cannonical[[3]][[1]]$Sample <- as.character(cannonical[[3]][[1]]$Sample)

cannonical[[3]][[2]] <- cbind(Morocco_Label, cannonical[[3]][[2]])
  colnames(cannonical[[3]][[2]])[[1]] <- c('Sample')
  cannonical[[3]][[2]]$Sample <- as.character(cannonical[[3]][[2]]$Sample) 
  
`US_Morocco Cannonical` <- rbind(cannonical[[3]][[1]], cannonical[[3]][[2]])
  
write_csv(`US_Morocco Cannonical`, file.path(DATA.DIR, "U.S. and Morocco Intersect Formatted.csv"))

#Egypt vs. Morocco
  
Egypt_Label <- names(Pairs[[4]])[[1]]
Morocco_Label <-  names(Pairs[[4]])[[2]]

#Pairs
Pairs[[4]][[1]] <- cbind(Egypt_Label, Pairs[[4]][[1]])
  colnames(Pairs[[4]][[1]])[[1]] <- c('Sample')
  Pairs[[4]][[1]]$Sample <- as.character(Pairs[[4]][[1]]$Sample)
  
Pairs[[4]][[2]] <- cbind(Morocco_Label, Pairs[[4]][[2]])
  colnames(Pairs[[4]][[2]])[[1]] <- c('Sample')
  Pairs[[4]][[2]]$Sample <- as.character(Pairs[[4]][[2]]$Sample) 

`Egypt_Morocco Pairs` <- rbind(Pairs[[4]][[1]], Pairs[[4]][[2]])
  
write_csv(`Egypt_Morocco Pairs`, file.path(DATA.DIR, "Egypt and Morocco Intersect.csv"))

#Cannonical
cannonical[[4]][[1]] <- cbind(Egypt_Label, cannonical[[4]][[1]])
  colnames(cannonical[[4]][[1]])[[1]] <- c('Sample')
  cannonical[[4]][[1]]$Sample <- as.character(cannonical[[4]][[1]]$Sample)

cannonical[[4]][[2]] <- cbind(Morocco_Label, cannonical[[4]][[2]])
  colnames(cannonical[[4]][[2]])[[1]] <- c('Sample')
  cannonical[[4]][[2]]$Sample <- as.character(cannonical[[4]][[2]]$Sample) 
  
`Egypt_Morocco Cannonical` <- rbind(cannonical[[4]][[1]], cannonical[[4]][[2]])
  
write_csv(`Egypt_Morocco Cannonical`, file.path(DATA.DIR, "Egypt and Morocco Intersect Formatted.csv"))

#Write Out Similarity Scores
readr::write_csv(`Culture Similarity Scores`, file.path(DATA.DIR, "Culture Similarity Scores.csv"))