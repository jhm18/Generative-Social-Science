#COP Bump Chart Example
#Jonathan H. Morgan
#1 January 2019

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Home Machine
setwd("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities Data")
getwd()

################
#   PACKAGES   # 
################

library(readr)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(ggplotify)
library(magick)

#################
#   FUNCTIONS   #
#################

#Home Utilities
source("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities_2Oct2020.R")
source("~/R Resources/R Plotting Utilities & Resources/util.R")

######################
#   IMPORTING DATA   #
######################

#Data came from: https://docs.google.com/spreadsheets/d/1UDxVCqLj4FLZxFOGuKgV1IFm6At3xOCL6BJizJjcBQU/edit#gid=0
COPs_Themes <- readr::read_csv("COPs themes over time - Sheet 1.csv")

#######################
#   FORMATTING DATA   #
#######################

cop_id <- trim(sub("\\-.*", "", COPs_Themes[[3]]))

cop_index <- as.data.frame(unique(cop_id), stringsAsFactors = FALSE)
colnames(cop_index)[[1]] <- c('COP_ID')

cop_year <- c(1995, 1996, 1997, 1998, 1999, 2000, 2001, 2001, 2002, 2003, 2004, 2005,
              2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 1995)

cop_loc <- c('Berlin', 'Geneva', 'Kyoto', 'Buenos Aires', 'Bonn', 'The Hague', 'Bonn', 'Marrakech',
             'New Delhi', 'Milan', 'Buenos Aires', 'Montreal', 'Nairobi', 'Bali', 'Poznan', 'Copenhagen', 
             'Cancun', 'Durban','Doha','Warsaw', 'New York')

cop_label <- c("COP 01", "COP 02", "COP 03", "COP 04", "COP 05", "COP 06", "COP 06b", "COP 07", "COP 08", "COP 09", "COP 10",          
               "COP 11", "COP 12","COP 13", "COP 14", "COP 15", "COP 16", "COP 17", "COP 18", "COP 19", "INC 11") 

cop_index$cop_year <- cop_year
cop_index$cop_loc <- cop_loc
cop_index$cop_label <- cop_label

rm(cop_year, cop_loc, cop_label)

#Merging index values with COP_Themes Data
COPs_Themes <- merge(COPs_Themes, cop_index, by="COP_ID",all.x=TRUE) 

#Final Formatting
COPs_Themes <- COPs_Themes[-c(1)]
colnames(COPs_Themes)[[1]] <- c('Themes')
COPs_Themes$Themes <- gsub("^(\\w)(\\w+)", "\\U\\1\\L\\2", COPs_Themes$Themes, perl = TRUE)

rm(cop_index, cop_id)

#save(COPs_Themes, file='~/Desktop/COPs_Themes.Rdata')

########################
#   VISUALIZING DATA   #
########################

load('COPs_Themes.Rdata')

#Creating Visualization Components
index <- as.data.frame(seq(1,21,by=1))
cop_label <- c("INC 11", "COP 01", "COP 02", "COP 03", "COP 04", "COP 05", "COP 06", 
               "COP 06b", "COP 07", "COP 08", "COP 09", "COP 10", "COP 11", "COP 12", 
               "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", "COP 18", "COP 19")
index$cop_label <- cop_label
colnames(index)[[1]] <- c('cop_id')

COPs_Themes <- merge(COPs_Themes, index, by="cop_label",all.x=TRUE) 

#Creating themes index: Used to generate the flow polygons in the plot.
theme_index <- as.data.frame(unique(COPs_Themes$Themes), stringsAsFactors=FALSE)
theme_index$theme_id <- c(1,11, 3, 2, 8, 6, 4, 9, 5, 7, 10, 12)
colnames(theme_index)[[1]] <- c('Themes')
theme_index <- theme_index[order(theme_index$theme_id), ]

#Merging theme ids with COP_Themes Data
COPs_Themes <- merge(COPs_Themes, theme_index, by="Themes",all.x=TRUE) 
COPs_Themes <- COPs_Themes[order(COPs_Themes$cop_id), ]

#Creating Splines
curve_list <- vector('list', nrow(theme_index))
names(curve_list) <- theme_index$Themes

for (i in seq_along(theme_index[[1]])){
  Theme <- COPs_Themes %>%
    dplyr::filter(theme_id == i)

  xy <- Theme[c(7, 3)]

  #Spline Fitting
  plot(0)
  val.curve <- xspline(xy, shape = -0.5, draw = FALSE)
  val.curve <- data.frame(val.curve)
  junk <- dev.off(which = dev.cur())

  val.curve$base_y <- 0.75*val.curve$y
  
  curve_list[[i]] <- val.curve

  rm(Theme, xy, val.curve)
}

rm(junk)

#Creating color variables for rgb function
reds <- rev(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.7, 0.8, 0.9, 1))
greens <- c(0.5, 0.55, 0.6, 0.7, 0, 0.1, 0.2, 0.3, 0.4, 0.8, 0.9, 1)
blues <- c(0, 0.1,  0.6, 0.7, 0.8, 0.9, 1, 0.2, 0.3, 0.4, 0.5, 0.55)

#Plotting
png('p_1.png', width=1243, height = 621)
  par(mar = c(3, 9, 3, 0), family='serif')  
  plot(0, type='n', xlim=c(1, 21), ylim=c(-20, 100), xlab=' ', ylab=' ', cex.lab=1.5, family='serif', 
     axes=FALSE, bty='n')

  #Adding Axis Text
  mtext(side=1, c('1995', ' ', '1996', '1997', '1998', '1999', '2000', '2001', ' ', 
                '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
                '2010', '2011', '2012', '2013'), 
      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.1, cex=1)

  mtext(side=3, c('New York', 'Berlin', 'Geneva', 'Kyoto', 'Buenos Aires', 'Bonn', 'The Hague', 'Bonn', 'Marrakech', 
                'New Delhi', 'Milan', 'Buenos Aires', 'Montreal', 'Nairobi', 'Bali', 'Poznan', 'Copenhagen',
                'Cancun', 'Durban', 'Doha', 'Warsaw'), 
      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.1, cex=0.75)

  mtext(side=3, c('INC 11', 'COP 1', 'COP 02', 'COP 03', 'COP 04', 'COP 05', 'COP 06', 'COP 06b', 'COP 07', 
                'COP 08', 'COP 09', 'COP 10', 'COP 11', 'COP 12', 'COP 13', 'COP 14', 'COP 15',
                'COP 16', 'COP 17', 'COP 18', 'COP 19'), 
      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.85, cex=1, font=2)

  #Adding Reference Lines
  for (i in seq_along(index[[1]])){
    abline(v=i, lty=1, col="grey50")
  }

  #Adding Flow Polygons
  for (i in seq_along(curve_list)){
    lines(curve_list[[i]]$x, curve_list[[i]]$base_y, col="black", lwd=1)
    lines(curve_list[[i]]$x, curve_list[[i]]$y, col="black", lwd=1)
    polygon(c(curve_list[[i]]$x,rev(curve_list[[i]]$x)), 
        c(curve_list[[i]]$y,rev(curve_list[[i]]$base_y)) ,
        col=rgb(reds[[i]], greens[[i]], blues[[i]], 0.5), border = NA)
  }

  #Adding y-axis tick labels
  mtext(side = 2, text = 'Adaptation funding & equity', at =29.75, line = -1.4, cex = 0.9, las=1)
  mtext(side = 2, text = 'GHGs & emission measures', at =23.625, line = -1.4, cex = 0.9, las=1)
  mtext(side = 2, text = 'Energy + technology transfer', at =19, line = -1.4, cex = 0.9, las=1)
  mtext(side = 2, text = 'Models and IPCC', at =13.125, line = -1.4, cex = 0.9, las=1)
  mtext(side = 2, text = 'Transport sector', at =6.2, line = -1.4, cex = 0.9, las=1)
  mtext(side = 2, text = 'Land use & forests', at =3, line = -1.4, cex = 0.9, las=1)

  mtext(side = 2, text = 'Vulnerability + adaptation action', at =.3, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 3), y=c(0.3, 0.3), col = 'black', lty = 3, lwd=1.5)
  lines(x=c(3, 3), y=c(0.3, 3.5), col = 'black', lty = 3, lwd= 1.5)

  mtext(side = 2, text = 'CDM + carbon offsets', at =-3.25, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 4), y=c(-3.25, -3.25), col = 'black', lty = 3, lwd=1.5)
  lines(x=c(4, 4), y=c(-3.25, 3), col = 'black', lty = 3, lwd= 1.5)

  mtext(side = 2, text = 'Kyoto protocol', at =-6.25, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 5), y=c(-6.25, -6.25), col = 'black', lty = 3, lwd=1.5)
  lines(x=c(5, 5), y=c(-6.25, 19.5), col = 'black', lty = 3, lwd= 1.5)

  mtext(side = 2, text = 'Social & environmental impacts', at =-9.25, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 6), y=c(-9.25, -9.25), col = 'black', lty = 3, lwd=1.5)
  lines(x=c(6, 6), y=c(-8, 3.5), col = 'black', lty = 3, lwd= 1.5)

  mtext(side = 2, text = 'Compliance enforcement', at =-13, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 2), y=c(-13, -13), col = 'red', lty = 3, lwd=1.5)
  lines(x=c(2, 2), y=c(-13, 2), col = 'red', lty = 3, lwd= 1.5)

  mtext(side = 2, text = 'Post-Kyoto + Redd', at =-16.25, line = -1.4, cex = 0.9, las=1)
  lines(x=c(1, 5), y=c(-16.25, -16.25), col = 'red', lty = 3, lwd=1.5)
  lines(x=c(5, 5), y=c(-16.25, 2.3), col = 'red', lty = 3, lwd= 1.5)

  #Adding Note
  mtext(side=1, c("The height of the flows indicates the theme's frequency for that year."), 
      at=c(2), line=1.3, cex=1)
dev.off()
  
g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p <- ggplotify::as.ggplot(g)  

ggplot2::ggsave("Example Bump Chart_COP_Themes_2Jan2019.pdf")

