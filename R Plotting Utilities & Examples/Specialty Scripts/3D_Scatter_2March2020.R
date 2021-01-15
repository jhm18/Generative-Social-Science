#Panel 3D Scatter
#RPbus: https://rpubs.com/yoshio/95844
#2 March 2019

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

################
#   PACKAGES   #
################

library(plot3D)

###############################
#   EXAMPLE 3D SCATTER PLOT   #
###############################

#Additonal arguments: https://cran.r-project.org/web/packages/plot3D/plot3D.pdf
  #xlim (range of x)
  #ylim (range of y)
  #zlim (range of z)

#Plot Elements
z <- seq(-4.3, 4.3 , 0.5)
x <- cos(z)
y <- sin(z)*z

png("p_1.png", width = 877, height = 676)
  par (mfrow = c(2, 2), mar=c(2, 0, 2, 0), family='serif')
  #Plot 1: A Ribbon of EPA
  scatter3D(x, y, z, phi = 0, bty = "b2", pch = 20, cex = 2, ticktype = "detailed", 
          xlab="Evaluation", ylab="Potency", zlab="Activity", 
          xlim=c(-4.3, 4.3), ylim=c(-4.3, 4.3), zlim=c(-4.3, 4.3), main="Points")

  # add text
  text3D(x =cos(-4.3:4.3), y = (sin(-4.3:4.3)*(-4.3:4.3) - 1), 
        z = -4.3:4.3, colkey = FALSE, add = TRUE, 
        labels = LETTERS[1:9], col = c("black", "red"))

  #Plot 2: Line Plot
  scatter3D(x, y, z, phi = 0, bty = "b2", type = "l", ticktype = "detailed", lwd = 4,
          xlab="Evaluation", ylab="Potency", zlab="Activity", main="Lines",
          xlim=c(-4.3, 4.3), ylim=c(-4.3, 4.3), zlim=c(-4.3, 4.3))

  #Plot 3: Points and Lines
  scatter3D(x, y, z, phi = 0, bty = "b2", type = "b", ticktype = "detailed", pch = 20,
          cex = c(0.5, 1, 1.5), xlab="Evaluation", ylab="Potency", zlab="Activity", 
          xlim=c(-4.3, 4.3), ylim=c(-4.3, 4.3), zlim=c(-4.3, 4.3), main="Points & Lines")

  #Plot 3: Vertical Lines
  scatter3D(x, y, z, phi = 0, bty = "b2",  type = "h", ticktype = "detailed",
           xlab="Evaluation", ylab="Potency", zlab="Activity", 
           xlim=c(-4.3, 4.3), ylim=c(-4.3, 4.3), zlim=c(-4.3, 4.3), main="Drop Lines")
dev.off()

g <- magick::image_read('p_1.png')
file.remove('p_1.png')
p_1 <- ggplotify::as.ggplot(g)
rm(g)

ggplot2::ggsave("SSC Verification_Median Agent Deflection_20Feb2020.pdf", 
                dpi=600, width = 8.5, height = 5)




