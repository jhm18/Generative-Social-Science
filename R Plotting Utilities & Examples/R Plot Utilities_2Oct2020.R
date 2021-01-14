#Plot Utilities
#Jonathan H. Morgan
#17 November 2019

#Installing Necessary Packages
list.of.packages <- c('ggplot2', 'pipeR', 'magick', 'dplyr', 'plyr', 'questionr', 'fossil', 'sm', 'ggfortify',
                      'ggplotify', 'gridExtra', 'RColorBrewer', 'yarrr', 'devtools', 'scales', 'stringi')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

rm(list.of.packages, new.packages)

###############################
#   VISUALIZATION FUNCTIONS   #
###############################

util = new.env()

#Base R Graphics Style for Consistency
util$base_breaks_x <- function(x, l=pretty(x)){
  b <- pretty(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=b, labels=l))
}

util$base_breaks_x_d <- function(x){
  b <- seq(1, length(unique(x)), by=1)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend)))
}

util$base_breaks_y <- function(x, l=pretty(x)){
  b <- pretty(x)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=b, labels=l))
}

util$base_breaks_y_d <- function(y){
  b <- seq(1, length(unique(y)), by=1)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend)))
}

util$rotate <- function(x) t(apply(x, 2, rev))

util$plotsegraph <- function(loc, value, sterr, wiskwidth, color = "grey", linewidth = 2) {
  w <- wiskwidth/2
  segments(x0 = loc, x1 = loc, y0 = value - sterr, y1 = value + sterr, col = color, 
           lwd = linewidth)
  segments(x0 = loc - w, x1 = loc + w, y0 = value + sterr, y1 = value + sterr, 
           col = color, lwd = linewidth)  # upper whiskers
  segments(x0 = loc - w, x1 = loc + w, y0 = value - sterr, y1 = value - sterr, 
           col = color, lwd = linewidth)  # lower whiskers
}

util$find_modes<- function(x) {
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

#Waterfall Plot: Developed by https://github.com/EconometricsBySimulation/BivariateSlicer/blob/master/slicedens.R
#October 10, 2014

#Function Parameters
  # x, y, z: data
  # slices: number of horizontal slices through the data
  # lboost: coefficient to increase the height of the lines
  # gboost: coefficient to increase the height of the graph (ylim)
  # xinc: horizontal offset for each succesive slice
  # (typically something like 1/80)
  # yinc: vertical offset for each succesive slice
  # bcol: background color
  # fcol: fill color for each slice (polygon)
  # lcol: line color for each slice
  # lwidth: line width
  # cutprop: 
  # Vary transarency 
  # transprop=FALSE
  # tmax = .9
  # tmin = .2

util$slicedens <- function(x,y,z=NULL,slices=50,lboost=1,gboost=1,
                    xinc=0,yinc=0.01, bcol="black", fcol="black",
                    lcol="white",lwidth=1, cutprop=FALSE,
                    transprop=FALSE, tmax = .8, tmin = .05,
                    heightprop=FALSE, xlab=FALSE) {
  
  # This function takes a matrix of one or more rgb sets
  # as well as a degree [0,1] and returns a combined
  # color.
  color.mix <- function(cols, degree=0) {
    if (is.null(nrow(cols))) {
      if (class(cols)=="numeric") 
        return(rgb(cols[1],cols[2],cols[3],cols[4]))
      return(cols)
    }
    # Define a function to find elementwise minimum 
    (deg <- degree*(nrow(cols)-1)+1)
    emin <- function(x, y=0) apply(cbind(x, y), 1, min)
    (r <- 1-emin(abs(1:nrow(cols)-deg),1))
    (comb <- apply(cols*r,2,sum))
    mm <- function(x) max(min(x,1),0)
    rgb(mm(comb[1]),
        mm(comb[2]),
        mm(comb[3]),
        mm(comb[4]))
  }
  
  ycut<-min(y)+((0:(slices))*(max(y)-min(y))/slices)
  
  height<-gboost*((slices*yinc)+max(density(x)$y))
  
  plot( c(min(x)-((max(x)-min(x))/10),max(x)+((max(x)-min(x))/10)),
        c(0,height),
        xaxt="n",yaxt="n",ylab="",xlab="")
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
  
  # Calcuate the 'degree' for each z value which will be used to choose the color of each slice.
  if (length(z)==length(y)) {
    zmin <- min(z)
    zmax <- max(z)
    zrange <- max(zmax-zmin)
  }
  
  # Define ifcol and ilfol for later reference.
  # Unless noted otherwise, degree=0
  # Meaning the first color will be selected from
  # the rgb color matrix.
  ifcol <- fcol; ilcol <- lcol;  zdeg <- 0
  
  # Define the degree which is the color degree that each slice will
  # contain
  if (length(z)==length(y)){
    meanz <- NULL
    for(i in slices:1)
      meanz[i]<- mean(z[y>=min(ycut[i]) & y<max(ycut[i+1])])
    zdegree<-(meanz-min(meanz, na.rm=TRUE))/
      (max(meanz, na.rm=TRUE)-min(meanz, na.rm=TRUE))
  }
  
  # Loop through and plot slices
  for(i in slices:1) {
    miny<-ycut[i]
    maxy<-ycut[i+1]
    
    gx<-(i-1)*(max(x)-min(x))*xinc
    
    if (cutprop) {
      slLength <- slices*sum(y>=miny & y<maxy)/length(y)
      if (i==slices) gy <- (i-1)*(height)*yinc
      if (i<slices)  gy <- gyLast-(height)*yinc*slLength
      gyLast <- gy
    }
    else gy<-(i-1)*(height)*yinc
    
    if (transprop) {
      trange <- tmax-tmin
      if (is.null(nrow(ifcol))) 
        ifcol[4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
      if (!is.null(nrow(ifcol))) 
        ifcol[,4] <- min(trange*slices*sum(y>=miny & y<maxy)/length(y)+tmin,tmax)
    }
    
    # If z is an input vector then grab the color degree from it
    if (length(z)==length(y)) zdeg<-zdegree[i]
    
    # Added the try because distributions without defined upper
    # and lower bounds can give the density function trouble.
    try({
      # Use the color.mixer function to select a color
      fcol<-color.mix(ifcol, zdeg);
      lcol<-color.mix(ilcol, zdeg);
      # Calculte density curves and plot them
      dd<-density(x[y>=miny & y<maxy]);
      if (heightprop) vscale <- lboost*slices*sum(y>=miny & y<maxy)/length(y)
      if (!heightprop) vscale <- lboost
      polygon(dd$x+gx,vscale*dd$y+gy,col=fcol, border=fcol);
      lines(dd$x+gx,vscale*dd$y+gy,col=lcol,lwd=lwidth);
    })
  }
}

#3D Polar Plot: Developed by https://stackoverflow.com/users/165089/nate
#23 May 2017

library('plyr')
library('dplyr')
util$PolarImageInterpolate <- function(
  ### Plotting data (in cartesian) - will be converted to polar space.
  x, y, z, 
  ### Plot component flags
  contours=TRUE,   # Add contours to the plotted surface
  legend=TRUE,        # Plot a surface data legend?
  axes=TRUE,      # Plot axes?
  points=TRUE,        # Plot individual data points
  extrapolate=FALSE, # Should we extrapolate outside data points?
  ### Data splitting params for color scale and contours
  col_breaks_source = 1, # Where to calculate the color brakes from (1=data,2=surface)
  # If you know the levels, input directly (i.e. c(0,1))
  col_levels = 10,    # Number of color levels to use - must match length(col) if 
  #col specified separately
  col = rev(heat.colors(col_levels)),  # Colors to plot
  contour_breaks_source = 1, # 1=z data, 2=calculated surface data
  # If you know the levels, input directly (i.e. c(0,1))
  contour_levels = col_levels+1, # One more contour break than col_levels (must be
  # specified correctly if done manually
  ### Plotting params
  outer.radius = round_any(max(sqrt(x^2+y^2)),5,f=ceiling),  
  circle.rads = pretty(c(0,outer.radius)), #Radius lines
  spatial_res=1000, #Resolution of fitted surface
  single_point_overlay=0, #Overlay "key" data point with square 
  #(0 = No, Other = number of pt)
  ### Fitting parameters
  interp.type = 1, #1 = linear, 2 = Thin plate spline 
  lambda=0){ #Used only when interp.type = 2
  
  minitics <- seq(-outer.radius, outer.radius, length.out = spatial_res)
  # interpolate the data
  if (interp.type ==1 ){
    Interp <- akima:::interp(x = x, y = y, z = z, 
                             extrap = extrapolate, 
                             xo = minitics, 
                             yo = minitics,
                             duplicate = "median",   #User Defined
                             linear = FALSE)
    Mat <- Interp[[3]]
  }
  else if (interp.type == 2){
    library(fields)
    grid.list = list(x=minitics,y=minitics)
    t = Tps(cbind(x,y),z,lambda=lambda)
    tmp = predict.surface(t,grid.list,extrap=extrapolate)
    Mat = tmp$z
  }
  else {stop("interp.type value not valid")}
  
  # mark cells outside circle as NA
  markNA <- matrix(minitics, ncol = spatial_res, nrow = spatial_res) 
  Mat[!sqrt(markNA ^ 2 + t(markNA) ^ 2) < outer.radius] <- NA 
  
  ### Set contour_breaks based on requested source
  if ((length(contour_breaks_source == 1)) & (contour_breaks_source[1] == 1)){
    contour_breaks = seq(min(z,na.rm=TRUE),max(z,na.rm=TRUE),
                         by=(max(z,na.rm=TRUE)-min(z,na.rm=TRUE))/(contour_levels-1))
  }
  else if ((length(contour_breaks_source == 1)) & (contour_breaks_source[1] == 2)){
    contour_breaks = seq(min(Mat,na.rm=TRUE),max(Mat,na.rm=TRUE),
                         by=(max(Mat,na.rm=TRUE)-min(Mat,na.rm=TRUE))/(contour_levels-1))
  } 
  else if ((length(contour_breaks_source) == 2) & (is.numeric(contour_breaks_source))){
    contour_breaks = pretty(contour_breaks_source,n=contour_levels)
    contour_breaks = seq(contour_breaks_source[1],contour_breaks_source[2],
                         by=(contour_breaks_source[2]-contour_breaks_source[1])/(contour_levels-1))
  }
  else {stop("Invalid selection for \"contour_breaks_source\"")}
  
  ### Set color breaks based on requested source
  if ((length(col_breaks_source) == 1) & (col_breaks_source[1] == 1))
  {zlim=c(min(z,na.rm=TRUE),max(z,na.rm=TRUE))}
  else if ((length(col_breaks_source) == 1) & (col_breaks_source[1] == 2))
  {zlim=c(min(Mat,na.rm=TRUE),max(Mat,na.rm=TRUE))}
  else if ((length(col_breaks_source) == 2) & (is.numeric(col_breaks_source)))
  {zlim=col_breaks_source}
  else {stop("Invalid selection for \"col_breaks_source\"")}
  
  # begin plot
  Mat_plot = Mat
  Mat_plot[which(Mat_plot<zlim[1])]=zlim[1]
  Mat_plot[which(Mat_plot>zlim[2])]=zlim[2]
  image(x = minitics, y = minitics, Mat_plot , useRaster = TRUE, asp = 1, axes = FALSE, xlab = "", ylab = "", zlim = zlim, col = col)
  
  # add contours if desired
  if (contours){
    CL <- contourLines(x = minitics, y = minitics, Mat, levels = contour_breaks)
    A <- lapply(CL, function(xy){
      lines(xy$x, xy$y, col = gray(.2), lwd = .5)
    })
  }
  # add interpolated point if desired
  if (points){
    points(x,y,pch=4)
  }
  # add overlay point (used for trained image marking) if desired
  if (single_point_overlay!=0){
    points(x[single_point_overlay],y[single_point_overlay],pch=0)
  }
  
  # add radial axes if desired
  if (axes){ 
    # internals for axis markup
    RMat <- function(radians){
      matrix(c(cos(radians), sin(radians), -sin(radians), cos(radians)), ncol = 2)
    }    
    
    circle <- function(x, y, rad = 1, nvert = 500){
      rads <- seq(0,2*pi,length.out = nvert)
      xcoords <- cos(rads) * rad + x
      ycoords <- sin(rads) * rad + y
      cbind(xcoords, ycoords)
    }
    
    # draw circles
    if (missing(circle.rads)){
      circle.rads <- pretty(c(0,outer.radius))
    }
    
    for (i in circle.rads){
      lines(circle(0, 0, i), col = "#66666650")
    }
    
    # put on radial spoke axes:
    axis.rads <- c(0, pi / 6, pi / 3, pi / 2, 2 * pi / 3, 5 * pi / 6)
    r.labs <- c(90, 60, 30, 0, 330, 300)
    l.labs <- c(270, 240, 210, 180, 150, 120)
    
    for (i in 1:length(axis.rads)){ 
      endpoints <- zapsmall(c(RMat(axis.rads[i]) %*% matrix(c(1, 0, -1, 0) * outer.radius,ncol = 2)))
      segments(endpoints[1], endpoints[2], endpoints[3], endpoints[4], col = "#66666650")
      endpoints <- c(RMat(axis.rads[i]) %*% matrix(c(1.1, 0, -1.1, 0) * outer.radius, ncol = 2))
      lab1 <- bquote(.(r.labs[i]) * degree)
      lab2 <- bquote(.(l.labs[i]) * degree)
      text(endpoints[1], endpoints[2], lab1, xpd = TRUE)
      text(endpoints[3], endpoints[4], lab2, xpd = TRUE)
    }
    
    axis(2, pos = -1.25 * outer.radius, at = sort(union(circle.rads,-circle.rads)), labels = NA)
    text( -1.26 * outer.radius, sort(union(circle.rads, -circle.rads)),sort(union(circle.rads, -circle.rads)), xpd = TRUE, pos = 2)
  }
  
  # add legend if desired
  # this could be sloppy if there are lots of breaks, and that's why it's optional.
  # another option would be to use fields:::image.plot(), using only the legend. 
  # There's an example for how to do so in its documentation
  if (legend){
    library(fields)
    image.plot(legend.only=TRUE, smallplot=c(.78,.82,.1,.8), col=col, zlim=zlim)
    # ylevs <- seq(-outer.radius, outer.radius, length = contour_levels+ 1)
    # #ylevs <- seq(-outer.radius, outer.radius, length = length(contour_breaks))
    # rect(1.2 * outer.radius, ylevs[1:(length(ylevs) - 1)], 1.3 * outer.radius, ylevs[2:length(ylevs)], col = col, border = NA, xpd = TRUE)
    # rect(1.2 * outer.radius, min(ylevs), 1.3 * outer.radius, max(ylevs), border = "#66666650", xpd = TRUE)
    # text(1.3 * outer.radius, ylevs[seq(1,length(ylevs),length.out=length(contour_breaks))],round(contour_breaks, 1), pos = 4, xpd = TRUE)
  }
}

#vioplot.singmann function: Developed by Henrik Singmann
library(sm)
util$vioplot.singmann <- function(x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                             horizontal = FALSE, col = NULL, border = "black", lty = 1, lwd = 1, rectCol = "black", 
                             colMed = "white", pchMed = 19, at, add = FALSE, wex = 1, mark.outlier = TRUE, 
                             pch.mean = 4, ids = NULL, drawRect = TRUE, yaxt = "s") {
  
  # process multiple datas
  datas <- list(x, ...)
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  # pass 1 - calculate base range - estimate density setup parameters for
  # density estimation
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  outliers <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  
  # global args for sm.density function-call
  args <- list(display = "none")
  
  if (!(is.null(h))) 
    args <- c(args, h = h)
  for (i in 1:n) {
    data <- datas[[i]]
    if (!is.null(ids)) 
      names(data) <- ids
    if (is.null(names(data))) 
      names(data) <- as.character(1:(length(data)))
    
    # calculate plot parameters 1- and 3-quantile, median, IQR, upper- and
    # lower-adjacent
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    
    # strategy: xmin = min(lower, data.min)) ymax = max(upper, data.max))
    est.xlim <- c(min(lower[i], data.min), max(upper[i], data.max))
    
    # estimate density curve
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), args))
    
    # calculate stretch factor the plots density heights is defined in range 0.0
    # ... 0.5 we scale maximum estimated point to 0.4 per data
    hscale <- 0.4/max(smout$estimate) * wex
    
    # add density curve x,y pair to lists
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
    min.d <- boxplot.stats(data)[["stats"]][1]
    max.d <- boxplot.stats(data)[["stats"]][5]
    height[[i]] <- height[[i]][(base[[i]] > min.d) & (base[[i]] < max.d)]
    height[[i]] <- c(height[[i]][1], height[[i]], height[[i]][length(height[[i]])])
    base[[i]] <- base[[i]][(base[[i]] > min.d) & (base[[i]] < max.d)]
    base[[i]] <- c(min.d, base[[i]], max.d)
    outliers[[i]] <- list(data[(data < min.d) | (data > max.d)], names(data[(data < 
                                                                               min.d) | (data > max.d)]))
    
    # calculate min,max base ranges
  }
  # pass 2 - plot graphics setup parameters for plot
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5) else range(at) + min(diff(at))/2 * c(-1, 1)
    
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  } else {
    label <- names
  }
  boxwidth <- 0.05 * wex
  
  # setup plot
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    
    box()
    for (i in 1:n) {
      # plot left/right density curve
      polygon(c(at[i] - height[[i]], rev(at[i] + height[[i]])), c(base[[i]], 
                                                                  rev(base[[i]])), col = col, border = border, lty = lty, lwd = lwd)
      
      if (drawRect) {
        # browser() plot IQR
        boxplot(datas[[i]], at = at[i], add = TRUE, yaxt = yaxt, pars = list(boxwex = 0.6 * 
                                                                               wex, outpch = if (mark.outlier) "" else 1))
        if ((length(outliers[[i]][[1]]) > 0) & mark.outlier) 
          text(rep(at[i], length(outliers[[i]][[1]])), outliers[[i]][[1]], 
               labels = outliers[[i]][[2]])
        # lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty) plot 50% KI
        # box rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)
        # plot median point points( at[i], med[i], pch=pchMed, col=colMed )
      }
      points(at[i], mean(datas[[i]]), pch = pch.mean, cex = 1.3)
    }
  } else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    
    box()
    for (i in 1:n) {
      # plot left/right density curve
      polygon(c(base[[i]], rev(base[[i]])), c(at[i] - height[[i]], rev(at[i] + 
                                                                         height[[i]])), col = col, border = border, lty = lty, lwd = lwd)
      
      if (drawRect) {
        # plot IQR
        boxplot(datas[[i]], yaxt = yaxt, at = at[i], add = TRUE, pars = list(boxwex = 0.8 * 
                                                                               wex, outpch = if (mark.outlier) "" else 1))
        if ((length(outliers[[i]][[1]]) > 0) & mark.outlier) 
          text(rep(at[i], length(outliers[[i]][[1]])), outliers[[i]][[1]], 
               labels = outliers[[i]][[2]])
        # lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)
      }
      points(at[i], mean(datas[[i]]), pch = pch.mean, cex = 1.3)
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, q1 = q1, q3 = q3))
}

#Split Violin Plot Function: Developed by https://gist.github.com/mbjoseph
util$vioplot2 <- function (x, ..., range = 1.5, h = NULL, ylim = NULL, names = NULL, 
                      horizontal = FALSE, col = "magenta", border = "black", lty = 1, 
                      lwd = 1, rectCol = "black", colMed = "white", pchMed = 19, 
                      at, add = FALSE, wex = 1, drawRect = TRUE, side="both") 
{
  datas <- list(x, ...)
  n <- length(datas)
  if (missing(at)) 
    at <- 1:n
  upper <- vector(mode = "numeric", length = n)
  lower <- vector(mode = "numeric", length = n)
  q1 <- vector(mode = "numeric", length = n)
  q2 <- vector(mode = "numeric", length = n)
  q3 <- vector(mode = "numeric", length = n)
  med <- vector(mode = "numeric", length = n)
  base <- vector(mode = "list", length = n)
  height <- vector(mode = "list", length = n)
  baserange <- c(Inf, -Inf)
  args <- list(display = "none")
  radj <- ifelse(side == "right", 0, 1)
  ladj <- ifelse(side == "left", 0, 1)
  if (!(is.null(h))) 
    args <- c(args, h = h)
  med.dens <- rep(NA, n)
  for (i in 1:n) {
    data <- datas[[i]]
    data.min <- min(data)
    data.max <- max(data)
    q1[i] <- quantile(data, 0.25)
    q2[i] <- quantile(data, 0.5)
    q3[i] <- quantile(data, 0.75)
    med[i] <- median(data)
    iqd <- q3[i] - q1[i]
    upper[i] <- min(q3[i] + range * iqd, data.max)
    lower[i] <- max(q1[i] - range * iqd, data.min)
    est.xlim <- c(min(lower[i], data.min), max(upper[i], 
                                               data.max))
    smout <- do.call("sm.density", c(list(data, xlim = est.xlim), 
                                     args))
    med.dat <- do.call("sm.density", 
                       c(list(data, xlim=est.xlim,
                              eval.points=med[i], display = "none")))
    med.dens[i] <- med.dat$estimate
    hscale <- 0.4/max(smout$estimate) * wex
    base[[i]] <- smout$eval.points
    height[[i]] <- smout$estimate * hscale
    med.dens[i] <- med.dens[i] * hscale
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1], t[1])
    baserange[2] <- max(baserange[2], t[2])
  }
  if (!add) {
    xlim <- if (n == 1) 
      at + c(-0.5, 0.5)
    else range(at) + min(diff(at))/2 * c(-1, 1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  }
  else {
    label <- names
  }
  boxwidth <- 0.05 * wex
  if (!add) 
    plot.new()
  if (!horizontal) {
    if (!add) {
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(x = c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
              y = c(base[[i]], rev(base[[i]])), 
              col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(at[c(i, i)], c(lower[i], upper[i]), lwd = lwd, 
              lty = lty)
        rect(at[i] - radj*boxwidth/2, 
             q1[i], 
             at[i] + ladj*boxwidth/2, 
             q3[i], col = rectCol)
        # median line segment
        lines(x = c(at[i] - radj*med.dens[i], 
                    at[i], 
                    at[i] + ladj*med.dens[i]),
              y = rep(med[i],3))
      }
    }
  }
  else {
    if (!add) {
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2, at = at, label = label)
    }
    box()
    for (i in 1:n) {
      polygon(c(base[[i]], rev(base[[i]])), 
              c(at[i] - radj*height[[i]], rev(at[i] + ladj*height[[i]])), 
              col = col, border = border, 
              lty = lty, lwd = lwd)
      if (drawRect) {
        lines(c(lower[i], upper[i]), at[c(i, i)], lwd = lwd, 
              lty = lty)
        rect(q1[i], at[i] - radj*boxwidth/2, q3[i], at[i] + 
               ladj*boxwidth/2, col = rectCol)
        lines(y = c(at[i] - radj*med.dens[i], 
                    at[i], 
                    at[i] + ladj*med.dens[i]),
              x = rep(med[i],3))
      }
    }
  }
  invisible(list(upper = upper, lower = lower, median = med, 
                 q1 = q1, q3 = q3))
}

#Function Used to Add an Alpha Legend: http://www.math.mcmaster.ca/bolker/R/misc/legendx.R
util$legend <- function(x, y = NULL, legend, fill = NULL, col = par("col"), border="black",
         lty, lwd, pch, angle = 45, density = NULL, bty = "o", bg = par("bg"),
         box.lwd = par("lwd"), box.lty = par("lty"), box.col = par("fg"),
         box.cex = c(0.8,0.5),
         pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd,
         xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 0.5),
         text.width = NULL, text.col = par("col"), text.font = NULL,
         merge = do.lines && has.pch, trace = FALSE,
         plot = TRUE, ncol = 1, horiz = FALSE, title = NULL,
         inset = 0, xpd, title.col = text.col, title.adj = 0.5,
         seg.len = 2)
{
  ## the 2nd arg may really be `legend'
  if(missing(legend) && !missing(y) &&
     (is.character(y) || is.expression(y))) {
    legend <- y
    y <- NULL
  }
  mfill <- !missing(fill) || !missing(density)
  
  if(!missing(xpd)) {
    op <- par("xpd")
    on.exit(par(xpd=op))
    par(xpd=xpd)
  }
  title <- as.graphicsAnnot(title)
  if(length(title) > 1) stop("invalid 'title'")
  legend <- as.graphicsAnnot(legend)
  n.leg <- if(is.call(legend)) 1 else length(legend)
  if(n.leg == 0) stop("'legend' is of length 0")
  auto <-
    if (is.character(x))
      match.arg(x, c("bottomright", "bottom", "bottomleft", "left",
                     "topleft", "top", "topright", "right", "center"))
  else NA
  
  if (is.na(auto)) {
    xy <- xy.coords(x, y); x <- xy$x; y <- xy$y
    nx <- length(x)
    if (nx < 1 || nx > 2) stop("invalid coordinate lengths")
  } else nx <- 0
  
  xlog <- par("xlog")
  ylog <- par("ylog")
  
  rect2 <- function(left, top, dx, dy, density = NULL, angle, ...) {
    r <- left + dx; if(xlog) { left <- 10^left; r <- 10^r }
    b <- top  - dy; if(ylog) {  top <- 10^top;  b <- 10^b }
    rect(left, top, r, b, angle = angle, density = density, ...)
  }
  segments2 <- function(x1, y1, dx, dy, ...) {
    x2 <- x1 + dx; if(xlog) { x1 <- 10^x1; x2 <- 10^x2 }
    y2 <- y1 + dy; if(ylog) { y1 <- 10^y1; y2 <- 10^y2 }
    segments(x1, y1, x2, y2, ...)
  }
  points2 <- function(x, y, ...) {
    if(xlog) x <- 10^x
    if(ylog) y <- 10^y
    points(x, y, ...)
  }
  text2 <- function(x, y, ...) {
    ##--- need to adjust  adj == c(xadj, yadj) ?? --
    if(xlog) x <- 10^x
    if(ylog) y <- 10^y
    text(x, y, ...)
  }
  if(trace)
    catn <- function(...)
      do.call("cat", c(lapply(list(...),formatC), list("\n")))
  
  cin <- par("cin")
  Cex <- cex * par("cex")		# = the `effective' cex for text
  
  ## at this point we want positive width even for reversed x axis.
  if(is.null(text.width))
    text.width <- max(abs(strwidth(legend, units="user",
                                   cex=cex, font = text.font)))
  else if(!is.numeric(text.width) || text.width < 0)
    stop("'text.width' must be numeric, >= 0")
  
  xc <- Cex * xinch(cin[1L], warn.log=FALSE) # [uses par("usr") and "pin"]
  yc <- Cex * yinch(cin[2L], warn.log=FALSE)
  if(xc < 0) text.width <- -text.width
  
  xchar  <- xc
  xextra <- 0
  yextra <- yc * (y.intersp - 1)
  ## watch out for reversed axis here: heights can be negative
  ymax   <- yc * max(1, strheight(legend, units="user", cex=cex)/yc)
  ychar <- yextra + ymax
  if(trace) catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra,ychar))
  
  if(mfill) {
    ##= sizes of filled boxes.
    xbox <- xc * box.cex[1]
    ybox <- yc * box.cex[2]
    dx.fill <- xbox ## + x.intersp*xchar
  }
  do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 0))
  ) || !missing(lwd)
  
  ## legends per column:
  n.legpercol <-
    if(horiz) {
      if(ncol != 1)
        warning(gettextf("horizontal specification overrides: Number of columns := %d",
                         n.leg), domain = NA)
      ncol <- n.leg
      1
    } else ceiling(n.leg / ncol)
  
  has.pch <- !missing(pch) && length(pch) > 0 # -> default 'merge' is available
  if(do.lines) {
    x.off <- if(merge) -0.7 else 0
  } else if(merge)
    warning("'merge = TRUE' has no effect when no line segments are drawn")
  
  if(has.pch) {
    if(is.character(pch) && !is.na(pch[1L]) &&
       nchar(pch[1L], type="c") > 1) {
      if(length(pch) > 1)
        warning("not using pch[2..] since pch[1L] has multiple chars")
      np <- nchar(pch[1L], type="c")
      pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
    }
    ##D	if(!merge) dx.pch <- x.intersp/2 * xchar
  }
  
  if (is.na(auto)) {
    ##- Adjust (x,y) :
    if (xlog) x <- log10(x)
    if (ylog) y <- log10(y)
  }
  if(nx == 2) {
    ## (x,y) are specifiying OPPOSITE corners of the box
    x <- sort(x)
    y <- sort(y)
    left <- x[1L]
    top  <- y[2L]
    w <- diff(x)# width
    h <- diff(y)# height
    w0 <- w/ncol # column width
    
    x <- mean(x)
    y <- mean(y)
    if(missing(xjust)) xjust <- 0.5
    if(missing(yjust)) yjust <- 0.5
    
  }
  else {## nx == 1  or  auto
    ## -- (w,h) := (width,height) of the box to draw -- computed in steps
    h <- (n.legpercol + !is.null(title)) * ychar + yc
    w0 <- text.width + (x.intersp + 1) * xchar
    if(mfill)	w0 <- w0 + dx.fill
    ##D	if(has.pch && !merge)	w0 <- w0 + dx.pch
    if(do.lines)		w0 <- w0 + (seg.len + x.off)*xchar
    w <- ncol*w0 + .5* xchar
    if (!is.null(title)
        && (abs(tw <- strwidth(title, units="user", cex=cex) + 0.5*xchar)) > abs(w)) {
      xextra <- (tw - w)/2
      w <- tw
    }
    
    ##-- (w,h) are now the final box width/height.
    
    if (is.na(auto)) {
      left <- x - xjust * w
      top	 <- y + (1 - yjust) * h
    } else {
      usr <- par("usr")
      inset <- rep_len(inset, 2)
      insetx <- inset[1L]*(usr[2L] - usr[1L])
      left <- switch(auto, "bottomright"=,
                     "topright"=, "right" = usr[2L] - w - insetx,
                     "bottomleft"=, "left"=, "topleft"= usr[1L] + insetx,
                     "bottom"=, "top"=, "center"= (usr[1L] + usr[2L] - w)/2)
      insety <- inset[2L]*(usr[4L] - usr[3L])
      top <- switch(auto, "bottomright"=,
                    "bottom"=, "bottomleft"= usr[3L] + h + insety,
                    "topleft"=, "top"=, "topright" = usr[4L] - insety,
                    "left"=, "right"=, "center" = (usr[3L] + usr[4L] + h)/2)
    }
  }
  
  if (plot && bty != "n") { ## The legend box :
    if(trace)
      catn("  rect2(",left,",",top,", w=",w,", h=",h,", ...)",sep="")
    rect2(left, top, dx = w, dy = h, col = bg, density = NULL,
          lwd = box.lwd, lty = box.lty, border = box.col)
  }
  
  ## (xt[],yt[]) := `current' vectors of (x/y) legend text
  xt <- left + xchar + xextra +
    (w0 * rep.int(0:(ncol-1), rep.int(n.legpercol,ncol)))[1L:n.leg]
  yt <- top -	0.5 * yextra - ymax -
    (rep.int(1L:n.legpercol,ncol)[1L:n.leg] - 1 + !is.null(title)) * ychar
  
  if (mfill) {		#- draw filled boxes -------------
    if(plot) {
      if(!is.null(fill)) fill <- rep_len(fill, n.leg)
      rect2(left = xt, top=yt+ybox/2, dx = xbox, dy = ybox,
            col = fill,
            density = density, angle = angle, border = border)
    }
    xt <- xt + dx.fill
  }
  if(plot && (has.pch || do.lines))
    col <- rep_len(col, n.leg)
  
  ## NULL is not documented but people use it.
  if(missing(lwd) || is.null(lwd))
    lwd <- par("lwd") # = default for pt.lwd
  if (do.lines) {			#- draw lines ---------------------
    ## NULL is not documented
    if(missing(lty) || is.null(lty)) lty <- 1
    lty <- rep_len(lty, n.leg)
    lwd <- rep_len(lwd, n.leg)
    ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & !is.na(lwd)
    if(trace)
      catn("  segments2(",xt[ok.l] + x.off*xchar, ",", yt[ok.l],
           ", dx=", seg.len*xchar, ", dy=0, ...)")
    if(plot)
      segments2(xt[ok.l] + x.off*xchar, yt[ok.l], dx= seg.len*xchar, dy=0,
                lty = lty[ok.l], lwd = lwd[ok.l], col = col[ok.l])
    # if (!merge)
    xt <- xt + (seg.len+x.off) * xchar
  }
  if (has.pch) {			#- draw points -------------------
    pch   <- rep_len(pch, n.leg)
    pt.bg <- rep_len(pt.bg, n.leg)
    pt.cex<- rep_len(pt.cex, n.leg)
    pt.lwd<- rep_len(pt.lwd, n.leg)
    ok <- !is.na(pch) & (is.character(pch) | pch >= 0)
    x1 <- (if(merge && do.lines) xt-(seg.len/2)*xchar else xt)[ok]
    y1 <- yt[ok]
    if(trace)
      catn("  points2(", x1,",", y1,", pch=", pch[ok],", ...)")
    if(plot)
      points2(x1, y1, pch = pch[ok], col = col[ok],
              cex = pt.cex[ok], bg = pt.bg[ok], lwd = pt.lwd[ok])
    ##D	if (!merge) xt <- xt + dx.pch
  }
  
  xt <- xt + x.intersp * xchar
  if(plot) {
    if (!is.null(title))
      text2(left + w*title.adj, top - ymax, labels = title,
            adj = c(title.adj, 0), cex = cex, col = title.col)
    
    text2(xt, yt, labels = legend, adj = adj, cex = cex,
          col = text.col, font = text.font)
  }
  invisible(list(rect = list(w = w, h = h, left = left, top = top),
                 text = list(x = xt, y = yt)))
}

##################################
#  EXAMPLE FORMATS AND SCRIPTS   #
##################################

#Notes
  #Base R plot assumes that factor plots must be plotted using boxplots. See Sim_NetworkDist_14Nov2019.R for an exammple work around
  #Grouped bar charts assume a matrix data structure. See Sim_NetworkDist_14Nov2019.R for a nice example.
  #Repeated lines, plots, etc require loops. See Resolution Analyses_16July2019 for a nice example of 
  #Repeated drop lines (something you can't do in ggplot2).
  #The axTicks() command can be quite helful when trying to adjust text with reference to the axis ticks.
  #To replicate Base R points 21-25 in ggplot2, see http://sape.inf.usi.ch/quick-reference/ggplot2/shape.

  #ggplot2: When saving ggplots as images, use ggsave.

#BASE R PLOT PARAMETERS
  #   R Documentation on Parameters: https://www.rdocumentation.org/packages/graphics/versions/3.6.1
  #   ?par pulls up the pamater documentation in R.

#   marigns reference: https://www.r-graph-gallery.com/74-margin-and-oma-cheatsheet.html
#   mar – A numeric vector of length 4, which sets the margin sizes in the following order: 
#   bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1)

#   mgp – A numeric vector of length 3, which sets the axis label locations relative 
#   to the edge of the inner plot window. The first value represents the location the 
#   labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. 
#   The default is c(3, 1, 0).

#   mai -  Another way is by specifying the margins, in this case units are in inches

#   oma  - A numeric vector of length 3, which sets the outer margin sizes (bottom, left, top, right)

#   las – A numeric value indicating the orientation of the tick mark labels and any other text added to a plot 
#   after its initialization. The options are as follows: always parallel to the axis (the default, 0), 
#   always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

#PLOT STYLES

#Plot Style Example for ggplot2 plots

#p1 <- Free_Data %>>% ggplot() + 
#        theme(panel.background = element_rect(fill = 'white', linetype = 1),
#          strip.text.x = element_text(size = 11, face="bold"),
#         strip.text.y = element_text(size = 11, face="bold"),
#          strip.background = element_rect(colour='black', fill=NA),
#          panel.grid.minor = element_line(colour = 'snow3'),
#          plot.title = element_text(size = 14, face = "bold"), 
#          axis.text.x = element_text(colour="grey20",size=12),
#          axis.title.x = element_text(colour="grey20",size=14),
#          axis.text.y = element_text(colour="grey20",size=12, angle=90, hjust=0.5),
#          axis.title.y = element_text(colour="grey20",size=16),
#          text = element_text(family="Times"),
#          axis.ticks.length = unit(0.25, "cm")) +  
#  geom_violin(mapping = aes(x=Data, y=`Angular Similarity`), width= 1.5, fill='gray70') +
#  geom_boxplot(mapping = aes(x=Data, y=`Angular Similarity`), width=0.02, linetype = "dashed", outlier.shape = 1) +
#  stat_boxplot(mapping = aes(x=Data, y=`Angular Similarity`, ymin = ..lower.., ymax = ..upper..), width=0.02, outlier.shape = 1) +
#  stat_boxplot(geom = "errorbar", mapping = aes(x=Data, y=`Angular Similarity`, ymin = ..ymax..), width=0.02) +
#  stat_boxplot(geom = "errorbar", mapping = aes(x=Data, y=`Angular Similarity`, ymax = ..ymin..), width=0.02) +
#  geom_point(mapping = aes(x = Data, y =`Mean Angular Similarity`), size=3, shape=4) +
#  base_breaks_x_d(Data$Data) +
#  base_breaks_y(Data$`Angular Similarity`) +
#  labs(x = "Free CAMs") +
#  theme(plot.margin = unit(c(1,1,0,1), "cm"))

#p1

#PLOT UTILITIES 

#*******************************************   -Base R Continuous Color Scale-   ************************************************************#
#Specifying the color range
#colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))

#Example plot: Will need to use a matrix if additional subplots are needed.
#layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
#par(mar = c(3, 3, 2, 1))  
#plot(1:20, 1:20, pch = 19, cex=2, col = colfunc(20))

#Creating image raster based on the color ramp specified in the colfunc
#legend_image <- as.raster(matrix(colfunc(20), ncol=1))

#Specifying the legend: Adjustments are required with differnt margins (just make and axes=TRUE and adjust)
#par(mar = c(3, 0, 2, 0), xaxs = "i", yaxs = "i", family='serif')
#plot(c(0,2),c(0,1),type = 'n', axes = FALSE,xlab = '', ylab = '')
#title('Heat', line=-2.5, family='serif', font.main=2,  cex.main = 1.5, adj=0)

#text(x=1.25, y = seq(0.11,0.885,l=5), labels = seq(0,1,l=5))
#rasterImage(legend_image, 0, 0.10, 0.85, 0.90)

#***********************   -Discrete Legend Using the Legend Function (Specifies Legend Cell Sizes etc)-   ************************************#

#Creating Some Fake Data
#a<-c(1,1,2,3,3,3,3,4,54,56,2,23,1,3,23)

#Plotting
#cex <- 1
#hist(a)
#util$legend("topright",c(">0%",">20%",">40%",">60%",">80%"),
#       bty="n",
#       fill=c("black","gray50","gray70","gray85","white"),
#       box.cex=c(3,3),
#       y.intersp=2.8)

#********************************************    -Discrete Legend-   ***********************************************************************#

#Adding Legend
#xl <- 1
#yb <- 1
#xr <- 1.5
#yt <- 2

#par(mar=c(5,0.5,1,0.5))
#plot(NA,type="n", main="Total Distance", xlab = " ", ylab = " ", family = 'serif',
#    xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n", cex.main=0.9, adj = 0)

#rect(
#  xl,
#  head(seq(yb,yt,(yt-yb)/k),-1),
#  xr,
#  tail(seq(yb,yt,(yt-yb)/k),-1),
#  col=my.cols

#PLOT TYPES

#***************************************************   -Pie Chart-   *************************************************************************#

#Creating Data
#B <- c(5, 3, 1, 8, 9, 4, 6)

#Set up black, grey and white for clear printing.
#cols <- c("grey90","grey50","black","grey30","white","grey70","grey50")

#Calculate the percentage for each day, using one decimal place.
#percentlabels<- round(100*B/sum(B), 1)

#Add a ‘%’ sign to each percentage value using the paste command.
#pielabels<- paste(percentlabels, "%", sep="")

#What does the paste command do?
#pie(B, main="My Best Piechart", col=cols, labels=pielabels, cex=0.8)

#Create a legend at the right.
#legend("topright", c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"), cex=0.8, fill=cols)

#****************************************************   -Corrgram-   *************************************************************************#
#For more information about the Corrgram Package see: https://cran.r-project.org/web/packages/corrgram/vignettes/corrgram_examples.html
#Corrgram does support Pearson and Kendall Correlations, but it's good to check the Kendall's with Package Kendall to confirm which Tau is being used.

#library("knitr")
#library("corrgram")
#library(psych)

#opts_chunk$set(fig.align="center", fig.width=6, fig.height=6)
#options(width=90)

#Shaded Correlation Plot
#corrgram(vote, order=TRUE, upper.panel=panel.cor, main="Vote")

#Classic Correation Plot
#corrgram(iris, main="Iris data with example panel functions", lower.panel=panel.pts, upper.panel=panel.conf, diag.panel=panel.density)

#A Nice Alernative in Base R with the psych package :-)
#pairs.panels(iris[,-5], 
#             method = "pearson", # correlation method
#             hist.col = "#00AFBB",
#             density = TRUE,  # show density plots
#             ellipses = TRUE) # show correlation ellipses 

#*************************************************   -Scatter Plot-   ************************************************************************#
#Compendium of Clean Graphs (Very Nice Resource): http://shinyapps.org/apps/RGraphCompendium/index.php

# Presidential data up to and including 2008; data from Stulp et al. 2013

# Height of president divided by height of most successful opponent: 
#height.ratio <- c(0.924324324, 1.081871345, 1, 0.971098266, 1.029761905,
#                  0.935135135, 0.994252874, 0.908163265, 1.045714286, 1.18404908,
#                  1.115606936, 0.971910112, 0.97752809, 0.978609626, 1,
#                  0.933333333, 1.071428571, 0.944444444, 0.944444444, 1.017142857,
#                  1.011111111, 1.011235955, 1.011235955, 1.089285714, 0.988888889,
#                1.011111111, 1.032967033, 1.044444444, 1, 1.086705202,
#                  1.011560694, 1.005617978, 1.005617978, 1.005494505, 1.072222222,
#                  1.011111111, 0.983783784, 0.967213115, 1.04519774, 1.027777778,
#                  1.086705202, 1, 1.005347594, 0.983783784, 0.943005181, 1.057142857)

# Proportion popular vote for president vs most successful opponent
# NB can be lower than .5 because popolar vote does not decide election
#pop.vote <- c(0.427780852, 0.56148981, 0.597141922, 0.581254292, 0.530344067,
#              0.507425996, 0.526679292, 0.536690951, 0.577825976, 0.573225387,
#              0.550410082, 0.559380032, 0.484823958, 0.500466176, 0.502934212,
#              0.49569636, 0.516904414, 0.522050547, 0.531494442, 0.60014892, 
#              0.545079801, 0.604274986, 0.51635906, 0.63850958, 0.652184407, 
#              0.587920412, 0.5914898, 0.624614752, 0.550040193, 0.537771958, 
#              0.523673642, 0.554517134, 0.577511576, 0.500856251, 0.613444534, 
#              0.504063153, 0.617883695, 0.51049949, 0.553073235, 0.59166415, 
#              0.538982024, 0.53455133, 0.547304058, 0.497350649, 0.512424242, 
#              0.536914796)

# cor.test(height.ratio,pop.vote)
#library(plotrix) # package plotrix is needed for function "ablineclip""

#op <- par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5 , font.lab = 2, cex.axis = 1.3, bty = "n", las = 1)
#plot(height.ratio, pop.vote, col = "black", pch = 21, bg = "grey", cex = 2,
#     xlim = c(.90,1.20), ylim = c(.40,.70), ylab = "", xlab = "", axes = FALSE)
#axis(1)
#axis(2) 
#reg1 <- lm(pop.vote ~ height.ratio)
#ablineclip(reg1, lwd = 2,x1 = .9, x2 = 1.2) 
#par(las = 0)
#mtext("Presidential Height Ratio", side = 1, line = 2.5, cex = 1.5)
#mtext("Relative Support for President", side = 2, line = 3.7, cex = 1.5)
#text(1.15, .65, "r = .39", cex = 1.5)

#**********************************************   -Tufte Time Series Data-   ***********************************************************************#
#Brad Boehmke
#http://rpubs.com/bradleyboehmke/weather_graphic
#January 2, 2015

#library(dplyr)
#library(tidyr)
#library(readr)

#Importing Data 
#Dayton_Weather <- read_csv("Desktop/FHP Micro Projects/R Plot Utilities Data/Dayton_Weather.csv")

#Formatting Data
#Past <- Dayton_Weather %>%
#  group_by(Year, Month) %>%
#  arrange(Day) %>%
#  ungroup() %>%
#  group_by(Year) %>%
#  mutate(newDay = seq(1, length(Day))) %>%   # label days as 1:365 (will represent x-axis)         
#  ungroup() %>%
#  filter(Temp != -99 & Year != 2014) %>%     # filter out missing data (identified with '-99' value) & current year data
#  group_by(newDay) %>%
#  mutate(upper = max(Temp), # identify max value for each day
#         lower = min(Temp), # identify min value for each day
#         avg = mean(Temp),  # calculate mean value for each day
#         se = sd(Temp)/sqrt(length(Temp))) %>%  # calculate standard error of mean
#  mutate(avg_upper = avg+(2.101*se),  # calculate 95% CI for mean
#         avg_lower = avg-(2.101*se)) %>%  # calculate 95% CI for mean
#  ungroup()

#Create dataframe that represents 2014
#Present <- Dayton_Weather %>%
#  group_by(Year, Month) %>%
#  arrange(Day) %>%
#  ungroup() %>%
#  group_by(Year) %>%
#  mutate(newDay = seq(1, length(Day))) %>%  # create matching x-axis as historical data
#  ungroup() %>%
#  filter(Temp != -99 & Year == 2014)  # filter out missing data & select current year data

#Create dataframe that represents the lowest temp for each day for the historical data
#PastLows <- Past %>%
#  group_by(newDay) %>%
#  summarise(Pastlow = min(Temp)) # identify lowest temp for each day from 1995-2013

#Create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
#PresentLows <- Present %>%
#  left_join(PastLows) %>%  # merge historical lows to current year low data
#  mutate(record = ifelse(Temp<Pastlow, "Y", "N")) %>% # identifies if current year was record low
#  filter(record == "Y")  # filter for days that represent current year record lows

#Create dataframe that represents the highest temp for each day for the historical data
#PastHighs <- Past %>%
#  group_by(newDay) %>%
#  summarise(Pasthigh = max(Temp))  # identify highest temp for each day from 1995-2013

#Create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
#PresentHighs <- Present %>%
#  left_join(PastHighs) %>%  # merge historical highs to current year low data
#  mutate(record = ifelse(Temp>Pasthigh, "Y", "N")) %>% # identifies if current year was record high
#  filter(record == "Y")  # filter for days that represent current year record highs

#Function to turn y-axis labels into degree formatted values
#dgr_fmt <- function(x, ...) {
#  parse(text = paste(x, "*degree", sep = ""))
#}

#Create y-axis variable
#a <- dgr_fmt(seq(-20,100, by=10))

#Create a small dataframe to represent legend symbol for 2014 Temperature
#legend_data <- data.frame(x=seq(175,182),y=rnorm(8,15,2))

#ggplot Implementation of Tufte's Weather Data Plot
  #Nice, but the graph is sensitive to size constraints of the view window.
  #A bit pixilated for my tastes: Trying background polygons might help.

#p <- ggplot(Past, aes(newDay, Temp)) +
#  theme(plot.background = element_blank(),
#        panel.grid.minor = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.border = element_blank(),
#        panel.background = element_blank(),
#        axis.ticks = element_blank(),
#        #axis.text = element_blank(),  
#        axis.title = element_blank()) +
#  geom_linerange(Past, mapping=aes(x=newDay, ymin=lower, ymax=upper), colour = "wheat2", alpha=.1) +
#  geom_linerange(Past, mapping=aes(x=newDay, ymin=avg_lower, ymax=avg_upper), colour = "wheat4") +
#  geom_line(Present, mapping=aes(x=newDay, y=Temp, group=1)) +
#  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1) +
#  geom_hline(yintercept = -20, colour = "white", linetype=1) +
#  geom_hline(yintercept = -10, colour = "white", linetype=1) +
#  geom_hline(yintercept = 0, colour = "white", linetype=1) +
#  geom_hline(yintercept = 10, colour = "white", linetype=1) +
#  geom_hline(yintercept = 20, colour = "white", linetype=1) +
#  geom_hline(yintercept = 30, colour = "white", linetype=1) +
#  geom_hline(yintercept = 40, colour = "white", linetype=1) +
#  geom_hline(yintercept = 50, colour = "white", linetype=1) +
#  geom_hline(yintercept = 60, colour = "white", linetype=1) +
#  geom_hline(yintercept = 70, colour = "white", linetype=1) +
#  geom_hline(yintercept = 80, colour = "white", linetype=1) +
#  geom_hline(yintercept = 90, colour = "white", linetype=1) +
#  geom_hline(yintercept = 100, colour = "white", linetype=1) +
#  geom_vline(xintercept = 31, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 59, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 90, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 120, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 151, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 181, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 212, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 243, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 273, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 304, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 334, colour = "wheat4", linetype=3, size=.5) +
#  geom_vline(xintercept = 365, colour = "wheat4", linetype=3, size=.5) +
#  coord_cartesian(ylim = c(-20,100)) +
#  scale_y_continuous(breaks = seq(-20,100, by=10), labels = a) +
#  scale_x_continuous(expand = c(0, 0), 
#                     breaks = c(15,45,75,105,135,165,195,228,258,288,320,350),
#                     labels = c("January", "February", "March", "April",
#                                "May", "June", "July", "August", "September",
#                                "October", "November", "December")) +
#  geom_point(data=PresentLows, aes(x=newDay, y=Temp), colour="blue3") +
#  geom_point(data=PresentHighs, aes(x=newDay, y=Temp), colour="firebrick3") +
#  ggtitle("Dayton's Weather in 2014") +
#  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20)) +
#  annotate("text", x = 19, y = 98, label = "Temperature", size=4, fontface="bold") +
#  annotate("text", x = 66, y = 93, 
#           label = "Data represents average daily temperatures. Accessible data dates back to", size=3, colour="gray30") +
#  annotate("text", x = 62, y = 89, 
#           label = "January 1, 1995. Data for 2014 is only available through December 16.", size=3, colour="gray30") +
#  annotate("text", x = 64, y = 85, 
#           label = "Average temperature for the year was 51.9° making 2014 the 9th coldest", size=3, colour="gray30") +
#  annotate("text", x = 18, y = 81, label = "year since 1995", size=3, colour="gray30") +
#  annotate("segment", x = 30, xend = 40, y = -5, yend = -10, colour = "blue3") +
#  annotate("text", x = 65, y = -10, label = "We had 35 days that were the", size=3, colour="blue3") +
#  annotate("text", x = 56, y = -14, label = "coldest since 1995", size=3, colour="blue3") +
#  annotate("segment", x = 302, xend = 307, y = 74, yend = 82, colour = "firebrick3") +
#  annotate("text", x = 333, y = 82, label = "We had 19 days that were the", size=3, colour="firebrick3") +
#  annotate("text", x = 324, y = 78, label = "hottest since 1995", size=3, colour="firebrick3") +
#  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
#  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
#  geom_line(data=legend_data, aes(x=x,y=y)) +
#  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
#  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
#  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
#  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
#  annotate("text", x = 162, y = 14.75, label = "2014 TEMPERATURE", size=2, colour="gray30") +
#  annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
#  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30") 

#print(p)

#Tufte Time Series Data: Base R Graphic


#*************************************************   -Grouped Bar Chart-   *************************************************************************#
#Data Processing Occurs in Sim_NetworkDist_14Nov2019
#library(dplyr)

#Creating Looping Variables and Data
#s_variables <- c("Prop_Comp", "Diameter")
#s_labels <- c('Largest Component Proportion', 'Diameter')
#y_axses <- vector('list', length(s_variables))
#y_axses[[1]] <- seq(0, 1, by=0.1)
#y_axses[[2]] <- seq(1, 10, by= 1)

#plots <- c('p_2', 'p_3')

#Specifying the breaks used for the figure
#stat <- Stats %>%
#  dplyr::filter(Stats$variables == s_variables[[i]])
#viz_data <- NetStats %>%
#  dplyr::select(Type, Network, Empirical, s_variables[[i]])

#viz_data <- dplyr::left_join(viz_data, stat, by=c("Type" = "Type", "Empirical" = "Empirical"))

#viz_se <- rev(stat$se)

#Creating matrix to generate bar chart
#viz_matrix <- matrix(ncol=2, nrow=2)
#rownames(viz_matrix) <- c('Simulated Networks', 'Empirical Networks')
#colnames(viz_matrix) <- c('Text CAMs', 'Free CAMs')

#viz_matrix[[1,1]] <- stat[4, 2]
#viz_matrix[[2,1]] <- stat[3,2]
#viz_matrix[[1,2]] <- stat[2,2]
#viz_matrix[[2,2]] <- stat[1,2]

#Plotting
#layout(rbind(1,2), heights=c(9,1))  # put dot plot in bottom 1/5th of the chart
#par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(3,5,3,1))
#barplot(viz_matrix,
#        beside = TRUE, col = c("gray50", "blue"), 
#        density=c(30, 40), angle= c(60),
#        las = 1, xlab = " ", ylab = "Mean", family = 'serif', cex.names=1.5,
#        cex.main = 1.5, axes = FALSE, ylim = c(0, (max(stat[[2]]) + max(viz_se))), cex.axis = 1.5, cex.lab=1.5)

#axis(2, y_axses[[i]], cex.axis = 1.5, family = 'serif')

#at1 <- c(2, 5)
#axis(1, at1, labels = F , cex.axis = 1.5, family = 'serif')

#Adding Error Bars
#x <- c(1.5, 4.5)
#y <- c(2.5, 5.5)

#plot.errbars <- plotsegraph(x, c(stat[[4,2]], stat[[2,2]]), 
#                            c(stat[[4,4]], stat[[2,4]]), 0.1, color = "black") 

#plot.errbars <- plotsegraph(y, c(stat[[3,2]], stat[[1,2]]), 
#                            c(stat[[3,4]], stat[[1,4]]), 0.1, color = "blue")  #0.1 = wiskwidth

#Adding Title
#title(s_labels[[i]], line=-0.25, family='serif', font.main=2,  cex.main = 2)

#Adding Legend
#par(mar=c(0, 5, 0, 0), family='serif')
#plot.new()
#legend('center','groups',c("Simulated Networks", "Empirical Networks"), pch=15, col=c('gray50', 'blue'), 
#       cex=1.5, bty ="n", ncol=2)

#***************************************************   -Stacked Bar Char-   ***********************************************************************#
#Generating Toy Data Matrix with Function
#counts <- table(mtcars$vs, mtcars$gear)

#proportions <- counts / rep(colSums(counts), each = nrow(counts))
#proportions <- round(proportions, 2L)

# Converting Table into Data.Frame for Convenience
#counts <- as.data.frame(unclass(counts))

#Creating Mid-Points (The Number of mid-points depends upon the number of rows in the matrix)  ---> Build Loop Later
#mids_1 <- (counts[1, ]/2)
#mids_2 <- (counts[2, ]/2 + counts[1, ])
#mids <- rbind(mids_1, mids_2)

#The Bar Plot Function Requires Either a Table or Matrix
#mids <- as.matrix(mids)
#counts <- as.matrix(counts)

#Plotting
#par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(3,5,4,1), mgp = c(2.5, 0.8, 0.1))
#x <- barplot(counts, col=c("gray","dodgerblue"), density=c(30, 40), las = 1, xlab = " ", 
#             ylab = "Frequency", family = 'serif', cex.names=1.5, cex.axis = 1.5, cex.lab=1.5)

#text(rep(x, each = nrow(mids)), mids, labels = paste0(100 * proportions, "%"))

#mtext(side=1, line = 2, cex=1.5, 'Gear', family = 'serif')

#at1 <- c(0.70, 1.9, 3.1)
#axis(1, at1, labels = F , cex.axis = 2, family = 'serif')

#Adding Title
#mtext(side=3, 'Car Distribution by Gears and Steering', line=1, family='serif', font=2,  cex=2)

# Add a legend
#par(family='serif')
#legend('topright', legend=c('Standard', 'Variable'), pch=15, cex=1.25, col=c('gray','dodgerblue'), bty='n')

#rm(mids, mids_1, mids_2, at1, proportions, x)

#*************************************************   -Cleveland Dot Plots-   **********************************************************************#
#Cleveland, W. S., & McGill, R. (1984). Graphical perception: Theory, experimentation, and application to the development of graphical methods. 
#   Journal of the American statistical association, 79(387), 531-554.

#library(readxl)         # for reading in Excel data
#library(dplyr)          # for data manipulation
#library(tidyr)          # for data shaping

#Getting Data
#supermarket <- read_excel("~/Desktop/FHP Micro Projects/R Plot Utilities Data/Supermarket Transactions.xlsx", sheet = "Data")

#Constructing Data
#city_rev <- supermarket %>%
#  group_by(City) %>%
#  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
#  arrange(Revenue) %>%
#  mutate(City = factor(City, levels = .$City))

#city_gender_rev <- supermarket %>%
#  group_by(City, Gender) %>%
#  summarise(Revenue = sum(Revenue, na.rm = TRUE)) %>%
#  ungroup() %>%
#  mutate(City = factor(City, levels = city_rev$City))

#Create data frame that identifies revenue differences over 20%
#big_diff <- city_gender_rev %>% 
#  spread(Gender, Revenue) %>% 
#  group_by(City) %>% 
#  mutate(Max = max(F, M),
#         Min = min(F, M),
#         Diff = Max / Min - 1) %>% 
#  arrange(desc(Diff)) %>%
#  filter(Diff > .2)

#Creating Labels
#right_label <- city_gender_rev %>%
#  group_by(City) %>%
#  arrange(desc(Revenue)) %>%
#  top_n(1)

#left_label <- city_gender_rev %>%
#  group_by(City) %>%
#  arrange(desc(Revenue)) %>%
#  slice(2)

#Filter the label data frames to only include those cities where the difference exceeds 20%
#right_label <- dplyr::filter(right_label, City %in% big_diff$City)
#left_label <- filter(left_label, City %in% big_diff$City)

#Filter the main data frame to only include those cities where the difference exceeds 20%.
#highlight <- filter(city_gender_rev, City %in% big_diff$City)

# create a new label data frame
#plot_label <- big_diff %>%
#  select(City, Revenue = Max, Diff) %>%
#  right_join(right_label)

#Cleveland Dot Plot: ggplot 2
#library(ggplot2)        # for generating the visualizations

#p <- ggplot(city_gender_rev, aes(Revenue, City)) +
#  theme_minimal() +
#  theme(axis.title = element_blank(),
#        panel.grid.major.x = element_blank(),
#        panel.grid.minor = element_blank(),
#        axis.text.x = element_text(colour="black",size=12),
#        axis.text.y = element_text(colour="black",size=12),
#        legend.title = element_blank(),
#        legend.justification = c(0, 1), 
#        legend.position = c(.1, 1.075),
#        legend.background = element_blank(),
#        legend.direction="horizontal",
#        text = element_text(family = "Georgia"),
#        plot.title = element_text(size = 20, margin = margin(b = 10)),
#        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)),
#        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0)) +
#  geom_line(aes(group = City), alpha = .3) +
#  geom_point(aes(color = Gender), size = 2, alpha = .3) +
#  geom_line(data = highlight, aes(group = City)) +
#  geom_point(data = highlight, aes(color = Gender), size = 2) +
#  scale_color_manual(labels = c("Female", "Male"), values=c("brown", "blue")) +
#  scale_x_continuous(labels = scales::dollar, expand = c(0.02, 0), 
#                     limits = c(0, 10500),
#                     breaks = seq(0, 10000, by = 2500)) +
#  scale_y_discrete(expand = c(.02, 0)) +
#  labs(title = "Total Revenue by City and Gender",
#       subtitle = "Out of 23 cities, eight locations experience a 20% or greater difference \nin revenue generated by males versus females. Hidalgo experiences the \ngreatest difference with females generating 86% more revenue than males.") +
#  geom_text(data = plot_label, aes(color = Gender, 
#  label = paste0("+", scales::percent(round(Diff, 2)))), size = 3, hjust = -.5)

#p

#Cleveland Dot Plot: Base R Graphics

#*************************************************   -Shaded Density Plot-   **********************************************************************#
#Data Generated by ACT_Cosine Function_30Aug2019

#Creating plot data

#Getting Kernel Density
#viz_den <- density(similarity[[i]][[15]])

#5th Percentile Polygon Range
#poly_range_5 <- density(similarity[[i]][[15]])$x > 0 & 
#  density(similarity[[i]][[15]])$x < descriptives[[i]]$`5th percentile`       

#95 Percentile Polygon Range
#poly_range_95 <- density(similarity[[i]][[15]])$x >= descriptives[[i]]$`95th percentile` & 
#  density(similarity[[i]][[15]])$x < max(density(similarity[[i]][[15]])$x)

#Defining Background and Dimensions
#par(bty="n", mar=c(5,5,1,5))
#plot(viz_den, xlim=c(0, 1), lwd = 2.3, main=" ", xlab = "Total Distance", ylab = "Density", family = 'serif',
#     cex.lab=1.5,  xaxt = 'n')

#Adding Shading for the 5th percentile
#polygon(c(0, density(similarity[[i]][[15]])$x[poly_range_5], descriptives[[i]]$`5th percentile`),               
#        c(0, density(similarity[[i]][[15]])$y[poly_range_5], 0),                 
#        col = "gray")

#Adding Shading for the 95th percentile
#polygon(c(descriptives[[i]]$`95th percentile`, density(similarity[[i]][[15]])$x[poly_range_95], descriptives[[i]]$`95th percentile`),               
#        c(0, density(similarity[[i]][[15]])$y[poly_range_95], 0),                 
#        col = "gray")  

#Adding Median Line
#abline(v = descriptives[[i]]$median, col = "dodgerblue4", lwd = 2)

#Adding Mean Line
#abline(v = descriptives[[i]]$mean, col = "dodgerblue4", lty = 2, lwd = 2)

#Adding Labels 
#Md.expression <- expression(paste(italic(Md)))

#text(descriptives[[i]]$median, (max(density(similarity[[i]][[15]])$y) - 0.5), Md.expression, cex = 1.5, pos = 2)

#text(descriptives[[i]]$mean, (max(density(similarity[[i]][[15]])$y) - 0.5), expression(bar(x)), cex = 1.5, pos = 4)

#Specifying Axses
#axis(side=1,at=c(0, 0.1, 0.2, 0.3, 0.4, .5, 0.6, 0.7, 0.8, 0.9, 1), 
#     labels=c("0", "0.1", "0.2", "0.3", '0.4', "0.5", "0.6", "0.7", "0.8", "0.9", "1"), font = 2, family = 'serif')

#axis(side=2, font = 2, family = 'serif')

#Adding legend
#source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
#legend(x=0.8, y=max(density(similarity[[i]][[15]])$y), legend=c("5th and 95th Percentiles"), fill=c("gray"), 
#       cex=1.3, box.cex=c(0.7,0.7), bty="n", x.intersp=0.2, xjust=0.2, xpd=TRUE)

#Adding Title
#title(titles[[i]], adj=0, family='serif')

#*************************************************   -Function Plot (Convex Hull)-   **************************************************************#
#library(readr)
#library(RColorBrewer)

#Retrieving Data
#`Optimal Modularity` <- readr::read_csv('~/GPM_Diss/Time1_Structure/E_mail Network_t1_Resolution Data.csv')

#Ensuring the axses labels are not in scientific notation
#options(scipen=10000)

#Creating Visualization Components
#zero <- c(0)
#labels <- as.character(`Optimal Modularity`$r)

#Plots elements for later
#colors <- c("#FF0000", "#FF4D00", "#FF6800", "#FFA200", "#F5BA00", "#E3D000", "#C3EE00", "#A7FF00", "#9CFF00", "#74FF00", "#6DFF00", "#1FFF4B", "#21FF55", 
#            "#39DAD1", "#00FFAA", "#37DFCB", "#37CAE0", "#3084FF","#327CFF", "#325AFF", "#2F51FF", "#6000FF", "#9A00FF", "#AC00FE", "#A600FF", "#FF00AA", "#EA00C4")

#palette <- brewer.pal("Greys", n=9)

#Plotting
#par(bty="n", bg=palette[1], mar=c(5,5,1,5))

#plot(`Optimal Modularity`$r, `Optimal Modularity`$Q, xlim=c(0, 1.8),
#     xlab = "Resolution Parameter Gamma", ylab = "Quality Score (non-normalized)", 
#     xaxt = 'n', yaxt = 'n', cex.lab=1.5, family = 'serif')

#Adding Slope Lines
#for (i in seq_along(`Optimal Modularity`$sumAij)){
#  abline(a=`Optimal Modularity`$sumAij[i], `Optimal Modularity`$slope[i], col = 'snow2')
#}

#Adding Droplines
#segments(`Optimal Modularity`$r, zero, `Optimal Modularity`$r, `Optimal Modularity`$Q)

#Adding Resolution Value Points
#lines(`Optimal Modularity`$r, `Optimal Modularity`$Q, type = "p", pch = 19, col=colors)

#Adding Labels
#text(`Optimal Modularity`$r, `Optimal Modularity`$Q, labels, cex=0.6, pos=3, col="black")

#Specifying Axses
#axis(side=1,at=c(0, 0.50, 0.75, 1.00, 1.25, 1.50, 1.8), labels=c("0", "0.5", "0.75", "1", '1.25', "1.5", "1.8"), font = 2, family = 'serif')
#axis(side=2, font = 2, family = 'serif')

#Adding Group Size
#par(new = T, mar = c(5,5,1,5), las=0)
#with(`Optimal Modularity`, plot(r, grpN, pch=4, axes=F, xlab=NA, ylab=NA, 
#                                cex=1.5, xlim=c(0, 1.8), col = 'darkolivegreen'))
#axis(side = 4, cex.axis = 1, font = 2, family = 'serif')
#mtext(side = 4, line = 3, cex=1.5, 'Number of Communities', family = 'serif')

#*************************************************   -Histogram with CDF-   ***********************************************************************#
#Creating Data
#set.seed(15)
#dt <- rnorm(500, 50, 10)

#ec <- ecdf(dt)

#Plotting Histogram and CDF
#par(mar = c(5,5,2,5))
#h <- hist(
#  dt,
#  breaks = seq(0, 100, 1),
#  xlim = c(0,100))

#par(new = T)
#plot(x = h$mids, y=ec(h$mids)*max(h$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
#lines(x = h$mids, y=ec(h$mids)*max(h$counts), col ='red')
#axis(4, at=seq(from = 0, to = max(h$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
#mtext(side = 4, line = 3, 'Cumulative Density', col = 'red')

#****************************************   -Stylized Cumulative Density Plot-   ***********************************************************#
#This plot was designed to compare two long-tailed distributions of differing sample sizes.
#The plot shows both the denstities and cmulative densities of each distribution. 
#This plot assumes the densities for both the distribution and CDF have been normalized to range between 0 and 1.

#Generating some fake data
#e_free <- (rexp(100))
#s_free <- (rexp(1000))

#Creating CDF Plot Elements
#e_free_ec <- ecdf(e_free)
#e_free_y <- e_free_ec(sort(e_free ))
#e_free_x <- sort(e_free)

#s_free_ec <- ecdf(s_free) 
#s_free_y <- s_free_ec(sort(s_free ))
#s_free_x <- sort(s_free )

#Creating Density Plot Elments
#viz_den_1 <- density(s_free, from=0, to=max(s_free))
#viz_den_1$x <- ifelse(viz_den_1$x  < 0.001, 0, viz_den_1$x)

#viz_den_2 <- density(e_free, from=0, to=max(e_free))
#viz_den_2$x <- ifelse(viz_den_2$x  < 0.001, 0, viz_den_2$x)

#Adding Reference Lines
#s_max <- max(viz_den_1$x)
#e_max <- max(viz_den_2$x)

#Identifying Modes
#s_indices <- find_modes(viz_den_1$y) #you need to try it on the y axis
#s_modes <- viz_den_1$x[s_indices]

#e_indices <- find_modes(viz_den_2$y) #you need to try it on the y axis
#e_modes <- viz_den_2$x[e_indices]

#Plotting
#png("p_1.png", width = 781, height = 680)
#  layout(rbind(1,2), heights=c(8,1)) 
#  par(mar = c(4.1, 4.1, 4.1, 2.1), bty='n', family='serif')
#  plot(viz_den_1$x, viz_den_1$y, lwd = 2.3, main=" ", xlab = " ", ylab = " ", family = 'serif', type='n',
#     col = 'black',  cex.lab=1.5, xaxt = 'n', axes=FALSE)

#  lines(viz_den_1$x, viz_den_1$y, lwd = 2.3, col = 'black')
#  lines(viz_den_2$x, viz_den_2$y, lwd = 2.3, col = 'blue')

#  par(new = TRUE, mar = c(4.1, 4.1, 4.1, 2.1), bty='n', family='serif')
#  plot(ecdf(s_free), verticals=T, do.points = FALSE, col='black', 
#     cex.lab=1.5, cex.axis=1.5, lwd=1.3, main=' ', xlab=' ', ylab= ' ', xlim=c(0, max(s_free)))

#  points(s_free_x, s_free_y, pch=21, col='black', bg='grey')

#  lines(ecdf(e_free),verticals=T, do.points = FALSE, col='blue', cex.lab=1.5, cex.axis=1.5)
#  points(e_free_x, e_free_y, pch=21, col='blue', bg='grey')

#  lines(x=c(s_max, s_max), y=c(1, 0), col = 'black', lty = 2, lwd = 1.2)
#  lines(x=c(e_max, e_max), y=c(1, 0), col = 'blue', lty = 2, lwd = 1.2)
#  lines(x=c(e_modes[[1]], e_modes[[1]]), y=c(max(viz_den_1$y), 0), col = 'blue', lty = 2, lwd = 1.2)

#  mtext(side = 1, line = 2.6, 'Angular Similarity', cex=1.5)
#  mtext(side = 2, line = 2.5, 'Cumlative Density', cex=1.5)

  #Adding Title
#  title('Text CAMs', family='serif', cex.main=1.5)

  #Adding legend
#  par(mar=c(0, 0, 0, 0))
#  plot.new()
#  legend('center','groups',c("Empirical Networks", "Simulated Networks"), 
#       lty=c(1, 1), col=c('blue', 'black'), lwd=c(2, 2), pch=c(21, 21), pt.bg=c('grey', 'grey'), ncol=2, cex=1.3,
#       bty ="n", text.font = 2)
#dev.off()

#rm(viz_den_1, viz_den_2, e_free, e_free_ec, e_free_x, e_free_y, e_indices, e_modes, s_free, s_free_ec, s_free_x, s_free_y, s_indices,s_modes, e_max, s_max)

#**********************************   -Kolmogorov-Smirnov Plot (Test of Normality)-   ******************************************************#

#A quick R example of plotting the ECDF curves of the Kolmogorov-Smirnov test along with maximum distance (D) between distributions. 
#Matt Harris: https://rpubs.com/mharris/KSplot
#June 9, 2015

#Simulate two distributions - your data goes here!
#sample1 <- rnorm(10000, 10, 5)
#sample2 <- rnorm(10000, 1, 5)
#group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
#dat <- data.frame(KSD = c(sample1,sample2), group = group)

#Create ECDF of data
#cdf1 <- ecdf(sample1) 
#cdf2 <- ecdf(sample2) 

#Find min and max statistics to draw line between points of greatest distance
#minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1)) 
#x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )] 
#y0 <- cdf1(x0) 
#y1 <- cdf2(x0) 

#plot(cdf1, verticals=TRUE, do.points=FALSE, col="blue") 
#plot(cdf2, verticals=TRUE, do.points=FALSE, col="green", add=TRUE) 

## alternatine, use standard R plot of ecdf 
#plot(f.a, col="blue") 
#lines(f.b, col="green") 

#points(c(x0, x0), c(y0, y1), pch=16, col="red") 
#segments(x0, y0, x0, y1, col="red", lty="dotted") 

#****************************************   -Complementary Cumulative Distribution Function (CCDF)-   ********************************************#
#https://rpubs.com/mdlama/spring2017-lab6supp1
#Good Examples Plots Are Provided on this Site.

#The complementary cumulative distribution function G(x) corresponds to the right-tail of the probability density function, as can be seen from the following picture.

#Generating Data
#mydata <- read.csv("http://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q21FallingCatsByMonth.csv") %>%
#  tbl_df()
#(chi2 <- chisq.test(table(mydata), p = rep(1/12, 12)))

#Calculating Chi Squared
#chi2 <- chi2$statistic

#***************************************************   -Stylized Q-Q Plot-   *********************************************************************#

#library(qualityTools)

#Creating Fake Data
#e_free <- (rexp(100))
#s_free <- (rexp(1000))

#Plotting
#png("p_1.png", width = 781, height = 680)
#  layout(rbind(1,2), heights=c(8,1)) 
#  par(mar = c(3.1, 4.1, 4.1, 2.1), bty='n', family='serif')
#  p <- ppoints(length(s_free))
#  q <- quantile(s_free, p=p)

#  qqplot(x=q, y=qexp(p), main = '', plot.it = TRUE,  pch=21, col='black', bg='grey', bty='n', 
#       xlab = ' ', ylab = ' ', axes=FALSE)

#  qqline(q, distribution=qexp, datax=TRUE, col='black', lty=2, lwd=2)

#  par(new=TRUE)
#  qqPlot(e_free, "exponential", col = "blue", pch = 21, bg = "grey", cex.axis=1.5, main=' ', 
#       xlab= '', ylab= '', border='black', bounds.col='red', bounds.lty=2, bty='n')

  #Adding Gridline
#  grid(lwd = 2)

  #Adding Axses Labels
#  mtext("Observed Quantiles", side = 1, line = 2.5, cex = 1.5)
#  mtext("Theoretical Quantiles", side = 2, line = 2.5, cex = 1.5)

  #Adding Title
#  title('Text CAMs Exponential Q-Q Plot', family='serif', line=-0.02, cex.main=1.5)

#  legend(x=0, y=6, c('Empirical', 'Simulated'), pch=21, cex=1.5, pt.bg=c('grey', 'grey'), col=c('blue','black'), bty='n')

  #Adding legend
#  par(mar=c(0, 0, 0, 0))
#  plot.new()
#  legend('center','groups',c("Empirical Reference Line", "Simulated Reference Line", "Empirical Confidence Interval"), 
#       lty=c(1, 2, 2), col=c('black', 'black', 'red'), lwd=c(2, 2, 2), ncol=3,  cex=1.3,
#       bty ="n", text.font = 2)
#dev.off()

#**********************************************   -Split Violin Plot Example-   ******************************************************************#

#Function Description: https://rdrr.io/github/AsexGenomeEvol/AsexStats/man/vioplot2.html
#There are features you can customize such as the fill of the boxplots and their orientation.

#require(vioplot)
#require(devtools)
#require(digest)
#source_gist("https://gist.github.com/mbjoseph/5852613")
#plot(x=NULL, y=NULL,
#     xlim = c(0.5, 2.5), ylim=c(min(values), max(values)),
#     type="n", ann=FALSE, axes=F)
#axis(1, at=c(1, 2),  labels=c("A", "B"))
#axis(2)
#for (i in unique(treatment)) {
#  for (j in unique(group)){
#    vioplot2(values[which(treatment == i & group == j)],
#             at = ifelse(i == "A", 1, 2),
#             side = ifelse(j == 1, "left", "right"),
#             col = ifelse(j == 1, "purple", "lightblue"),
#             add = T)
#  }
#}

#title("Violin plot", xlab="Treatment")
#legend("bottomright", fill = c("purple", "lightblue"),
#       legend = c("Group 1", "Group 2"), box.lt)

#*****************************************************  -Pirate Plot-   *********************************************************************#
#library(devtools)
#install_github("ndphillips/yarrr")
#library("yarrr")

#pirateplot(weight ~ Diet,
#           data = ChickWeight,
#           pal = "info", theme = 3)

#***************************************************   -Beeswarm Plot-   **********************************************************************#
#Data Processing Occurs in Sim_NetworkDist_14Nov2019
#library(beeswarm)     #Produces Beeswarm style plots
#library(dplyr)

#stat <- subset(Stats, variables == variables[[i]])
#viz_data <- NetStats[c(variables[[i]], 'Type', 'Empirical', 'Network')]
#viz_data <- dplyr::left_join(viz_data, stat, by=c("Type" = "Type", "Empirical" = "Empirical"))

#Empirical_Stats <- viz_data %>%
#  dplyr::filter(Empirical == 1)

#Vivualization
#layout(rbind(2,1), heights=c(8,2))  # put dot plot in top 1/4th of the chart
#par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(2.25,5,0,1))
#plot(0, xlim=c(0, 1), ylim=c(min(stat$uci), max(stat$lci)), family = 'serif',
#     type="n", xaxt='n', ylab = c('Mean'))
#axis(1, at = c(0.25, 0.75), labels = c("Text CAMs", "Free CAMs"), family = 'serif', cex.axis=1.5)

#Adding Points and error bars
#x <- c(0.25, 0.75)

#Empirical
#points(x, c(stat[[3,2]], stat[[1,2]]), cex = 1.5, lwd = 2, pch = 16, col='blue')
#plot.errbars <- plotsegraph(x, c(stat[[3,2]], stat[[1,2]]), 
#                            c(stat[[3,4]], stat[[1,4]]), 0.03, color = "blue")  #0.1 = wiskwidth

#Simulation
#points(x, c(stat[[4,2]], stat[[2,2]]), cex = 1.5, lwd = 2, pch = 16, col='gray50')
#plot.errbars <- plotsegraph(x, c(stat[[4,2]], stat[[2,2]]), 
#                            c(stat[[4,4]], stat[[2,4]]), 0.03, color = "gray50") 

#par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(1,5,2,1))
#beeswarm(viz_data[[1]] ~ Type, data = viz_data,
#         method = 'swarm', corral = 'wrap',
#         pch = 21, col = viz_data$Type, bg = "#00000050",
#         xlab = '', ylab = labels[[i]], cex.lab=1.5, family = 'serif', xaxt='n')

#Adding Title
#title(labels[[i]], line=-0.25, family='serif', font.main=2,  cex.main = 2)

#Adding Legend
#op <- par(family = "serif")
#legend('topleft','groups',c("Simulated Networks", "Empirical Networks"), pch=16, col=c('gray50', 'blue'), 
#       cex=1.25, bty ="n")

#Adding Empirical Bee Swarm
#par(new = T)
#with(Empirical_Stats, beeswarm(Empirical_Stats[[1]] ~ Type, data = Empirical_Stats,
#                               method = 'swarm',
#                               pch = 16, col = c(4), cex=1.5, bg = "#00000050", family = 'serif',
#                               xlab = '', ylab = ' ', cex.lab=1.5, xaxt='n', yaxt='n', ann=FALSE))

#****************************************************   -Fourfold Plot-   *********************************************************************#
#Fourfold plots model realtionships in a contingency table.

#Creating Data
#data1 <- list(structure(c(159076L, 5858L, 7285L, 23571L), .Dim = c(2L, 2L), 
#                          .Dimnames = list(Prediction = c("O3<80", "O3>80"), 
#                          Reference = c("O3<80", "O3>80")), class = "table"), 
#              structure(c(159385L, 5549L, 6679L, 24177L), .Dim = c(2L, 2L), 
#                          .Dimnames = list(Prediction = c("O3<80", "O3>80"), 
#                          Reference = c("O3<80", "O3>80")), class = "table"), 
#              structure(c(159273L, 5661L, 8985L, 21871L), .Dim = c(2L, 2L), 
#                          .Dimnames = list(Prediction = c("O3<80", "O3>80"), 
#                          Reference = c("O3<80", "O3>80")), class = "table"), 
#              structure(c(159250L, 5684L, 8486L, 22370L), .Dim = c(2L, 2L), 
#                          .Dimnames = list(Prediction = c("O3<80", "O3>80"), 
#                          Reference = c("O3<80", "O3>80")), class = "table"))

#Plotting Fourfold Pot
#par(family='serif', mfrow=c(2,2))
#for(pltnum in 1:length(data1)) {
#  fourfoldplot(data1[[pltnum]], color = c("brown", "lightblue"), main = "") + 
#    text(-0.4,0.4, "TN", cex=1) + 
#    text(0.4, -0.4, "TP", cex=1) + 
#    text(0.4,0.4, "FN", cex=1) + 
#    text(-0.4, -0.4, "FP", cex=1)
#}

#*********************************************   -Conditional Density Panel Plot-   ***********************************************************#
#Arthur Charpentier: https://www.r-bloggers.com/conditional-densities-on-one-single-graph/
#December 5, 2013

#Lodaing and Preparing Data
#load("credit.Rda")
#myVariableNames <- c("checking_status","duration","credit_history", "purpose","credit_amount","savings","employment","installment_rate",
#                      "personal_status","other_parties","residence_since","property_magnitude", "age","other_payment_plans","housing","existing_credits",
#                     "job", "num_dependents","telephone","foreign_worker","class")

#Preparing Plot Elements
#varQuanti = function(base,y,x) {
#    library(RColorBrewer)
#    CL=brewer.pal(6, "RdBu")
    
#   layout(matrix(c(1, 2), 2, 1, byrow = TRUE),heights=c(3, 1))
#   par(mar = c(2, 4, 2, 1), bty='n')
#    base0 <- base[base[,y]==0,]
#    base1 <- base[base[,y]==1,]
#    xlim1 <- range(c(base0[ ,x], base1[,x]))
#    ylim1 <- c(0, max( max(density(base0[,x])$y) , max( density(base1[,x])$y )))
#    plot(density(base0[,x]),main=" ",col=CL[1],ylab=paste("Density of ",x), xlim = xlim1, ylim = ylim1 ,lwd=2, family='serif', bty= 'n')
    
#    par(new = TRUE, family='serif')
#    plot(density(base1[,x]),col=CL[6],lty=1,lwd=2, xlim = xlim1, ylim = ylim1,xlab = '', ylab = '',main=' ')
#    legend("topright",c(paste(y," = 0"),paste(y," = 1")), lty=1, col= c(CL[1],CL[6]), lwd=2, bty='n')
#    texte <- c("Kruskal-Wallis'Chi² = \n\n", round(kruskal.test(base[,x]~base[,y])$statistic*1000)/1000)
#    text(xlim1[2]*0.8, ylim1[2]*0.5, texte,cex=0.85)
#    boxplot(base[,x]~ base[,y], horizontal = TRUE, ylab= y, col= c(CL[1],CL[6]), famly='serif', bty='n')
#}

#varQuanti(credit,"class","duration")

#***************************************************   -Bubble Plot-   ************************************************************************#
#myDF <- readr::read_csv("BubblePlot_TestData.csv")

#***** Begin common/converttocolnums.r
# 20141222 three
#converttocolnums <- function(myDF, colindex) {
#  if (typeof(colindex) == "character") {
#    for (i in seq_along(colindex)) {
#      options(warn = -1)
#      if (is.na(as.double(colindex[i]))) {
#        options(warn = 0)
#      ind <- (which(names(myDF) %in% colindex[i]))
#        if (length(ind) > 0) {
#          colindex[i] <- ind
#        } else {
#          colindex[i] <- NA
#        }
#    } else {
#        options(warn = 0)
#        colindex[i] <- as.double(colindex[i])
#      }
#    }
#  }
#  return(as.integer(colindex))
#} 

#***** End common/converttocolnums.r

#***** Begin common/bubble.r
#Original2014-05-19
#' bubble 
#' 
#' @param  mydf 
#' @param  colindex 
#' @param  freqs 
#' 
#' @export
#' @examples

#bubble <- function(mydf, colindex=c(dosecol='Dose', daycol='Day', ncol='ID'), freqs=TRUE)
#{colindex <- converttocolnums(mydf, colindex)
#dosecol <- colindex[1]
#daycol <- colindex[2]

#dfnames <- names(mydf)
#if(!freqs) #Need to create df with frequencies
#{
#  mydf <- createdosebydaydf(mydf, dose=dfnames[dosecol], day=dfnames[daycol])
#  title <- "Bubble Chart of Dose versus Exposure Time"
#  ncol <- 3
#}
#else #ALready have dataframe of frequencies
#{
#  ncol <- colindex[3]
#  title <- "Bubble Chart of Dose versus Exposure Time freqs"
#}
#dose<-mydf[[dosecol]]
#day<- mydf[[daycol]]
#n<-mydf[[ncol]]

#calc squareroot of n for use as size 
#size <- sqrt(n)

#Get user names for dose and day
#dosename<- colnames(mydf)[dosecol]
#dayname<- colnames(mydf)[daycol]
#numbreaks <- 2
#myinches=.25
#myhist <- hist(n, breaks=numbreaks, plot=FALSE )
#myhist$breaks[length(myhist$breaks)]<-max(n)
#myhist$breaks[1]<-min(n)

#Graph stuff
#Create place for legend and plot
#layout(matrix(c(2,1),2,1), heights=c(10,2))

#Plot legend in bottome plot area
#par( mar=c(1,2,0,0.1))
#symbols(max(day)*c(.15,.3,.6, .9), rep(max(dose)/4,numbreaks+2), 
#        circles = sqrt(myhist$breaks), 
#        inches=myinches, fg = 'blue', bg="#ff00ff22", xaxt='n', yaxt='n',xlim=c(0,max(day)), ylim=c(0,max(dose)))


#text( max(day)*c(.15,.32,.62, .92), rep( (max(dose)/4),(numbreaks+3)),c(myhist$breaks), pos=4, cex=.9 )
#text( min(day) + (max(day)-min(day))/2, max(dose)/2,c('Sample Size'), font=2, pos=3, cex=.9 )

#Plot values
#par( mar=c(4,4,2,0.1))
#symbols(day, dose, circles = size, inches=myinches, fg = 'blue', bg="#ff00ff22", xlim=c(0,max(day)),
#        main = title, xlab=dayname, ylab=dosename)
#}

#createdosebydaydf <- function(mydf, dose='Dose', day='Day')
#{
#  mytable<-table(mydf[[dose]], mydf[[day]], dnn=c(dose,day))
#  mytablemargins<-addmargins(mytable)
#  print(mytablemargins)
#  tableDF <- as.data.frame(mytable)
#  names(tableDF) <- c(dose, day, 'N')
  
#  tableDF[[1]] <- as.integer(as.character(tableDF[[dose]]) ) 
#  tableDF[[2]] <- as.integer(as.character(tableDF[[day]]) )
#  return (tableDF)
#}
#***** End bubble/bubbledata.R

#suppressPackageStartupMessages(library(Cairo))
#CairoPNG(file="/tmp/tmpk9uomo3_.png",width = 500, height = 500)

#***** Begin bubble/bubblegraph.r
#bubble(myDF, c('Dose', 'Day', 'ID'), freqs=FALSE)

#***** End bubble/bubblegraph.r
#invisible(dev.off())
#q()
#************************************************   -ggplot Panel Plot Style-   ***************************************************************#
#Getting Data
#setwd("/Users/jhmorgan/Desktop/Bias_Project/Final Figures") 
#getwd()

#Formatting Steps Occurr in: Logged Networks_2June2019.R

#Plotting
#f1 <- ggplot(Directed_Networks[Directed_Networks$net.name == "HighSc p13", ], aes(x=missing.percentage, y=Bias, alpha=imputation.type, size=imputation.type)) +
#        theme(panel.background = element_rect(fill = 'white', linetype = 1),
#          strip.text.x = element_text(size = 11, face="bold"),
#         strip.text.y = element_text(size = 11, face="bold"),
#          strip.background = element_rect(colour='black', fill=NA),
#          panel.grid.minor = element_line(colour = 'snow3', linetype="dashed", size=0.5),
#          plot.title = element_text(size = 14, face = "bold"), 
#          axis.text.x = element_text(colour="black",size=12),
#          axis.title.x = element_text(colour="black",size=14),
#          axis.text.y = element_text(colour="black",size=12, angle=90, hjust=0.5),
#          axis.title.y = element_text(colour="black",size=16),
#          text = element_text(family="Times"),
#          axis.ticks.length = unit(0.25, "cm")) +  
#  geom_line(aes(color=imputation.type)) +
#  scale_alpha_manual(values = c(0.3, 0.5, 0.4, 0.4, 0.4, 0.5), guide = F) +
#  scale_size_manual(values = c(1.5, 1.25, 1.50, 1.25, 1.25, 1.25), guide = F) +
#  facet_grid(missing.datacor ~ Measure) +
#  base_breaks_x(Directed_Networks[Directed_Networks$net.name == "HighSc p13", ]$missing.percentage,
#                l=c("1%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")) +
#  base_breaks_y(Directed_Networks[Directed_Networks$net.name == "HighSc p13", ]$Bias) +
#  scale_color_manual(values=c("olivedrab3", "plum", "goldenrod2", "burlywood4", "dodgerblue2", "cyan2")) +
#  labs(y = "Bias", x = "Percentage Missing") +
#  guides(color=guide_legend(title="Imputation Method")) +
#  ggtitle("Large, Directed, and Moderately Centralized") +
#  theme(plot.margin = unit(c(1,1,0,1), "cm"))

#f1

#***********************************************   -Example Panel Plot Base R-   **************************************************************# 
#library(dplyr)

##Data
#load(url("http://varianceexplained.org/files/ggplot2_example.rda"))
#top_data <- cleaned_data %>%
#  semi_join(top_intercept, by = "systematic_name")
#
#top_data$combined <- paste(top_data$name, top_data$systematic_name)

#par(mar = c(1.5, 1.5, 1.5, 1.5))
#colors <- 1:6
#names(colors) <- unique(top_data$nutrient)

# legend approach from http://stackoverflow.com/a/10391001/712603
#m <- matrix(c(1:20, 21, 21, 21, 21), nrow = 6, ncol = 4, byrow = TRUE)
#layout(mat = m, heights = c(.18, .18, .18, .18, .18, .1))

#for (gene in unique(top_data$combined)) {
#  sub_data <- filter(top_data, combined == gene)
#  plot(expression ~ rate, sub_data, col = colors[sub_data$nutrient], main = gene)
#  for (n in unique(sub_data$nutrient)) {
#    m <- lm(expression ~ rate, filter(sub_data, nutrient == n))
#    if (!is.na(m$coefficients[2])) {
#      abline(m, col = colors[n])
#    }
#  }
#}

# create a new plot for legend
#plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
#legend("top", names(colors), col = colors, horiz = TRUE, lwd = 4)

#*******************************   -Jitter Example: Dealing with Overplotting with Jitter and Alpha-   ***********************************************#

#iris$Sepal.Length = jitter(iris$Sepal.Length)
#iris$Sepal.Width = jitter(iris$Sepal.Width)

#alpha = 150 # Transparent points
#palette(c(rgb(200, 79, 178, alpha = alpha, maxColorValue = 255), 
#          rgb(105, 147, 45, alpha = alpha, maxColorValue = 255),
#          rgb(85, 130, 169, alpha = alpha, maxColorValue = 255)))

#par(mar = c(3, 3, 2, 1), # Dist' from plot to side of page
#    mgp = c(2, 0.4, 0), # Dist' plot to label
#    las = 1, # Rotate y-axis text
#    tck = -.01, # Reduce tick length
#    xaxs = "i", yaxs = "i") # Remove plot padding

#plot(iris$Sepal.Length, iris$Sepal.Width, 
#     bg = iris$Species, # Fill colour
#     pch = 21, # Shape: circles that can filed
#     xlab = "Sepal Length", ylab = "Sepal Width", # Labels
#     axes = FALSE, # Don't plot the axes
#     frame.plot = FALSE, # Remove the frame 
#     xlim = c(4, 8), ylim = c(2, 4.5), # Limits
#     panel.first = abline(h = seq(2, 4.5, 0.5), col = "grey80"))

#Adding in the x-axis labels with no tick marks
#at = pretty(iris$Sepal.Length)
#mtext(side = 1, text = at, at = at, 
#      col = "grey20", line = 1, cex = 0.9)

#Adding the y-axis
#at = pretty(iris$Sepal.Width)
#mtext(side = 2, text = at, at = at, col = "grey20", line = 1, cex = 0.9)

#Placing Text rather than use a legend (nice for smaller datasets)
#text(5, 4.2, "setosa", col = rgb(200, 79, 178, maxColorValue = 255))
#text(5.3, 2.1, "versicolor", col = rgb(105, 147, 45, maxColorValue = 255))
#text(7, 3.7, "virginica", col = rgb(85, 130, 169, maxColorValue = 255))

#Adding Title
#title("IRIS", adj = 1, 
#      cex.main = 0.8, font.main = 2, col.main = "black")

#******************************************************   -Ternary Plots-   *********************************************************************#

#https://ms609.github.io/Ternary/articles/Ternary.html
#The unerlying conceptualization of a Ternary plot is that the dimensions 
#correspond to proportions or concentrations. 

#library('Ternary')

#Example with Density
#par(mar=rep(0.2, 4))
#TernaryPlot()

#nPoints <- 4000L
#coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
#                     abs(rnorm(nPoints, 1, 1.5)),
#                     abs(rnorm(nPoints, 1, 0.5)))
#
#ColourTernary(TernaryDensity(coordinates, resolution=10L))
#TernaryPoints(coordinates, col='red', pch='.')
#TernaryDensityContour(coordinates, resolution=30L)

#ACT Example
#`Culture Similarity Scores` <- readr::read_csv('~/Desktop/R Plot Utilities Data/Culture Similarity Scores.csv', col_names = TRUE)

#Prepping Data
#coordinates <- vector('list', 2)
#distance_sum <- vector('list', length(coordinates))

#i <- 1
#coordinates[[i]] <- `Culture Similarity Scores`[c(6:8)]
#coordinates[[i]]$sum <- apply(coordinates[[i]][,names(coordinates[[i]])],1,sum)
#distance_sum[[i]] <- coordinates[[i]]$sum

#coordinates[[i]]$e_prop <- coordinates[[i]]$`e distance`/coordinates[[i]]$sum
#coordinates[[i]]$p_prop <- coordinates[[i]]$`p distance`/coordinates[[i]]$sum
#coordinates[[i]]$a_prop <- coordinates[[i]]$`a distance`/coordinates[[i]]$sum
#coordinates[[i]] <- coordinates[[i]][c(5:7)]

#Identifying Balanced Concepts
#coordinates <- as.matrix(coordinates[[1]])
#coordinates <- transform(coordinates, concept_var=apply(coordinates,1, var, na.rm = TRUE))
#threshold <- as.numeric(quantile(coordinates$concept_var, .10))

#coordinates <- cbind(coordinates, distance_sum[[i]])
#colnames(coordinates)[[5]] <- c('concept_distance')

#balanced_concepts <- coordinates[coordinates$concept_var <= threshold, ]
#coordinates <- coordinates[-c(4:5)]

#Plotting
#layout(rbind(1,2), heights=c(7,3))  # put dot plot in top 1/4th of the chart

#par(mar=rep(0.2, 4))
#TernaryPlot(alab="Evaluation Distance \u2192", blab="Potency Distance \u2192", clab=" \u2190 Activity Distance",
#            atip='E', btip='P', ctip='A')
#TernaryPoints(coordinates, col='red',  bg = "grey", pch=21, cex=0.8)
#TernaryDensityContour(coordinates, resolution=20L)

#Adding Title
#title('Student vs. MTurk', line=-1.1, family='serif', font.main=2,  cex.main = 2)

#Adding scatter box plot
#sample_sum <- sample(balanced_concepts$concept_distance, 200, replace=TRUE)

#par(bty="n", mar=c(4,0.2,0,1), mgp = c(2.25, 1, 0))
#stripchart(sample_sum, main=" ", xlab="Total Balanced Concept Distance", ylab=" ", method="jitter", col="black", 
#           pch=1, xlim=c(0, 3))

#Adding Boxplot
#par(new = T, mar = c(4,0.2,0,1))
#boxplot(balanced_concepts$concept_distance, horizontal=TRUE,las=1, staplelwd = 3.5,
#        main=" ", xlab = c(' '), xaxt='n')

#Adding Mean
#par(new = T, mar = c(4,0.2,0,1))
#plot(x=mean(balanced_concepts$concept_distance), y=max(balanced_concepts[[i]]), cex = 2.5, lwd = 2, pch = 18, col='black', xaxt='n', yaxt='n', xlab=' ')

#**********************************************   -Matrix Plot: Plotting Multiple Sets of Observations-   **************************************************#
#Nice Summary of the matplot function: https://riptutorial.com/r/example/4492/matplot

#Creating Data
#xmat <- cbind(rnorm(100, -3), rnorm(100, -1), rnorm(100, 1), rnorm(100, 3))
#head(xmat)

#matplot(xmat, type = 'l', bty='n', family='serif', xlab=c('Number of Observations'), ylab=c('Value'), cex.lab=1.5)
#title("Example Matrix Plot", cex.main = 1.5, font.main = 2, col.main = "black")

#******************************************************   -Tab Plot for Missing Data-   ********************************************************************#
#For more information and options see: https://cran.r-project.org/web/packages/tabplot/vignettes/tabplot-vignette.html
#library(tabplot)
#require(ggplot2)

#Getting Data
#data(diamonds)

## add some NA's for Visualization Purposes
#is.na(diamonds$price) <- diamonds$cut == "Ideal"
#is.na(diamonds$cut) <- (runif(nrow(diamonds)) > 0.8)

#Basic Visualization
#tableplot(diamonds)

#Focusing on a Subset of Variables
#tableplot(diamonds, select = c(carat, price, cut, color, clarity), sortCol = price)

#Zooming In
#tableplot(diamonds, select = c(carat, price, cut, color, clarity), sortCol = price, from = 0, to = 5)

#**********************************************************   -Heat Maps-   *********************************************************************#
#Reference Site: https://www.datanovia.com/en/lessons/heatmap-in-r-static-and-interactive-visualization/
#Heat Maps Help to Identify Patterns Among Continuous Variables Across Multiple Catogircal Ones (e.g., clydiner size and horse power for Fords and Buicks)

#Creating Data
#df <- scale(mtcars)

#Base R Heat Map Example

# Use RColorBrewer color palette names
#library("RColorBrewer")
#col <- colorRampPalette(brewer.pal(10, "RdYlBu"))(256)
#heatmap(df, scale = "none", col =  col, 
#        RowSideColors = rep(c("blue", "pink"), each = 16),
#       ColSideColors = c(rep("purple", 5), rep("orange", 6)))

#library("gplots")
#heatmap.2(df, scale = "none", col = bluered(100), 
#          trace = "none", density.info = "none")

#**************************************  -Heat Map with 3 Continuous Variables-   *****************************************************************#
#https://stackoverflow.com/questions/26822536/display-3-dimensions-on-a-2d-plot-in-ggplot2
#Using a Heat Map Like Strategy
#This approach should be used when the third variable is not assumed to be a function of the other two.

#Constructing Data
#df <- read.csv("Modu_VizData.csv")
#breaks <- seq(1.95,2.5,by=0.05)
#gg <- aggregate(mnc~cut(apl,breaks=breaks, labels=format(breaks[-1],nsmall=2))+modu, df,mean)
#colnames(gg)<- c("apl","modu","mnc")
#gg$modu <- as.factor(gg$modu)

#library(ggplot2)
#library(RColorBrewer)
#ggplot(gg) + 
#  theme(panel.background = element_rect(fill = 'white', linetype = 1),
#    panel.grid.minor = element_line(colour = 'snow3'),
#    plot.title = element_text(size = 14, face = "bold"), 
#    axis.text.x = element_text(colour="grey20",size=12),
#    axis.title.x = element_text(colour="grey20",size=14),
#    axis.text.y = element_text(colour="grey20",size=12),
#    axis.title.y = element_text(colour="grey20",size=16),
#    text = element_text(family="Times"),
#    axis.ticks.length = unit(0.25, "cm")) +  
#  geom_tile(aes(x=modu,y=apl,fill=mnc))+
#  scale_fill_gradientn(colours=rev(brewer.pal(15,"Spectral")))+
#  coord_fixed() +
#  base_breaks_x_d(gg$modu) +
#  base_breaks_y_d(gg$apl) 

#ACT Example

#Constructing Data
#df <- Pairs$`US Intersect`$`US MTurk Sample`[c(2:4)]
#breaks <- seq(-1,1,by=0.05)
#gg <- aggregate(A~cut(P,breaks=breaks, labels=format(breaks[-1],nsmall=2))+E, df,mean)
#colnames(gg) <- c('Potency', 'Evaluation', 'Activity')
#gg$Evaluation <- as.factor(gg$Evaluation)

#Plotting
#ggplot(gg) + 
#  theme(panel.background = element_rect(fill = 'white', linetype = 1),
#    panel.grid.minor = element_line(colour = 'snow3'),
#    plot.title = element_text(size = 14, face = "bold"), 
#    axis.text.x = element_text(colour="grey20",size=12),
#    axis.title.x = element_text(colour="grey20",size=14),
#    axis.text.y = element_text(colour="grey20",size=12),
#    axis.title.y = element_text(colour="grey20",size=16),
#    text = element_text(family="Times"),
#    axis.ticks.length = unit(0.25, "cm")) +  
#  geom_tile(aes(x=Evaluation,y=Potency,fill=Activity), alpha = .8)+
#  scale_fill_gradientn(colours=rev(brewer.pal(10,"Spectral")))+
#  scale_x_discrete(breaks = levels(gg$Evaluation)[c(T, rep(F, 9))]) +
#  scale_y_discrete(breaks = levels(gg$Potency)[c(T, rep(F, 12))]) +
#  coord_fixed() +
#  base_breaks_x_d(gg$Evaluation) +
#  base_breaks_y_d(gg$Potency) 

#Base R Heat Map with Dendogram, Legend, and Axis Labels

#Loading Data and Formatting Data
#Language_AllGroupARI <- read.csv("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities Data/Language_AllGroupARI.csv", header=TRUE)

#df <- Language_AllGroupARI[2:4]
#breaks <- seq(0,30,by=1)

#gg <- aggregate(ARI~cut(T2_Group,breaks=breaks, labels=format(breaks[-1],nsmall=2))+T1_Group, df,mean)
#colnames(gg)<- c("T1_Groups","T2_Groups","ARI")

#gg$T1_Groups <- as.numeric(as.character(gg$T1_Groups))
#gg$T1_Groups <- as.factor(gg$T1_Groups)
#gg$T2_Groups <- as.factor(gg$T2_Groups)

#Creeating and Populating Visualization Matrix
#Viz_Mat <- matrix(nrow=29, ncol=29)
#colnames(Viz_Mat) <- seq(1, 29, by=1)

#row_id <- as.numeric(as.c(gg[[1]]))
#col_id <- as.numeric(as.c(gg[[2]]))

#for (i in seq_along(row_id)) { 
#  Viz_Mat[row_id[[i]], col_id[[i]]] <- gg[[i, 3]]
#}

#rm(df, gg, Language_AllGroupARI, breaks, col_id, row_id, i)

#Creating Heatmap Visualization Elements
#col <- hcl.colors(12, "YlOrRd", rev = TRUE)
#legend_image <- as.raster(matrix(rev(col)), ncol=1)

#Plotting
#png("p1.png", width = 650, height = 587)
#  par(mar = c(3, 1, 2, 1), family='serif')  
#  heatmap(Viz_Mat, scale = "none", col=col, cexCol=1.3, cexRow = 1.3)
#  mtext("Time 1: Number of Positions", side = 1, line = 1, cex = 1.5)
#  mtext("Time 2: Number of Positions", side = 4, line = -3.4, cex = 1.5)

#  par(new=TRUE, mar=c(5.1, 4.1, 4.1, 1), family='serif')
#  plot(0, type='n', xlim=c(-0.1, 1.08), ylim=c(0, 1), xlab=' ', ylab=' ', cex.lab=1.5, family='serif', 
#     axes=FALSE, bty='n')

  #Add Legends
#  rasterImage(legend_image, 1.07, 0.25, 1.09, 0.9)
#  text(x=1.11, y = seq(0.26,0.89,l=5), labels = seq(0,1,l=5), cex=0.7)
#  text(x= 1.09, y=0.93, labels = c("ARI"))
#dev.off()

#Reading png back in to transform into a ggplot object to produce a higher fidelity PDF
#g <- magick::image_read('p1.png')
#file.remove('p1.png')
#p1 <- ggplotify::as.ggplot(g)
#rm(g)

#Saving as PDF
#ggplot2::ggsave("Language_ARI Heatmap_31Dec2019.pdf")

#Base R Heat Map with 3 Continuous Variables with Histogram Sidebars and Contour Overlay

#Note:  The image function is a far more flexible function than the heatmap function. 
#       If the image is not utilizing the clustering functions of the heatmap representation, than consider using
#       image if creating a plot with overlays.

#Loading Data
#load('US_Student&MTurk_Pairs.Rdata')

#Reference Dataset and breaks
#df <- Pairs$`US Intersect`$`US MTurk Sample`[c(2:4)]
#breaks <- seq(-1,1,by=0.05)

#df <- df[order(df$E, df$P),]

#Creating Unique Combinations with Breaks: Evaluation and Potency in this Example
#EPA_List <- vector('list', 1)
#EPA_List[[1]] <- aggregate(A~cut(P,breaks=breaks, labels=format(breaks[-1],nsmall=2))+E, df,mean)

#Renaming Columns
#colnames(EPA_List[[1]]) <- c('Potency', 'Evaluation', 'Activity')

#Making the Second Variable a Factor
#for (i in seq_along(EPA_List)){
#  EPA_List[[i]][[2]] <-  as.factor(EPA_List[[i]][[2]] )
#}

#plots <- c('p_1')

#names <- vector('list', length(EPA_List))
#for (i in seq_along(EPA_List)){
#  names[[i]] <-  colnames(EPA_List[[i]])
#}

#Use RColorBrewer color palette names: Cold-Hot
#col <- colorspace::diverging_hcl(11, h = c(250, 10), c = 100, l = c(37, 88), power = c(0.7, 1.7))

#Creating a matrix object for the heat map, in this case the image object
#i <- 1
#out <- with(EPA_List[[i]], {
#  out <- matrix(nrow=nlevels(EPA_List[[i]][[1]]), ncol=nlevels(EPA_List[[i]][[2]]),
#                dimnames=list(levels(EPA_List[[i]][[1]]), levels(EPA_List[[i]][[2]])))
#  out[cbind(EPA_List[[i]][[1]], EPA_List[[i]][[2]])] <- EPA_List[[i]][[3]]
#  out
#})

#Creating Rounded Colum Names to make later data processing easier
#colnames(out) <- round( as.numeric(colnames(out)), digits=2)
#rownames(out) <- round( as.numeric(rownames(out)), digits=2)

#Transposing because image displays rows on y column rather x
#out <- t(out)

#Setting Up Layout: Center Panel and 2 Sidebars
#layMat <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
#layout(layMat, widths=c(7/9, 2/9), heights=c(2/9, 7/9))
#ospc <- 0.5 # outer space
#pext <- 4 # par extension down and to the left
#bspc <- 0.9 # space between heatmap and bar plots

#Creating heatmap image using the image function (more flexible in this case)
#par. <- par(mar=c(pext, pext, bspc, bspc), oma=rep(ospc, 4)) 
#par(xpd = T, mgp=c(3, 1, 0), mar = par()$mar + c(0,0,0,2.55), family='serif')
#image(out, useRaster = TRUE, axes= FALSE, col=col, xlim=c(-0.01,1.01))

#y <- seq(0, 1, by=0.25)
#axis(2, y, cex.axis = 1, labels=NA)
#mtext(side=2, c("-1", "-0.5", "0", "0.5", "1"), at=c(0, 0.25, 0.5, 0.75, 1), line=0.7)
#mtext(side=2, colnames(EPA_List[[i]])[[1]], at=c(0.5), line=1.75, cex=1.5)

#x <- seq(0, 1, by=0.25)
#axis(1, x, cex.axis = 1, labels=NA)
#mtext(side=1, c("-1", "-0.5", "0", "0.5", "1"), at=c(0, 0.25, 0.5, 0.75, 1), line=0.7)
#mtext(side=1, colnames(EPA_List[[i]])[[2]], at=c(0.5), line=1.75, cex=1.5)

#Adding Legend
#legend_image <- as.raster(matrix(rev(col)), ncol=1)
#rasterImage(legend_image, 1.02, 0.5, 1.05, 0.9)
#text(x=1.075, y = seq(0.51,0.89,l=5), labels = seq(-1,1,l=5), cex=0.7)

#par(family='serif', las=0)
#text(x=1.055, y=0.93, colnames(EPA_List[[i]])[3], cex=0.85)

#Adding Histogram Sidebars
#if (i == 1) {
#  x <- df$E
#  y <- df$P
#} else if (i == 2) {
#  x <- df$E
#  y <- df$A
#} else {
#  x <- df$A
#  y <- df$P
#}

#Adding Contour: Compute 2D kernel density, see MASS book, pp. 130-131
#z <- MASS::kde2d(x, y, n=100)
#par(new=TRUE)
#plot(type='n', x=x, y=y, xlim=c(-1, 1), ylim=c(-1, 1), axes=FALSE, xlab=' ', ylab= '')
#contour(z, drawlabels=FALSE, nlevels=8, col='black', add=TRUE)

#Adding Histograms
#xhist <- hist(x, plot=FALSE, breaks=seq(from=min(x), to=max(x), length.out=length(breaks)))
#yhist <- hist(y, plot=FALSE, breaks=seq(from=min(y), to=max(y), length.out=length(breaks)))

#Determine the plot range and all the things needed for the barplots and lines
#xx <- seq(min(x), max(x), length.out=(length(breaks)*5)) # evaluation points for the overlaid density
#xy <- dnorm(xx, mean(x), sd(x)) # density points
#yx <- seq(min(y), max(y), length.out=(length(breaks)*5))
#yy <- dnorm(yx, mean(y), sd(y))

#Barplot and line for x (top)
#par(mar=c(0, pext, 0, 0))
#barplot(xhist$density, axes=FALSE, ylim=c(-0.01, max(xhist$density, xy)), col=c("gray"), density=c(30), 
#        space=0) # barplot
#lines(seq(from=0, to=length(breaks)-1, length.out=(length(breaks)*5)), xy, col='blue') 

#Barplot and line for y (right)
#par(mar=c(pext, 1, 0, 0))
#barplot(yhist$density, axes=FALSE, xlim=c(0, max(yhist$density, yy)), 
#        col=c("grey"), density=c(40), space=0, horiz=TRUE) # barplot
#lines(yy, seq(from=0, to=length(breaks)-1, length.out=(length(breaks)*5)), col='blue') # line
#Restore parameters
#par(par.)

#*******************************************************   -Base R Contour Scatter Plot-   ***********************************************************#
#Data generated by ACT_Cosine_Function_30Aug2019.R

#Creating Plot Data Elements

#Creating Colors Attribute
#k <- as.numeric(length(unique(round(similarity[[i]][[15]], 1))))
#my.cols <- colfunc(k)

#Creating Labels
#label <- similarity[[i]][similarity[[i]][[15]] <= descriptives[[i]]$`5th percentile` | similarity[[i]][[15]] >= descriptives[[i]]$`95th percentile`, ]
#label <- label[order(label$`epa distance`), ]
#label <- label[c(1:7,(nrow(label)-7):(nrow(label))), ]
#label$label <-  sub("i_", " ", label[[2]]) 
#label$label <-  sub("b_", " ", label$label) 
#label$label <-  sub("m_", " ", label$label) 
#label$label <- trim(label$label)
#label <- label[c(2, 18)]

#Merging Labels Back In
#viz_data <- merge(x = similarity[[i]], y = label, by = c('Term ID'), all.x = TRUE)

#Removing Potentially Duplicated Rows
#viz_data <- viz_data[!duplicated(viz_data[c("Term ID")]),]

#Scaling distances for the purposes of visualization
#x <- scale(similarity[[i]]$`e distance`, center=TRUE, scale=TRUE)
#y <- scale(similarity[[i]]$`p distance`, center=TRUE, scale=TRUE)
#activity <- scale(similarity[[i]]$`a distance`, center=TRUE, scale=TRUE)
#label <- viz_data$label

#viz_data <- as.data.frame(cbind(x, y, activity), stringsAsFactors=FALSE)
#viz_data[] <- lapply(viz_data[1:3], as.numeric)
#viz_data$label <- label
#colnames(viz_data) <- c('e distance scaled', 'p distance scaled', 'a distance scaled', 'label')

#Getting rid of _ to make the labels a bit cleaner
#viz_data$label <-  sub("_", " ", viz_data$label) 
#viz_data$label <- trim(viz_data$label)
#rm(x, y, activity, label)

#Adding Reference Object to Anchor Text
#bottom <- -3.95
#right <- 3.5
#info <- c('Points Sized by Activity Distance (Scaled)')
#zero <- 0
#y_max <- 4
#y_min <- -4
#x_max <- 4
#x_min <- -4

#Compute 2D kernel density, see MASS book, pp. 130-131 as meausure of status gradient
#z <- kde2d(viz_data$`e distance scaled`, viz_data$`p distance scaled`, n=length(viz_data$`e distance scaled`))

#Plotting (Including ggplot element to repel labels)

#viz_png <- paste0(titles[[i]],'.png')
#png(viz_png, width = 12, height = 7,  units = 'in', res=600)

#layout(matrix(1:2,nrow=1),widths=c(0.9,0.1))
#par(bty="n", mar=c(5,5,1,5), xpd=TRUE)
#plot(jitter(viz_data$`e distance scaled`, factor=1.5), jitter(viz_data$`p distance scaled`, factor=1.9), type = "p", pch=19, col=my.cols, 
#     cex=viz_data$`a distance scaled`, cex.lab=1.5,  family = 'serif', xlab="Evaluation Distance (Scaled)", ylab="Potency Distance (Scaled)", 
#     ylim=c(-5, 5), xlim=c(-5, 5))

#plot gridlines
#grid(lwd = 2)

#Adding Guidelines
#s#egments(zero, y_min, zero, y_max)
#segments(x_min, zero, x_max, zero)

#contour(z, drawlabels=FALSE, nlevels=k, col='blue', add=TRUE)

#text(viz_data$`e distance scaled`, viz_data$`p distance scaled`, viz_data$label, cex=0.7, pos=2, col="black", offset = 1)
#text(right, bottom, info, cex=0.8, pos=1, col="black")

#axis(side=1, font = 2, family = 'serif')
#axis(side=2, font = 2, family = 'serif')

#title(titles[[i]], adj=0, family='serif')

#Adding Discrete Legend
#xl <- 1
#yb <- 1
#xr <- 1.5
#yt <- 2

#par(mar=c(5,0.5,1,0.5))
#plot(NA,type="n", main="Total Distance", xlab = " ", ylab = " ", family = 'serif',
#     xlim=c(1,2),ylim=c(1,2),xaxt="n",yaxt="n",bty="n", cex.main=0.9, adj = 0)

#rect(
#  xl,
#  head(seq(yb,yt,(yt-yb)/k),-1),
#  xr,
#  tail(seq(yb,yt,(yt-yb)/k),-1),
#  col=my.cols
#)

#mtext(k:1,side=2,at=tail(seq(yb,yt,(yt-yb)/k),-1)-0.05,las=2,cex=0.8)

#dev.off()

#Re-Importing Image
#p_2 <- magick::image_read(viz_png)

#file.remove(viz_png)

#rm(k, my.cols, bottom,  right, info, zero, x_min, x_max, y_min, y_max, xl, xr, yb, yt, z)

#p_2 <- ggplotify::as.ggplot(p_2)

#Ordering viz_data to check that scaling is sensible
#viz_data <- viz_data[order(viz_data[[1]], viz_data[[2]]), ]

#Computing Graph Positions to Add Text Annotations (Use axTicks() in future.)
#e_graph_distance <- scales::rescale(viz_data$`e distance scaled`, to = c(0.2, 0.745)) 
#p_graph_distance <- scales::rescale(viz_data$`p distance scaled`, to = c(0.23, 0.84))

#Creating Graph Distance Data.Frame to Generate Annotations
#graph_distance <- as.data.frame(cbind(e_graph_distance, p_graph_distance))
#graph_distance$label <- viz_data$label

#Adding Annotations with geom_text_repel
#p_2 <- p_2 +
#  geom_text_repel(data=graph_distance, aes(x=graph_distance[[1]], y=graph_distance[[2]], label = graph_distance[[3]]), force=0.3) 

#**************************************************   -Waterfall Plots: Visualizing Bi-Variate Distributions-   ****************************************#
#library(MASS)

#Creating Data
#n <- 500000; y<-rnorm(n); x<-3*rnorm(n)+y^2
#plot(x,y, cex =.2, col=rgb(0,0,0,.1))

#Figure 1: Plot out the distribution using MASS density mapping
#par(mar=c(.1,.1,.1,.1))
#par(mfrow = c(2,2))

#den3d <- kde2d(x, y)
#persp(den3d, box=FALSE, expand=.6)
#persp(den3d, box=FALSE, expand=.6, theta=-90)
#persp(den3d, box=FALSE, expand=.6, theta=180)
#persp(den3d, box=FALSE, expand=.6, theta=90)

#Figure 2: Basic Slices

#fcol can either be a color or rgb vector
#fcol <- c(.6,0,0,.35)
#lcol <- c(.7,1,1,.35)

#Default Number of Slices is 50, this can be reset with the slices parameter.
#par(mar=c(.1,.1,.1,.1))
#par(mfrow = c(2,2))

#util$slicedens(x,y,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(y,x,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

#util$slicedens(-x,-y,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(-y,-x,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

# Figure 3: Proportionally distanced cuts
#util$slicedens(x,y, cutprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(y,x, cutprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

#util$slicedens(-x,-y, cutprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(-y,-x, cutprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

# Figure 4: Transparency weighted cuts
#util$slicedens(x,y, transprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(y,x, transprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

#util$slicedens(-x,-y, transprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(-y,-x, transprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

# Figure 5: Height weighted cuts
#fcol <- c(.6,0,0,.2)
#lcol <- c(.7,1,1,.4)

#util$slicedens(x,y, heightprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.3)

#util$slicedens(y,x, heightprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=3)

#util$slicedens(-x,-y, heightprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.3)

#util$slicedens(-y,-x, heightprop=T,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=4)

#Figure 6: Tree Variables (Sliced by Two Heated by the Third)
#z <- -(abs(x)+abs(y))+rnorm(n)*3
#fcol <- rbind(c(0,.1,.5,.5), c(.3,.8,.8,.5), c(1,1,0,.5))
#lcol <- rbind(c(0,.3,.3,.8), c(.1,.1,.2,.7), c(0,0,1,.65))

#par(mar=c(.1,.1,.1,.1))
#par(mfrow = c(2,2))

#util$slicedens(x,y,z,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(y,x,z, 
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1.6)

#util$slicedens(z,y,x,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#util$slicedens(z,x,y,
#          fcol=fcol, bcol='white', lcol=lcol,
#          gboost=1)

#*******************************************************   -ggnetworks Contour Nentowrk-   ************************************************************#
#library(ggplot2)      #Visualizing data
#library(ggnetwork)    #Network Visualization
#library(ggraph)       #Allows you to selective label points on a graph
#library(network)      #Component of Statnet, Useful for Pajek File Conversions
#library(dplyr)

#load('~/GPM_Diss/Networks_List.Rdata')

#Importing Vector Files
#Coordinates_List <- vector("list", length(Networks_List))

#x-cooridnates for each layout
#x_coord <- vector("list", length(Networks_List))
#x_coord[[1]] <- readLines('~/GPM_Diss/Time1_Structure/Email Network_T1_Xcoord.vec')
#x_coord[[2]] <- readLines('~/GPM_Diss/Time1_Language/Language1_x_coordinates.vec')
#x_coord[[3]] <- readLines('~/GPM_Diss/Time2_Structure/Structure2_x_coordinates.vec')
#x_coord[[4]] <- readLines('~/GPM_Diss/Time2_Language/Language2_x_coordinates.vec')

#x_coord <- lapply(x_coord, function(x) as.numeric(x[c(-1)]))

#y-cooridnates for each layout
#y_coord <- vector("list", length(Networks_List))
#y_coord[[1]] <- readLines('~/GPM_Diss/Time1_Structure/Email Network_T1_Ycoord.vec')
#y_coord[[2]] <- readLines('~/GPM_Diss/Time1_Language/Language1_y_coordinates.vec')
#y_coord[[3]] <- readLines('~/GPM_Diss/Time2_Structure/Structure2_y_coordinates.vec')
#y_coord[[4]] <- readLines('~/GPM_Diss/Time2_Language/Language2_y_coordinates.vec')

#y_coord <- lapply(y_coord, function(x) as.numeric(x[c(-1)]))

#Populating Coordinates List
#for (i in seq_along(Networks_List)) {
#  Coordinates_List[[i]] <- as.matrix(cbind(x_coord[[i]], y_coord[[i]]))
#  names(Coordinates_List)[[i]] <- paste0("Network_", i)
#}

#rm(x_coord, y_coord, i)

#E-mail Network: Time 1
#Edges <- as.matrix.network(Networks_List[[1]],matrix.type="edgelist")
#Nodes <- network.vertex.names(Networks_List[[1]])

#Creating Network
#el <- Edges

#el[,1]=as.character(el[,1])
#el[,2]=as.character(el[,2])

#n_1=network(el,matrix.type="edgelist",directed=TRUE) 

#summary(n_1)

#E-mail Network: Time 1
#p1  = ggnetwork(n_1, layout = Coordinates_List[[1]]) %>%
#  ggplot(aes(x = x, y = y, xend = xend, yend = yend))+ 
#  theme(panel.background = element_rect(fill = 'white', linetype = 1),
#        panel.grid.minor = element_line(colour = 'snow3'),
#        text = element_text(family="Times"),
#        plot.title = element_text(size = 15, face = 'bold'),
#        axis.title.x = element_blank(),
#        axis.text.x=element_blank(),
#        axis.ticks.x=element_blank(),
#        axis.title.y = element_blank(),
#        axis.text.y=element_blank(),
#        axis.ticks.y=element_blank())+
#  guides(size=FALSE) +
#  stat_density2d(aes(alpha=..level.., fill=..level..), size=0.01, bins=10, geom="polygon") + 
#  scale_fill_gradient(low = "cornsilk", high = "burlywood4", guide = FALSE) +
#  scale_alpha(range = c(0.00, 0.8), guide = FALSE) +
#  geom_density2d(colour="gray8", bins=10) +
#  scale_size(guide = 'none') +
#  ggtitle("Structural Network: Time 1") +
#  theme(plot.margin = unit(c(1,1,0,1), "cm")) +
#  theme(axis.line = element_line(color = 'black'))

#***********************************************************   -3D Polar Plot-   *********************************************************************#
#Creating some example data
#x = rnorm(20)
#y = rnorm(20)
#z = rnorm(20)

#PolarImageInterpolate(x, y, z, contours=TRUE, legend=TRUE, points=TRUE, extrapolate=FALSE)

#***********************************************************   -Flow Diagrams-   *********************************************************************#

#Chord Diagram: https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html

# Create an adjacency matrix (20 origin nodes and 5 destination nodes) : 
#numbers <- sample(c(1:1000), 100, replace = T)
#data <- matrix( numbers, ncol=5)
#rownames(data) <- paste0("orig-", seq(1,20))
#colnames(data) <- paste0("dest-", seq(1,5))

#library(circlize)
#chordDiagram(data, transparency = 0.5)

#Arc Diagrams (Basic Templates, I might build these in Base at some point to be a bit cleaner)

#library(devtools)
#install_github("gastonstat/arcdiagram", username = "gastonstat")
#library(arcdiagram)
#library(igraph)

#Getting Data
#mis_file = "~/Desktop/lesmiserables.txt"
#mis_graph = igraph::read.graph(mis_file, format="gml")

#Formatting Data
#edgelist = get.edgelist(mis_graph)  #Get edgelist
#vlabels = get.vertex.attribute(mis_graph, "label")    #Get vertex labels
#vgroups = get.vertex.attribute(mis_graph, "group")    #Get vertex groups
#vfill = get.vertex.attribute(mis_graph, "fill")       #Get vertex fill color
#vborders = get.vertex.attribute(mis_graph, "border")  #Get vertex border color
#degrees = degree(mis_graph)                           #Get vertex degree
#values = get.edge.attribute(mis_graph, "value")       #Get edges value

#Putting Data into Cannonical Format
#library(reshape)

# data frame with vgroups, degree, vlabels and ind
#x = data.frame(vgroups, degrees, vlabels, ind=1:vcount(mis_graph))

# arranging by vgroups and degrees
#y = dplyr::arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'
#new_ord = y$ind

#Plot arc diagram
#arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
#        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
#        cex.nodes = log(degrees)+0.5, pch.nodes=21,
#        lwd.nodes = 2, line=-0.5,
#        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)

#Alternative Approach with ggraph: https://www.data-to-viz.com/graph/arc.html
#library(dplyr)
#library(tidyr)
#library(tibble)
#library(igraph)
#library(RColorBrewer)
#library(ggraph)

# Load data
#dataUU <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyUndirectedUnweighted.csv", header=TRUE)

# Transform the adjacency matrix in a long format
#connect <- dataUU %>% 
#  gather(key="to", value="value", -1) %>%
#  mutate(to = gsub("\\.", " ",to)) %>%
#  na.omit() 

# Number of connection per person (Clever bit of coding here)
#c( as.character(connect$from), as.character(connect$to)) %>%
#  as.tibble() %>%
#  group_by(value) %>%
#  summarize(n=n()) -> coauth
#colnames(coauth) <- c("name", "n")

# Create a graph object with igraph
#mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

# Find community
#com <- walktrap.community(mygraph)

#Reorder dataset and make the graph
#coauth <- coauth %>% 
#  mutate( grp = com$membership) %>%
#  arrange(grp) %>%
#  mutate(name=factor(name, name))

# keep only 10 first communities
#coauth <- coauth %>% 
#  filter(grp<16)

# keep only this people in edges
#connect <- connect %>%
#  filter(from %in% coauth$name) %>%
#  filter(to %in% coauth$name)

# Create a graph object with igraph
#mygraph <- graph_from_data_frame( connect, vertices = coauth, directed = FALSE )

# prepare a vector of n color in the viridis scale
#colfunc <-colorRampPalette(c("red","yellow","springgreen","royalblue"))
#mycolor <- colfunc(max(unique(coauth$grp)))

# Make the graph
#ggraph(mygraph, layout="linear") + 
#  geom_edge_arc(edge_colour="black", edge_alpha=0.2, edge_width=0.3, fold=TRUE) +
#  geom_node_point(aes(size=n, color=as.factor(grp), fill=grp), alpha=0.5) +
#  scale_size_continuous(range=c(0.5,8)) +
#  scale_color_manual(values=mycolor) +
#  geom_node_text(aes(label=name), angle=65, hjust=1, nudge_y = -1.1, size=2.3) +
#  theme_void() +
#  theme(
#    legend.position="none",
#    plot.margin=unit(c(0,0,0.4,0), "null"),
#    panel.spacing=unit(c(0,0,3.4,0), "null")) +
#  expand_limits(x = c(-1.2, 1.2), y = c(-5.6, 1.2)) 

#Alluvial Plots: 

#Creating Data
#library(alluvial)
#tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
#alluvial(tit[,1:4], freq=tit$Freq,
#         col = ifelse(tit$Survived == "Yes", "orange", "grey"),
#         border = ifelse(tit$Survived == "Yes", "orange", "grey"),
#         hide = tit$Freq == 0,
#         cex = 0.7
#)

#****************************************************************   -Bump Plot-   ********************************************************************#

#Note: This inspiration for this plot came from: http://climaps.eu/#!/map/absolute-and-relative-visibility-of-issues-in-unfccc-negotiations-1995-2013
#      A Bump Chart, like a Sankey or Alluvial Plot shows flow over time. 
#      This example illusrtrates the popularity of climate change topics over time.
#      Unfortunately, the data the authors provided does not completely correspond with the plot displayed in the post in several respects.
#      Consequently, the example plot is in some respects too complicated. With my own data, I would pair down. 
#      Nevertheless, it's useful place to start, and provides some strategies for handling lots of plot elements.

#Loading Data
#load("~/R Resources/R Plotting Utilities & Resources/R Plot Utilities Data/COPs_Themes.Rdata")

#Creating Visualization Components
#index <- as.data.frame(seq(1,21,by=1))
#cop_label <- c("INC 11", "COP 01", "COP 02", "COP 03", "COP 04", "COP 05", "COP 06", 
#              "COP 06b", "COP 07", "COP 08", "COP 09", "COP 10", "COP 11", "COP 12", 
#               "COP 13", "COP 14", "COP 15", "COP 16", "COP 17", "COP 18", "COP 19")
#index$cop_label <- cop_label
#colnames(index)[[1]] <- c('cop_id')

#COPs_Themes <- merge(COPs_Themes, index, by="cop_label",all.x=TRUE) 

#Creating themes index: Used to generate the flow polygons in the plot.
#theme_index <- as.data.frame(unique(COPs_Themes$Themes), stringsAsFactors=FALSE)
#theme_index$theme_id <- c(1,11, 3, 2, 8, 6, 4, 9, 5, 7, 10, 12)
#colnames(theme_index)[[1]] <- c('Themes')
#theme_index <- theme_index[order(theme_index$theme_id), ]

#Merging theme ids with COP_Themes Data
#COPs_Themes <- merge(COPs_Themes, theme_index, by="Themes",all.x=TRUE) 
#COPs_Themes <- COPs_Themes[order(COPs_Themes$cop_id), ]

#Creating Splines
#curve_list <- vector('list', nrow(theme_index))
#names(curve_list) <- theme_index$Themes


#for (i in seq_along(theme_index[[1]])){
#  Theme <- COPs_Themes %>%

#    dplyr::filter(theme_id == i)
  
#  xy <- Theme[c(7, 3)]
  
  #Spline Fitting
#  plot(0)
#  val.curve <- xspline(xy, shape = -0.5, draw = FALSE)
#  val.curve <- data.frame(val.curve)
# junk <- dev.off(which = dev.cur())
  
#  val.curve$base_y <- 0.75*val.curve$y
  
#  curve_list[[i]] <- val.curve
  
#  rm(Theme, xy, val.curve)
#}

#rm(junk)

#Creating color variables for rgb function
#reds <- rev(c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.55, 0.6, 0.7, 0.8, 0.9, 1))
#greens <- c(0.5, 0.55, 0.6, 0.7, 0, 0.1, 0.2, 0.3, 0.4, 0.8, 0.9, 1)
#blues <- c(0, 0.1,  0.6, 0.7, 0.8, 0.9, 1, 0.2, 0.3, 0.4, 0.5, 0.55)

#Plotting
#png("p_1.png", width=1243, height = 621)
#  par(mar = c(3, 9, 3, 0), family='serif')  
#  plot(0, type='n', xlim=c(1, 21), ylim=c(-20, 100), xlab=' ', ylab=' ', cex.lab=1.5, family='serif', 
#      axes=FALSE, bty='n')

  #Adding Axis Text
#  mtext(side=1, c('1995', ' ', '1996', '1997', '1998', '1999', '2000', '2001', ' ', 
#                '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009',
#                '2010', '2011', '2012', '2013'), 
#      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.1, cex=1)

#  mtext(side=3, c('New York', 'Berlin', 'Geneva', 'Kyoto', 'Buenos Aires', 'Bonn', 'The Hague', 'Bonn', 'Marrakech', 
#                'New Delhi', 'Milan', 'Buenos Aires', 'Montreal', 'Nairobi', 'Bali', 'Poznan', 'Copenhagen',
#                'Cancun', 'Durban', 'Doha', 'Warsaw'), 
#      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.1, cex=0.75)

#  mtext(side=3, c('INC 11', 'COP 1', 'COP 02', 'COP 03', 'COP 04', 'COP 05', 'COP 06', 'COP 06b', 'COP 07', 
#                'COP 08', 'COP 09', 'COP 10', 'COP 11', 'COP 12', 'COP 13', 'COP 14', 'COP 15',
#                'COP 16', 'COP 17', 'COP 18', 'COP 19'), 
#      at=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
#           11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21), line=0.85, cex=1, font=2)

  #Adding Reference Lines
#  for (i in seq_along(index[[1]])){
#    abline(v=i, lty=1, col="grey50")
#  }

  #Adding Flow Polygons
#  for (i in seq_along(curve_list)){
#    lines(curve_list[[i]]$x, curve_list[[i]]$base_y, col="black", lwd=1)
#    lines(curve_list[[i]]$x, curve_list[[i]]$y, col="black", lwd=1)
#    polygon(c(curve_list[[i]]$x,rev(curve_list[[i]]$x)), 
#          c(curve_list[[i]]$y,rev(curve_list[[i]]$base_y)) ,
#          col=rgb(reds[[i]], greens[[i]], blues[[i]], 0.5), border = NA)
#  }

  #Adding y-axis tick labels
#  mtext(side = 2, text = 'Adaptation funding & equity', at =29.75, line = -1.4, cex = 0.9, las=1)
#  mtext(side = 2, text = 'GHGs & emission measures', at =23.625, line = -1.4, cex = 0.9, las=1)
#  mtext(side = 2, text = 'Energy + technology transfer', at =19, line = -1.4, cex = 0.9, las=1)
#  mtext(side = 2, text = 'Models and IPCC', at =13.125, line = -1.4, cex = 0.9, las=1)
#  mtext(side = 2, text = 'Transport sector', at =6.2, line = -1.4, cex = 0.9, las=1)
#  mtext(side = 2, text = 'Land use & forests', at =3, line = -1.4, cex = 0.9, las=1)

#  mtext(side = 2, text = 'Vulnerability + adaptation action', at =.3, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 3), y=c(0.3, 0.3), col = 'black', lty = 3, lwd=1.5)
#  lines(x=c(3, 3), y=c(0.3, 3.5), col = 'black', lty = 3, lwd= 1.5)

#  mtext(side = 2, text = 'CDM + carbon offsets', at =-3.25, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 4), y=c(-3.25, -3.25), col = 'black', lty = 3, lwd=1.5)
#  lines(x=c(4, 4), y=c(-3.25, 3), col = 'black', lty = 3, lwd= 1.5)

#  mtext(side = 2, text = 'Kyoto protocol', at =-6.25, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 5), y=c(-6.25, -6.25), col = 'black', lty = 3, lwd=1.5)
#  lines(x=c(5, 5), y=c(-6.25, 19.5), col = 'black', lty = 3, lwd= 1.5)

#  mtext(side = 2, text = 'Social & environmental impacts', at =-9.25, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 6), y=c(-9.25, -9.25), col = 'black', lty = 3, lwd=1.5)
#  lines(x=c(6, 6), y=c(-8, 3.5), col = 'black', lty = 3, lwd= 1.5)

#  mtext(side = 2, text = 'Compliance enforcement', at =-13, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 2), y=c(-13, -13), col = 'red', lty = 3, lwd=1.5)
#  lines(x=c(2, 2), y=c(-13, 2), col = 'red', lty = 3, lwd= 1.5)

#  mtext(side = 2, text = 'Post-Kyoto + Redd', at =-16.25, line = -1.4, cex = 0.9, las=1)
#  lines(x=c(1, 5), y=c(-16.25, -16.25), col = 'red', lty = 3, lwd=1.5)
#  lines(x=c(5, 5), y=c(-16.25, 2.3), col = 'red', lty = 3, lwd= 1.5)

  #Adding Note
#  mtext(side=1, c("The height of the flows indicates the theme's frequency for that year."), 
#      at=c(2), line=1.3, cex=1)
#dev.off()

#g <- magick::image_read('p_1.png')
#file.remove('p_1.png')
#p <- ggplotify::as.ggplot(g)  

#ggplot2::ggsave("Example Bump Chart_COP_Themes_2Jan2019.pdf")

#******************************************************   -Hägerstrand Time-Space Cube-   ************************************************************#
#Hägerstrand Time-Space Cube visualize progression over time through a space (e.g., animal traversing a landscape)
#This teplace was provided by: https://movebankworkshopraleighnc.netlify.com/testtimecube
#This is a starting point for what could be donel.

#library(rgl)
#library(dplyr)
#library(tidyr)
#library("htmltools", "rglwidget")

#Getting Data: https://movebankworkshopraleighnc.netlify.com/index.html
#ssfdat<-read.csv("~/Desktop/FisherSSFannotate.csv-1402112999909362686.csv")

#Convert time variables
#ssfdat$timestamp <-as.POSIXct(ssfdat$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#Simplify some variable names and make case a numeric variable
#names(ssfdat)[c(5,4,6)]<-c("Elevation", "LandClass", "PopDens")

#Create landcover classes (as suggested by Scott Lapoint :)
#ssfdat$LandClass <-as.factor(ssfdat$LandClass)
#ssfdat$LandClass<-as.character(ssfdat$LandClass)
#ssfdat<-ssfdat %>% mutate(landC = fct_collapse(LandClass,
#                                               agri = c("11", "14", "30"),
#                                               forest =c("30","40","50","60", "70","80", "90","100"),
#                                               shrub= c("110", "130", "150"),
#                                               grass = c("120", "140"),
#                                               wet= c("160"),
#                                              other = c("170", "180", "190", "200", "210", "220")))

#Display space-time cube
#martenF1 <- ssfdat
#martenF1<-martenF1[order(martenF1$timestamp),]

#with(martenF1, plot3d(location.long,location.lat,timestamp, type="l", col=as.integer(martenF1$landC)))
#(stcube<-with(martenF1, plot3d(location.long,location.lat,timestamp, type="l", 
#                               col=as.numeric(cut(martenF1$Elevation,5)), alpha=0.4)))
#rglwidget() 

##############################################
#   STATISTICAL FUNCTIONS & VISUALIZATIONS   #
##############################################

#Regression Diagnostics: http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/

#Getting Data
#library(tidyverse)
#library(broom)
#library(datarium)
#library(ggfortify)
#theme_set(theme_classic())

#data("marketing", package = "datarium")
#sample_n(marketing, 5)

#Fitting a model
#model <- lm(sales ~ youtube, data = marketing)

#Building Plot
#par(mfrow = c(2, 2))
#plot(model)

#ggplot2::autoplot(model)


#**********************************************************   -Dotplot Style 4Plot-   ****************************************************************#

util$four_plot <- function(var, var_name) {
  png("p_1.png", width = 877, height = 676)
  layout.matrix <- matrix(c(1, 3, 2, 4), nrow = 2, ncol = 2)
  layout(mat = layout.matrix,
         heights = c(2,2), # Heights of the two rows
         widths = c(2,2)) # Widths of the two columns
  
  x <- base::seq(1, length(var), 1)
  x_axis <- pretty(x)
  y_axis <- pretty(var)
  y <- var[c(1:(length(var)-1))]
  y_lag <- var[2:length(var)]
  
  #Run Sequence Plot
  par(mar = c(4,7.5,2,5),  family='HersheySerif')
  plot(0, type='n', xlim=c(min(x_axis), max(x_axis)), ylim=c(min(y_axis), max(y_axis)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
  grid(lwd = 2)
  lines(x, var, col='brown')
  title(paste('Run Sequence Plot', var_name), family='HersheySerif', cex.main=1.9, line=0.75)
  
  #Lag Plot
  par(mar = c(4,7.5,2,5),  family='HersheySerif')
  plot(0, type='n', xlim=c(min(y_axis), max(y_axis)), ylim=c(min(y_axis), max(y_axis)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
  grid(lwd = 2)
  points(y, y_lag, pch=4, col='brown')
  title(paste('Lag Plot', var_name), family='HersheySerif', cex.main=1.9, line=0.75)
  
  #Histogram
  graphics::hist(var, main = paste("Histogram of" , var_name), border='brown', cex.axis=1.3,cex.main=1.9, line=0.75, las=1, xlab=' ', ylab=' ')
  
  #Qplot
  p <- ppoints(length(var))
  q <- quantile(var, p=p)
  
  qqplot(x=q, y=qnorm(p), main = paste("Normal QQ Plot", var_name), cex.main=1.9, plot.it = TRUE,  pch=21, col='brown', bg='grey', bty='n', 
         xlab = ' ', ylab = ' ', cex.axis=1.3, las=1, axes=TRUE)
  grid(lwd = 2)
  qqline(q, distribution=qnorm, datax=TRUE, col='black', lty=2, lwd=2)
  
  rm(layout.matrix, x, x_axis, y_axis, y, y_lag, p, q)
  dev.off()
  
  g <- magick::image_read('p_1.png')
  file.remove('p_1.png')
  p_1 <- ggplotify::as.ggplot(g)
  rm(g)
  
  #writing objects to the Global Environment
  plot <- assign(x = paste(var_name, '4Plot'), value = p_1, .GlobalEnv) 
}

#four_plot(equation_list[[2]]$`Ae′`, "Ae'")

#***********************************************************   -Dotplot Style 6Plot-   ***************************************************************#

util$six_plot <- function(pred, resid, outcome,covariate,x_lab, y_lab) {
  png("p_1.png", width = 877, height = 676)
    #Plot Elements
    y_axis <- pretty(pred)
    x_axis <- pretty(covariate)
    resid_y <- resid[c(1:(length(resid)-1))]
    resid_lag <- resid[2:length(resid)]
  
    layout.matrix <- matrix(c(1, 2, 3,4,5,6), nrow = 2, ncol = 3)
    layout(mat = layout.matrix,
         heights = c(2, 2, 2), # Heights of the two rows
         widths = c(2, 2, 2)) # Widths of the two columns
  
    #Response & Predicted Values
    par(mar = c(5,5,2,2),  family='HersheySerif')
    plot(0, type='n', xlim=c(min(x_axis), max(x_axis)), ylim=c(min(y_axis), max(y_axis)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
    grid(lwd = 2)
  
    points(covariate, outcome, pch=4, col='brown')
  
    plot_fit <- glm(pred~covariate)
    co <- coef(plot_fit)
    abline(plot_fit, col="black", lwd=2)
  
    title(paste('Response & Predicted Values'), family='HersheySerif', cex.main=1.9, line=0.75)
  
    mtext(side = 1, text = x_lab, at = median(x_axis), col = "black", line = 3, cex = 1.3, family='HersheySerif')
    mtext(side = 2, text = y_lab, at = median(y_axis), col = "black", line = 3, cex = 1.3, family='HersheySerif')
  
    #Lag Plot of Residuals
    par(mar = c(4,5,2,2),  family='HersheySerif')
    plot(0, type='n', xlim=c(min(resid_lag), max(resid_lag)), ylim=c(min(resid_y), max(resid_y)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
    grid(lwd = 2)
  
    points(resid_lag, resid_y, pch=4, col='brown')
  
    title(paste('Lag Plot of Residuals'), family='HersheySerif', cex.main=1.9, line=0.25)
  
    mtext(side = 1, text = 'Residuals (i-1)', at = median(resid_lag), col = "black", line = 3, cex = 1.3, family='HersheySerif')
    mtext(side = 2, text = 'Residuals', at = median(resid_y), col = "black", line = 3, cex = 1.3, family='HersheySerif')
  
    #Residuals vs. Independent Variable
    par(mar = c(5,5,2,2),  family='HersheySerif')
    plot(0, type='n', xlim=c(min(covariate), max(covariate)), ylim=c(min(resid), max(resid)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
    grid(lwd = 2)
  
    points(covariate, resid, pch=4, col='brown')
  
    mtext(side = 1, text = x_lab, at = median(covariate), col = "black", line = 3, cex = 1.3, family='HersheySerif')
    mtext(side = 2, text = 'Residuals', at = median(resid), col = "black", line = 3, cex = 1.3, family='HersheySerif')
  
    title(paste('Residuals vs. Independent Variable'), family='HersheySerif', cex.main=1.9, line=0.75)
  
    #Histogram
    graphics::hist(resid, main = paste("Histogram of Residuals"), border='brown', cex.axis=1.3,cex.main=1.9, line=0.25, las=1, xlab=' ', ylab=' ')
  
    #Residuals vs. Predicted Values
    par(mar = c(5,5,2,2),  family='HersheySerif')
    plot(0, type='n', xlim=c(min(pred), max(pred)), ylim=c(min(resid), max(resid)), xlab=' ', ylab=' ', cex.axis=1.3, las=1,family='HersheySerif', bty='n')
    grid(lwd = 2)
  
    points(pred, resid, pch=4, col='brown')
  
    mtext(side = 1, text =paste('Predicted', y_lab), at = median(pred), col = "black", line = 3, cex = 1.3, family='HersheySerif')
    mtext(side = 2, text = 'Residuals', at = median(resid), col = "black", line = 3, cex = 1.3, family='HersheySerif')
  
    title(paste('Residuals vs. Predicted Values'), family='HersheySerif', cex.main=1.9, line=0.75)
  
    #Normal Probability Plot
    p <- ppoints(length(resid))
    q <- quantile(resid, p=p)
  
    qqplot(x=q, y=qnorm(p), main = paste("Normal Probability Plot"), cex.main=1.9, plot.it = TRUE,  pch=21, col='brown', bg='grey', bty='n', 
           xlab = ' ', ylab = ' ', line=0.25,cex.axis=1.3, las=1, axes=TRUE)
    grid(lwd = 2)
    qqline(q, distribution=qnorm, datax=TRUE, col='black', lty=2, lwd=2)
  
    mtext(side = 1, text =paste('Theoretical Value'), at = median(q), col = "black", line = 3, cex = 1.3, family='HersheySerif')
    mtext(side = 2, text = 'Ordered Residuals', at = median(p), col = "black", line = 3, cex = 1.3, family='HersheySerif')
  dev.off()
  
  g <- magick::image_read('p_1.png')
  file.remove('p_1.png')
  p_1 <- ggplotify::as.ggplot(g)
  rm(g)
  
  #writing objects to the Global Environment
  plot <- assign(x = paste(y_lab, '6Plot'), value = p_1, .GlobalEnv)
  
  rm(layout.matrix,y_axis, x_axis, resid_y, resid_lag, co, p, q)
}

#six_plot(predictions$pred_medians, predictions$resid, data$ae_fundamentals, data$ae_transients, c("Ae"), c("Ae'"))

#******************************************************   -Principle Component Analysis-   ***********************************************************#
#Tutorial prepared by Benjamin Bell: https://www.benjaminbell.co.uk/2018/02/principal-components-analysis-pca-in-r.html?m=1

#ma.pollen.raw <- read.csv("~/Desktop/Morocco Pollen Surface Sample Data.csv", header=TRUE, row.names=1, sep=",", 
#                          check.names=FALSE)

# Formatting Data
#ma.pollen <- ma.pollen.raw[-1:-4]             # Remove the first four columns
#ma.pollen <- ma.pollen[-49:-51]               # Remove total AP/NAP and pollen concentration columns
#ma.sum <- colSums(ma.pollen)                  # Calculate column sums
#ma.pollen1 <- ma.pollen[, which(ma.sum > 10)] # Subset data

# PCA using base function - prcomp()
#p <- prcomp(ma.pollen1, scale=TRUE)

# Summary
#s <- summary(p)
#unclass(p)

# Screeplot
#layout(matrix(1:2, ncol=2))
#screeplot(p)
#screeplot(p, type="lines")

#Producing biplot
#biplot(p)

#Create groups
#pch.group <- c(rep(21, times=16), rep(22, times=14), rep(24, times=3))
#col.group <- c(rep("skyblue2", times=16), rep("gold", times=14), rep("green2", times=3))

# Plot individuals
#plot(p$x[,1], p$x[,2], xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
#     ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""), 
#     pch=pch.group, col="black", bg=col.group, cex=2, las=1, asp=1)

#Add grid lines
#abline(v=0, lty=2, col="grey50")
#abline(h=0, lty=2, col="grey50")

#Add labels
#text(p$x[,1], p$x[,2], labels=row.names(p$x), pos=c(1,3,4,2), font=2)

#Get co-ordinates of variables (loadings), and multiply by 10
#l.x <- p$rotation[,1]*10
#l.y <- p$rotation[,2]*10

#Draw arrows
#arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=1.5)

#Label position
#l.pos <- l.y # Create a vector of y axis coordinates
#lo <- which(l.y < 0) # Get the variables on the bottom half of the plot
#hi <- which(l.y > 0) # Get variables on the top half

#Replace values in the vector
#l.pos <- replace(l.pos, lo, "1")
#l.pos <- replace(l.pos, hi, "3")

#Variable labels
#text(l.x, l.y, labels=row.names(p$rotation), col="red", pos=l.pos)

# Add legend
#legend("topleft", legend=c("Tislit", "Sidi Ali", "Michliffen"), col="black", 
#       pt.bg=c("skyblue2", "gold", "green2"), pch=c(21, 22, 24), pt.cex=1.5)

#Adding 95% confidence ellipses

#tab <- matrix(c(p$x[,1], p$x[,2]), ncol=2)     #Get individuals (observations) as a matrix

#Calculate correlations
#c1 <- cor(tab[1:16,])
#c2 <- cor(tab[17:30,])
#c3 <- cor(tab[31:33,])

#Load package
#library(ellipse)

#Plot ellipse
#polygon(ellipse(c1*(max(abs(p$rotation))*1), centre=colMeans(tab[1:16,]), level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
#polygon(ellipse(c2*(max(abs(p$rotation))*1), centre=colMeans(tab[17:30,]), level=0.95), col=adjustcolor("gold", alpha.f=0.25), border="gold2")
#polygon(ellipse(c3*(max(abs(p$rotation))*1), centre=colMeans(tab[31:33,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="green")

# Plot biplot
#par(mar=c(4.5, 4.5, 1, 1))
#plot(p$x[,1], p$x[,2], xlim=c(-4.5, 4.5), ylim=c(-4.5, 4.5), xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = "", asp=1), 
#     pch=pch.group, col="black", bg=col.group, cex=2.5, cex.axis=1.5, cex.lab=1.5, las=1,
#     panel.first= {
#       polygon(ellipse(c1*(max(abs(p$rotation))*1), centre=colMeans(tab[1:16,]), level=0.95), col=adjustcolor("skyblue2", alpha.f=0.25), border="skyblue")
#       polygon(ellipse(c2*(max(abs(p$rotation))*1), centre=colMeans(tab[17:30,]), level=0.95), col=adjustcolor("gold", alpha.f=0.25), border="gold2")
#       polygon(ellipse(c3*(max(abs(p$rotation))*1), centre=colMeans(tab[31:33,]), level=0.95), col=adjustcolor("green2", alpha.f=0.25), border="green")
#       abline(v=0, lty=2, col="grey50") 
#       abline(h=0, lty=2, col="grey50")
#     },
#    panel.last=arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red3", length=0.1, lwd=1.5))

#Labels
#text(p$x[,1], p$x[,2], labels=row.names(p$x), pos=c(1,3,4,2), font=2)
#text(l.x, l.y, labels=row.names(p$rotation), col="red", pos=l.pos)

# Add legend
#legend("topleft", legend=c("Tislit", "Sidi Ali", "Michliffen"), title="Sample Area", inset=c(0.01, 0.01), col="black", 
#       pt.bg=c("skyblue2", "gold", "green2"), pch=c(21, 22, 24), pt.cex=2, cex=1.5, bty="n")


#Load package
#library("factoextra")

#Create groups
#group <- c(rep("Tislit", times=16), rep("Sidi Ali", times=14), rep("Michliffen", times=3))

#Plot
#fviz_pca_biplot(p, repel=TRUE, pointsize=6, pointshape=21, col.var="red", arrowsize=0.6, labelsize=5, 
#                col.ind=group, palette=c("green2", "gold", "skyblue2"), addEllipses=TRUE, ellipse.type="confidence")

#LOOKING AT LOADING: DIAGNOSTICS
#p <- prcomp(ma.pollen1, scale=TRUE)
#s <- summary(p) # Summary

#barplot(p$rotation[,1], main="PC 1 Loadings Plot", las=2)

#n.pc1 <- ifelse(p$rotation[,1] > 0, yes=-0.01, no=p$rotation[,1]-0.01)

#c.pc1 <- ifelse(p$rotation[,1] > 0, yes="green2", no="red2")

#par(mar=c(8,3,2,1)) # Set margins
#b1 <- barplot(p$rotation[,1], main="PC 1 Loadings Plot", col=c.pc1, las=2, axisnames=FALSE)
#abline(h=0) # Add horizontal line
#text(x=b1, y=n.pc1, labels=names(p$rotation[,1]), adj=1, srt=90, xpd=TRUE) # Add variable names

#Change colour of bar plot
#c.pc1 <- ifelse(p$rotation[,1] > 0, yes="green2", no="red2")
#c.pc2 <- ifelse(p$rotation[,2] > 0, "green2", "red2")

#Get position for variable names
#n.pc1 <- ifelse(p$rotation[,1] > 0, -0.01, p$rotation[,1]-0.01)
#n.pc2 <- ifelse(p$rotation[,2] > 0, -0.01, p$rotation[,2]-0.01)

#Plot
#layout(matrix(1:2, ncol=1)) # Set up layout
#par(mar=c(1,3,2,1), oma=c(7.5,0,0,0)) # Set up margins

#Plot PC 1
#b1 <- barplot(p$rotation[,1], main="PC 1 Loadings Plot", col=c.pc1, las=2, axisnames=FALSE)
#abline(h=0)

#Plot PC 2
#b2 <- barplot(p$rotation[,2], main="PC 2 Loadings Plot", col=c.pc2, las=2, axisnames=FALSE)
#abline(h=0)
# Add variable names
#text(x=b2, y=ifelse(p$rotation[,2] > 0, -0.01, p$rotation[,2]-0.01), labels=names(p$rotation[,2]), adj=1, srt=90, xpd=NA)

#Get individuals (observations), and create group matrices
# <- matrix(c(p$x[,1], p$x[,2]), ncol=2)
#tab1 <- tab[1:16,]
#tab2 <- tab[17:30,]
#tab3 <- tab[31:33,]

#Get the outer most individuals
#ch1 <- chull(tab1) 
#ch2 <- chull(tab2)
#ch3 <- chull(tab3)

#Create groups
#pch.group <- c(rep(21, times=16), rep(22, times=14), rep(24, times=3))
#col.group <- c(rep("skyblue2", times=16), rep("gold", times=14), rep("green2", times=3))

#Plot
#par(mar=c(4.5, 4.5, 1, 1))
#plot(p$x[,1], p$x[,2], xlim=c(-4.5, 4.5), ylim=c(-4.5, 4.5), xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""), 
#     pch=pch.group, col="black", bg=col.group, cex=2.5, cex.axis=1.5, cex.lab=1.5, las=1,
#     panel.first= {
#       #Add convex hulls
#       polygon(tab1[ch1, ], border="darkblue", col=NA, lwd=2)
#       polygon(tab2[ch2, ], border="gold4", col=NA, lwd=2)
#       polygon(tab3[ch3, ], border="darkgreen", col=NA, lwd=2)
#       # Add grid lines
#      abline(v=0, lty=2, col="grey50") 
#       abline(h=0, lty=2, col="grey50")
#     })

#Labels
#text(p$x[,1], p$x[,2], labels=row.names(p$x), pos=c(1,3,4,2), font=2)

#Create matrix of x coordinates (PC1) and multiply by 10
#l.x <- cbind(p$rotation[,1][c(15, 6, 5, 4)]) * 10
# y coordinates (PC2)
#l.y <- cbind(p$rotation[,2][c(15, 6, 5, 4)]) * 10

#Add arrows to biplot
#arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="red", length=0.15, lwd=3)

#Labels
#text(l.x, l.y, labels=rownames(l.x) , col="red", pos=c(3, 1, 3, 1), offset=1, cex=1.2)

#Specific labels
#n.x <- cbind(p$x[,1][c(16, 28, 33)]) # x coordinates
#n.y <- cbind(p$x[,2][c(16, 28, 33)]) # y coordinates

#Add labels to biplot
#text(n.x, n.y, labels=rownames(n.x), pos=c(4, 3, 3), offset=1.5, cex=1.5, font=2)

#Over-plot symbols
#points(n.x, n.y, pch=c(21, 22, 24), bg=c("skyblue2", "gold", "green2"), col="deeppink", cex=4, lwd=3)

#********************************************************   -Multiple Fit Lines-   *******************************************************************#
#library(zoo)

#Creating Data
#time <- 0:100
#temp <- 20+ 0.01 * time^2 + 0.8 * time + rnorm(101, 0, 5)

#Calculating Running Average
#mov.avg <- rollmean(temp, 5, fill=NA)

# Generate first order linear model
#lin.mod <- lm(temp~time)

# Generate second order linear model
#lin.mod2 <- lm(temp~I(time^2)+time)

# Calculate local regression (If doing a serious fit, look at the linear model to examine the relative weighting of relative to global)
#ls <- loess(temp~time)

#Fitting Line
#pr.lm <- predict(lin.mod)
#pr.lm2 <- predict(lin.mod2)
#pr.loess <- predict(ls)

#Plotting
#par(mfrow=c(2,2), mar=c(3, 3, 2, 1))
#plot(time, temp, "l")
#lines(time, mov.avg, col="orange", lwd=2)
#title('Running Average', line= - 0.25, family='serif', font.main=2,  cex.main = 1, adj=0)

#plot(time, temp, "l", las=1, xlab="Time", ylab="Temperature")
#lines(pr.lm~time, col="blue", lwd=2)
#title('Linear Regression', line= - 0.25, family='serif', font.main=2,  cex.main = 1, adj=0)

#plot(time, temp, "l", las=1, xlab="Time", ylab="Temperature")
#lines(pr.lm2~time, col="green", lwd=2)
#title('Linear Regression (2nd Order)', line= - 0.25, family='serif', font.main=2,  cex.main = 1, adj=0)

#plot(time, temp, "l", las=1, xlab="Time", ylab="Temperature")
#lines(pr.loess~time, col="red", lwd=2)
#title('LOESS', line= - 0.25, family='serif', font.main=2,  cex.main = 1, adj=0)

#*******************************************   -Fitted Line with Confidence Interval (Poly Fit)-   ***************************************************#
#library(MASS)
#attach(Boston)

#Fitting Data from MASS
#lm.fit2 = lm(medv~poly(lstat,2))

#Plotting
#alpha = 150 # Transparent points
#medv_1 = jitter(medv)
#lstat_1 = jitter(lstat)

#par(mar=c(3, 3, 3, 1), mgp = c(2, 0.4, 0), xaxs = "i", yaxs = "i", bty = "n", las=1)
#plot(x=lstat_1 , y=medv_1, bg = 'gray', col='black', pch=21, family='serif', xlab = NA, ylab = NA, 
#     ylim=c(min(medv_1), max(medv_1)))

#ax_ticks <- axTicks(2)
#at_2 <- median(ax_ticks)

#Adding Labels here because I jittered the points
#mtext("lsat", side = 1, line = 2, cex = 1.5, family='serif')

#par(las=0)
#axis(side=2, at=at_2, label="mdv", line=1.5, tick = FALSE, cex.axis = 1.5, family='serif')

#Getting Confidence Intervals
#new.lstat = seq(min(lstat), max(lstat), length.out=100)

#Generating Prediction Line
#preds <- predict(lm.fit2, newdata = data.frame(lstat=new.lstat), interval = 'prediction')

#Plotting Prediction Line
#lines(sort(lstat), fitted(lm.fit2)[order(lstat)], col='red', lwd=3) 

#Creating Polygon to Fill the Confidence Interval
#polygon(c(rev(new.lstat), new.lstat), c(rev(preds[ ,3]), preds[ ,2]), density=10, col = 'blue', border = NA)

#Creating Borders for the Confidence Interval
#lines(new.lstat, preds[ ,3], lty = 'dashed', col = 'red')
#lines(new.lstat, preds[ ,2], lty = 'dashed', col = 'red')

#Adding Title
#title('Example of a Fitted Line', line=-0.25, family='serif', font.main=2,  cex.main = 2)

#rm(medv_1, lstat_1, at_2, ax_ticks)

#***************************************************   -ggplot2 Smoothed Regression Line-   ***********************************************************#
#library(ggplot2)      #Primary Visualization Package

#Define the model
#model <- loess(wt ~ hp, data = mtcars)

#Predict fitted values for each observation in the original dataset
#modelFit <- data.frame(predict(model, se = TRUE))

#Define data frame for ggplot
#df <- data.frame(cbind(hp = mtcars$hp, wt = mtcars$wt, fit = modelFit$fit, upperBound = modelFit$fit + 2 * modelFit$se.fit
#                       , lowerBound = modelFit$fit - 2 * modelFit$se.fit))

#Plotting
  #Build the plot using the fitted values from the predict() function
  #geom_linerange() and the second geom_point() in the code are built using the values from the predict() function

#ggplot(df, aes(x=hp, y=wt)) + 
#     theme(panel.background = element_rect(fill = 'white', linetype = 1),
#        panel.grid.minor = element_line(colour = 'snow3'),
#        plot.title = element_text(size = 14, face = "bold"), 
#        axis.text.x = element_text(colour="grey20",size=12),
#       axis.title.x = element_text(colour="grey20",size=14),
#        axis.text.y = element_text(colour="grey20",size=12),
#        axis.title.y = element_text(colour="grey20",size=16),
#        text = element_text(family="Times"),
#        axis.ticks.length = unit(0.25, "cm")) +  
#        guides(size=FALSE) +
#  geom_point(aes(hp, fit, size=1)) +
#  geom_point(alpha=.5) +
#  geom_linerange(aes(ymin = lowerBound, ymax = upperBound)) +
#  geom_smooth(method = "loess", alpha=0.5, fill='cornsilk') +
#  geom_line(aes(x = hp, y = lowerBound), linetype = 2, col='red') +
#  geom_line(aes(x = hp, y = upperBound), linetype = 2, col= 'red') +
#  base_breaks_x(df$hp) +
#  base_breaks_y(df$wt) +
#  labs(y = "Weight", x = "Horsepower") +
#  ggtitle("Example Predicted Values Plot") +
#  theme(plot.margin = unit(c(1,1,0,1), "cm"))

#******************************************   -Johnson-Neyman Intervals for 2-Way Interactions-   ********************************************************#

#The predictions package provides Johnson-Neyman Plots for Ealuating Ineractions: https://cran.r-project.org/web/packages/interactions/readme/README.html
#library(interactions)
#In the interactions package, values beyond those obserced are simulated and a cofidence interval generated.
#These functions operate based exclusively on the observed data.
#To replicate the interactions package functionality, a simulation steps would first need to be done.

#Plotting the a JNT for Conditional Effects on Theta (Interaction Case): https://rpubs.com/bachl/jn-plot
#Marko Bachl
#March 2015

#Getting Data
#library(jtools) # for summ()
#states <- as.data.frame(state.x77)

#Fitting Model
#income.lm <- lm(Income ~ Illiteracy * Murder, data = states)
#fiti <- lm(mpg ~ hp * wt, data = mtcars)
#summary(income.lm)

require(foreign)
require(dplyr)

util$jnt <- function(.lm, predictor, moderator, alpha=.05) {
  require(stringi)
  b1 = coef(.lm)[predictor]
  b3 = coef(.lm)[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))]
  se_b1 = coef(summary(.lm))[predictor, 2]
  se_b3 = coef(summary(.lm))[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor)), 2]
  COV_b1b3 = vcov(.lm)[predictor, stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))]
  t_crit = qt(1-alpha/2, .lm$df.residual)
  # see Bauer & Curran, 2005
  a = t_crit^2 * se_b3^2 - b3^2
  b = 2 * (t_crit^2 * COV_b1b3 - b1 * b3)
  c = t_crit^2 * se_b1^2 - b1^2
  jn = c(
    (-b - sqrt(b^2 - 4 * a * c)) / (2 * a),
    (-b + sqrt(b^2 - 4 * a * c)) / (2 * a)
  )
  JN = sort(unname(jn))
  JN = JN[JN>=min(.lm$model[,moderator]) & JN<=max(.lm$model[,moderator])]
  JN
}

#jnt(income.lm, predictor = "Illiteracy", moderator = "Murder", alpha = .05)
#jnt(fiti, predictor = "hp", moderator = "wt", alpha = .05)

util$theta_plot <- function(.lm, predictor, moderator, alpha=.05, jn=F) {
  require(dplyr)
  require(ggplot2)
  require(stringi)
  .data = data_frame(b1 = coef(.lm)[predictor],
                     b3 = coef(.lm)[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))],
                     Z = quantile(.lm$model[,moderator], seq(0,1,.01)),
                     theta = b1 + Z * b3,
                     se_b1 = coef(summary(.lm))[predictor, 2],
                     COV_b1b3 = vcov(.lm)[predictor, stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor))],
                     se_b3 = coef(summary(.lm))[stri_startswith_fixed(names(coef(.lm)), paste0(predictor,":")) | stri_endswith_fixed(names(coef(.lm)), paste0(":",predictor)), 2],
                     se_theta = sqrt(se_b1^2 + 2 * Z * COV_b1b3 + Z^2 * se_b3^2),
                     ci.lo_theta = theta+qt(alpha/2, .lm$df.residual)*se_theta,
                     ci.hi_theta = theta+qt(1-alpha/2, .lm$df.residual)*se_theta)
  
  if (jn==TRUE) {
    JN = util$jnt(.lm=.lm, predictor=predictor, moderator=moderator, alpha=alpha)
    JN_lines = geom_vline(xintercept=JN, linetype=2)
    JN_regions = ifelse(length(JN) == 0, "no significance regions", paste(round(JN,2), collapse = "; "))
    Xlab = paste0(moderator, " (JN Significance Regions: ", JN_regions,")")
  } else {
    Xlab = moderator
    JN_lines = NULL
  }
  
  #Generating Plot
  layout(rbind(1,2), heights=c(9,1)) 
  par(mar=c(3, 3, 3, 1), mgp = c(2, 0.4, 0), xaxs = "i", yaxs = "i", bty = "n", las=1)
  plot(x=.data$Z , y=.data$theta, type="l", lty=1, lwd=2.5, col='blue', family='serif', xlab = NA, ylab = NA, panel.first = grid(),
       ylim=c(min(.data[[9]]), max(.data[[10]])))
  
  #Creating Polygon to Fill the Confidence Interval
  if (length(JN)!= 1) {
    JN_1 <- .data[.data$Z > 0 & .data$Z <= min(JN), ]
    polygon(c(JN_1$Z,rev(JN_1$Z)),c(JN_1[[10]],rev(JN_1[[9]])),col="cornsilk", border = NA)
  } else {
    JN_1 <- NULL
  }
  
  JN_2 <- .data[.data$Z >= max(JN), ]
  polygon(c(JN_2$Z,rev(JN_2$Z)),c(JN_2[[10]],rev(JN_2[[9]])),col="cornsilk", border = NA)
  polygon(c(.data$Z, .data$Z), c(.data[[10]], .data[[9]]), density=9, col = 'blue', border = NA)
  
  #Plotting Prediction Line
  lines(.data$Z, .data$theta, col='blue', lty= 1, lwd=2.5) 
  
  #Adding Confidence Bands
  lines(.data$Z, .data[[9]], lty = 'dashed', col = 'red')
  lines(.data$Z, .data[[10]], lty = 'dashed', col = 'red')
  
  #Adding Theta Reference Line
  abline(h=0, col="black", lty=3, lwd=1.5)
  
  #Adding JN Lines if Called
  if (c("gg") %in% class(JN_lines)  ) {
    
    for (i in seq_along(JN)){
      abline(v=JN[[i]], col="black", lty=3, lwd=1.5)
    }
  } else {
    JN_lines == JN_lines
  }
  
  #Adding Axes Text
  mtext(expression(theta), side = 2, line = 2, cex = 1.5, family='serif')
  mtext(Xlab, side = 1, line = 2, cex = 1.5, family='serif')
  
  #Adding Title
  title(paste("Conditional Effect of", predictor, "as Function of", moderator), line=0.90, family='serif', font.main=2,  cex.main = 1.5)
  
  #Adding Legend
  par(mar=c(0, 3, 0, 0), family='serif')
  plot.new()
  legend('left','groups',c("not signicant"), pch=0, pt.bg=NA, col='black', 
         cex=1.5, bty ="n", ncol=1)
}

#theta_plot(income.lm, predictor = "Illiteracy", moderator = "Murder", jn = T)
#theta_plot(fiti, predictor = 'hp', moderator = "wt", jn=T)

#COMPARING GROUPS
#Function Created by Ken Toyama and generalized by me: https://kenstoyama.wordpress.com/2018/01/21/the-johnson-neyman-tecnique-and-an-r-script-to-apply-it/
#The Function Below is for Johnson-Neyman Confidence Intervals for ANCOVA settings (Two groups, Same Relationship)


#Generating some Fake Data
#d <- data.frame(
#  state = rep(c('NY', 'CA'), 30),
#  year = rep(1:30, 2),
#  response= rnorm(30)
#)

#data_1 <- d[d$state == "NY", ]
#year_1 <- data_1$year
#response_1 <- data_1$response

#data_2 <- d[d$state == "CA", ]
#year_2 <- data_2$year
#response_2 <- data_2$response

#Fitting Data
#pred_1 = lm(response_1~poly(year_1,2))
#pred_2 = lm(response_2~poly(year_2,2))

#Plotting Assessment Plot
#alpha = 150 # Transparent points

#par(mar=c(3, 3, 3, 1), mgp = c(2, 0.4, 0), xaxs = "i", yaxs = "i", bty = "n", las=1)
#plot(x=year_1, y=response_1, bg = 'gray', col='black', pch=21, family='serif', xlab = NA, ylab = NA, 
#      ylim=c(min(d$response), max(d$response)))

#ax_ticks <- axTicks(2)
#at_2 <- median(ax_ticks)

#Adding Labels here because I jittered the points
#mtext("Year", side = 1, line = 2, cex = 1.5, family='serif')

#par(las=0)
#axis(side=2, at=at_2, label="Response", line=1.5, tick = FALSE, cex.axis = 1.5, family='serif')

#Getting Confidence Intervals
#new.year_1 = seq(min(year_1), max(year_1), length.out=100)

#Generating Prediction Line
#p_1 <- predict(pred_1, newdata = data.frame(year_1=new.year_1), interval = 'prediction')

#Plotting Prediction Line
#lines(sort(year_1), fitted(pred_1)[order(year_1)], col='black', lwd=3) 

#Adding California
#par(new = T, mar=c(3, 3, 3, 1), bty='n', xaxs = "i", yaxs = "i")
#plot(x=year_2, y=response_2, col='black', pch=16, family='serif', xlab = NA, ylab = NA, 
#     ylim=c(min(d$response), max(d$response)), axes = FALSE)

#Getting Confidence Intervals
#new.year_2 = seq(min(year_2), max(year_2), length.out=100)

#Generating Prediction Line
#p_2 <- predict(pred_2, newdata = data.frame(year_2=new.year_2), interval = 'prediction')

#Plotting Prediction Line
#lines(sort(year_2), fitted(pred_2)[order(year_2)], col='black', lty=2, lwd=3) 

#rm(pred_1, pred_2, p_1, p_2, new.year_1, new.year_2)

#labels <- c('Year', 'Response')
#JohnsonNeyman_GC(year_1, year_2, response_1, response_2, labels)

#Generating Johnson-Neyman Intervals for Two Groups
util$JohnsonNeyman_GC <- function(X1, X2, Y1, Y2, labels){ 
  #Defining Minimums and Maximums for the Purposes of Rescaling
  X1_viz <- X1
  X2_viz <- X2
  Y1_viz <- Y1
  Y2_viz <- Y2
  
  #Scaling to a positive scale for the purposes of logging later.
  X1 <- X1 + (abs(min(X1)) + 1)
  X2 <- X2 + (abs(min(X2)) + 1)
  Y1 <- Y1 + (abs(min(Y1)) + 1)
  Y2 <- Y2 + (abs(min(Y2)) + 1)
  
  n1 <- as.numeric(length(X1))
  n2 <- as.numeric(length(X2))

  #Running Regressions
  reg1 <- lm(log(Y1)~log(X1))
  reg2 <- lm(log(Y2)~log(X2))
  coef1 <- reg1$coefficients
  coef2 <- reg2$coefficients
  Fcvalue <- qf(.95, df1=1, df2=n1+n2-4)
  xmean1 <- mean(log(X1))
  xmean2 <- mean(log(X2))
  xmeansq1 <- mean((log(X1))^2)
  xmeansq2 <- mean((log(X2))^2)
  sumx1 <- 0
  sumx2 <- 0
  sumy1 <- 0
  sumy2 <- 0
  sumxy1 <- 0
  sumxy2 <- 0
  
  xcoord_group1 <- log(X1) # X1
  ycoord_group1 <- log(Y1) # Y1
  
  xcoord_group2 <- log(X2) # X2
  ycoord_group2 <- log(Y2) # Y2
  
  zx1 <- ((sum(xcoord_group1))^2)/n1
  zx2 <- ((sum(xcoord_group2))^2)/n2
  zy1 <- ((sum(ycoord_group1))^2)/n1
  zy2 <- ((sum(ycoord_group2))^2)/n2
  zxy1 <- ((sum(xcoord_group1))*(sum(ycoord_group1)))/n1
  zxy2 <- ((sum(xcoord_group2))*(sum(ycoord_group2)))/n2
  
  ######## sumx1 ########
  c <- 0
  while (c<n1) {
    sumx1 <- sumx1 + (((xcoord_group1[c+1])^2) - zx1)
    c <- c+1
  }
  sumx1
  
  ######## sumx2 ########
  c <- 0
  while (c<n2) {
    sumx2 <- sumx2 + (((xcoord_group2[c+1])^2) - zx2)
    c <- c+1
  }
  sumx2
  
  ######## sumy1 ########
  c <- 0
  while (c<n1) {
    sumy1 <- sumy1 + (((ycoord_group1[c+1])^2) - zy1)
    c <- c+1
  }
  sumy1
  
  ######## sumy2 ########
  c <- 0
  while (c<n2) {
    sumy2 <- sumy2 + (((ycoord_group2[c+1])^2) - zy2)
    c <- c+1
  }
  sumy2
  
  ######## sumxy1 ########
  c <- 0
  while (c<n1) {
    sumxy1 <- sumxy1 + (((xcoord_group1[c+1])*(ycoord_group1[c+1]))-zxy1)
    c <- c+1
  }
  sumxy1
  
  ######## sumxy2 ########
  c <- 0
  while (c<n2) {
    sumxy2 <- sumxy2 + (((xcoord_group2[c+1])*(ycoord_group2[c+1]))-zxy2)
    c <- c+1
  }
  sumxy2
  
  ######## SSres ########
  
  SSres <- (sumy1-(((sumxy1)^2)/sumx1))+(sumy2-(((sumxy2)^2)/sumx2))
  SSres
  
  ######## ABC ########
  a1 <- coef1[1]
  a2 <- coef2[1]
  b1 <- coef1[2]
  b2 <- coef2[2]
  A <- (-Fcvalue/(n1+n2-4))*(SSres)*((1/sumx1)+(1/sumx2))+((b1-b2)^2)
  B <- (Fcvalue/(n1+n2-4))*(SSres)*((xmean1/sumx1)+(xmean2/sumx2))+((a1-a2)*(b1-b2))
  C <- (-Fcvalue/(n1+n2-4))*(SSres)*(((n1+n2)/(n1*n2))+(xmeansq1/sumx1)+(xmeansq2/sumx2))+((a1-a2)^2)
  
  ########################
  
  xlower <- (-B-sqrt((B^2)-A*C))/A
  xupper <- (-B+sqrt((B^2)-A*C))/A
  
  #Recovering Original Scales for the Purposes of Visualization
  x_full <- unique(append(X1, X2))
  x_lower <- xlower/max(x_full)
  x_upper <- xupper/max(x_full)
  
  X_viz <- unique(append(X1_viz, X2_viz))
  Y_viz <- unique(append(Y1_viz, Y2_viz))
  
  xlower <- x_lower * max(X_viz)
  xupper <- x_upper * max(X_viz)
  
  #Plotting Johnson-Neyman Confidence Intervals
  par(mar=c(3, 3, 3, 1), mgp = c(2, 0.4, 0), xaxs = "i", yaxs = "i", bty = "n", las=1)
  plot(0, type = 'n', col='black', pch=21, family='serif', xlab = NA, ylab = NA, 
       xlim=c(min(X_viz), max(X_viz)), ylim=c(min(Y_viz), max(Y_viz)))
  
  points(X1_viz,Y1_viz, pch=21 ,col="black", bg = 'gray')
  points(X2_viz,Y2_viz, pch=16 ,col="black")
  
  #Adding Johnson-Neyman Confidence Interval
  polygon(c(xlower,xlower,xupper,xupper),c(min(Y_viz), max(Y_viz), max(Y_viz), min(Y_viz)),
          col=rgb(224, 224, 224,maxColorValue=255,alpha=130), border=NA)
  abline(v=xlower,lty=1, lwd=0.5)
  abline(v=xupper,lty=1, lwd=0.5)
  
  #Fitting Data
  pred_1 = lm(Y1_viz~poly(X1_viz,2))
  pred_2 = lm(Y2_viz~poly(X2_viz,2))
  
  #Getting Confidence Intervals
  new.X1_viz = seq(min(X1_viz), max(X1_viz), length.out=100)
  
  #Generating Prediction Line
  p_1 <- predict(pred_1, newdata = data.frame(X1_viz=new.X1_viz), interval = 'prediction')
  
  #Plotting Prediction Line
  lines(sort(X1_viz), fitted(pred_1)[order(X1_viz)], col='black', lwd=3) 
  
  #Getting Confidence Intervals
  new.X2_viz = seq(min(X2_viz), max(X2_viz), length.out=100)
  
  #Generating Prediction Line
  p_2 <- predict(pred_2, newdata = data.frame(X2_viz=new.X2_viz), interval = 'prediction')
  
  #Plotting Prediction Line
  lines(sort(X2_viz), fitted(pred_2)[order(X2_viz)], col='black', lwd=3, lty=2) 
  
  ax_ticks <- axTicks(2)
  at_2 <- median(ax_ticks)
  
  #Adding Labels here because I jittered the points
  mtext(labels[[1]], side = 1, line = 2, cex = 1.5, family='serif')
  
  par(las=0)
  axis(side=2, at=at_2, label=labels[[2]], line=1.5, tick = FALSE, cex.axis = 1.5, family='serif')
  
  #Adding Title
  title('Johnson-Neyman Confidence Interval', line=.90, family='serif', font.main=2,  cex.main = 2)
}

#*************************************************   -Repeated Density Ratios-   *********************************************************************#
#Note on Density Ratios: The Savage-Dickey density ratio is calculated by dividing the value of the posterior distribution over the parameters for the 
#full model evaluated at θ=0, p(θ=0|y, m), by the prior for the same model evaluated at the same point, p(θ=0|m). 
#The interpretation is very simple: if it is less likely that parameters θ equal 0 after seeing the data (posterior) than before (prior), 
#then p(y|mi)/p(y|mF)<1 and we have evidence in favour of the full model, mF, and vice-versa.

#Essentially a very nice way of visualizing what differences in BIC scores are telling us.

#Example Comes from: http://shinyapps.org/apps/RGraphCompendium/index.php#many-density-ratios

#Prepare Data
#xbar.therapy <- 92
#s.therapy <- 8.5
#xbar.placebo <- 85
#s.placebo <- 9.1
#n <- 15
#xdiff <- xbar.therapy - xbar.placebo
#sdiff <- sqrt((s.therapy^2 + s.placebo^2)/2) * sqrt(2/n)
#sdiff <- sqrt(s.therapy^2 + s.placebo^2)/sqrt(n)

#muH0 <- 0
#muH1 <- 8

#t0 <- (xdiff - muH0)/sdiff

# Bayes Factor (or, ugliest code ever and I DO NOT CARE TO FIX IT)
#x <- seq(-15, 30, by = 0.001)
#y <- dt(x/sdiff, df = 28)

#y1 <- dt((x - 1)/sdiff, df = 28)
#y2 <- dt((x - 2)/sdiff, df = 28)
#y3 <- dt((x - 3)/sdiff, df = 28)
#y4 <- dt((x - 4)/sdiff, df = 28)
#y5 <- dt((x - 5)/sdiff, df = 28)
#y6 <- dt((x - 6)/sdiff, df = 28)
#y7 <- dt((x - 7)/sdiff, df = 28)
#y8 <- dt((x - 8)/sdiff, df = 28)
#y9 <- dt((x - 9)/sdiff, df = 28)
#y10 <- dt((x - 10)/sdiff, df = 28)

#Plotting Graph
#par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3.5, 1, 0), cex.lab = 1.5, 
#    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)
#par(mar = c(4, 4.5, 4.5, 1))

#plot(x, y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(-15, 25), 
#     lwd = 2)

#lines(x, y1, col = "grey70")
#lines(x, y2, col = "grey70")
#lines(x, y3, col = "grey70", lwd = 2)
#lines(x, y4, col = "grey70")
#lines(x, y5, col = "grey70", lwd = 2)
#lines(x, y6, col = "grey70")
#lines(x, y7, col = "grey70")
#lines(x, y8, col = "grey70", lwd = 2)
#lines(x, y9, col = "grey70")
#lines(x, y10, col = "grey70", lwd = 2)

#axis(side = 1, at = seq(-15, 30, by = 5), pos = 0, lwd = 2, cex.axis = 1.7)
#axis(side = 1, at = 7, pos = 0, col = "red4", col.axis = "red4", lwd = 2, padj = 0.1)
#abline(v = xdiff, col = "red4", lwd = 2)

#L0 <- dt((xdiff/sdiff), df = 28)
#L1 <- dt(((xdiff - 1)/sdiff), df = 28)
#L2 <- dt(((xdiff - 2)/sdiff), df = 28)
#L3 <- dt(((xdiff - 3)/sdiff), df = 28)
#L4 <- dt(((xdiff - 4)/sdiff), df = 28)
#L5 <- dt(((xdiff - 5)/sdiff), df = 28)
#L6 <- dt(((xdiff - 6)/sdiff), df = 28)
#L7 <- dt(((xdiff - 7)/sdiff), df = 28)
#L8 <- dt(((xdiff - 8)/sdiff), df = 28)
#L9 <- dt(((xdiff - 9)/sdiff), df = 28)
#L10 <- dt(((xdiff - 10)/sdiff), df = 28)

#lines(c(6.7, 7.3), y = rep(L0, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L1, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L2, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L3, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L4, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L5, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L6, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L7, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L8, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L9, 2), col = "red4", lwd = 2)
#lines(c(6.7, 7.3), y = rep(L10, 2), col = "red4", lwd = 2)

#text(-16.8, 0.35, expression(paste(H[0], " : ", mu[diff], " = 0", sep = "")), 
#     adj = 0, cex = 1.6)
#text(-16.8, 0.3, expression(paste(H[1], " : 0", "" <= mu[diff], "" <= 10), sep = ""), 
#     adj = 0, cex = 1.6)
#text(15, 0.35, expression(paste(italic("L"), "(", H[0], ") = .04")), adj = 0, 
#     col = "red4", cex = 1.6)
#text(15, 0.3, expression(paste(italic("L"), "(", H[1], ") = .10")), adj = 0, 
#     col = "red4", cex = 1.6)
#text(14.2, 0.22, expression(paste("BF = ", frac(".10", ".04"), " = ", 2.5, sep = "")), 
#     adj = 0, col = "red4", cex = 1.6)
#mtext(expression(bar(x)[diff]), side = 1, line = 2, at = 6.5, adj = 0, col = "red4", 
#      cex = 1.8, padj = 0.1)

#*****************************************************   -AUROC Analysis-   ***********************************************************************#
#Notes: Think of a regression model mapping a number of features onto a real number (potentially a probability). 
#The resulting real number can then be mapped on one of two classes, depending on whether this predicted number is greater or lower than some 
#chosen threshold. 

#Nice Blogs on this: https://rpubs.com/Wangzf/pROC
#https://classeval.wordpress.com/introduction/introduction-to-the-roc-receiver-operating-characteristics-plot/

#library(pROC)

#Getting Data
#data(aSAH)
#DT::datatable(aSAH)

#First Example: Multiple Presentation Style Depicted in the Blog (https://rpubs.com/Wangzf/pROC )
#par(bty = "n", family = 'serif')
#plot.roc(aSAH$outcome, aSAH$s100b,          # data
#         percent = TRUE,                    # show all values in percent
#         partial.auc=c(100, 90), 
#         partial.auc.correct=TRUE,          # define a partial AUC (pAUC)
#         print.auc=TRUE,                    
#         #display pAUC value on the plot with following options:
#         print.auc.pattern = "Corrected pAUC (100-90%% SP):\n%.1f%%",
#         print.auc.col = "#1c61b6",
#         auc.polygon = TRUE, 
#         auc.polygon.col = "#1c61b6",       # show pAUC as a polygon
#         max.auc.polygon = TRUE, 
#         max.auc.polygon.col = "#1c61b622", # also show the 100% polygon
#         main = "Partial AUC (pAUC)")
#plot.roc(aSAH$outcome, aSAH$s100b,
#         percent = TRUE, 
#         add = TRUE, 
#         type = "n",                        # add to plot, but don't re-add the ROC itself (useless)
#         partial.auc = c(100, 90), 
#         partial.auc.correct = TRUE,
#         partial.auc.focus = "se",          # focus pAUC on the sensitivity
#         print.auc = TRUE, 
#         print.auc.pattern = "Corrected pAUC (100-90%% SE):\n%.1f%%", 
#         print.auc.col = "#008600",
#         print.auc.y = 35,                  # do not print auc over the previous one
#         auc.polygon = TRUE, 
#        auc.polygon.col = "#008600",
#         max.auc.polygon = TRUE, 
#         max.auc.polygon.col = "#00860022")

#**************************************************   -Taylor Diagram-   **************************************************************#
#Function developed by Olivier Eterradossi with new features and bug fixes by me. 
#January 9, 2007
#https://stat.ethz.ch/pipermail/r-help/2007-January/123343.html

#Note: Taylor diagrams are mathematical diagrams designed to graphically indicate which of several approximate 
#representations (or models) of a system, process, or phenomenon is most realistic (see for more details: https://en.wikipedia.org/wiki/Taylor_diagram)

#This diagram, invented by Karl E. Taylor in 1994 (published in 2001[1]) facilitates the comparative assessment of 
#different models. It is used to quantify the degree of correspondence between the modeled and observed behavior in terms of three statistics: 
#   The Pearson correlation coefficient, 
#   The root-mean-square error (RMSE) error
#   Standard deviation

#See Plotrix for Comparison: https://www.rdocumentation.org/packages/plotrix/versions/3.7-6/topics/taylor.diagram
#This function does not include a normailization feature as that makes RMS

#Bias Arrows
#Where the length of the arrow indicates the bias and the length of dotted line joining the vector's tip to 
#the reference point is the RMS error. 
#The direction of the vector indicates the sign of the bias ..

#Creating Some Data
#ref<-rnorm(30,sd=2)  #Reference Model

#Models 1 and 2
#model1<-ref+rnorm(30)/2
#model2<-ref+rnorm(30)

util$Taylor<-function(ref, batch, add=F, model_n, couleur="red", m_bias=F, title="Taylor Diagram"){ # ref, batch : vecteurs
  x <- ref
  y <- batch
  
  grad.corr.full<-c(0,0.2,0.4,0.6,0.8,0.9,0.95,0.99,1)
  grad.corr.lines<-c(0.2,0.4,0.6,0.8,0.9)
  
  R <- cor(x,y,use="pairwise")
  
  sd.r<-sd(x)
  sd.f<-sd(y)
  maxray<-1.5*max(sd.f,sd.r)
  discrete<-seq(180,0,by=-1)
  listepoints<-NULL

  if (add==F) {   # pourtour du diagramme
    par(mar=c(1, 1, 1, 1), mgp = c(2, 0.4, 0), bty = "n")
    plot(c(-maxray,maxray),c(0,maxray),type="n",asp=1,bty="n",xaxt="n",yaxt="n",xlab="",ylab="", family='serif')
    for (i in discrete){
      listepoints<-cbind(listepoints,maxray*cos(i*pi/180),maxray*sin(i*pi/180))
    }
    listepoints<-matrix(listepoints,2,length(listepoints)/2)
    listepoints<-t(listepoints)
    lines(listepoints[,1],listepoints[,2])
    
    # axes x,y
    lines(c(-maxray,maxray),c(0,0))
    lines(c(0,0),c(0,maxray))   # lines radials jusqu'à R = +/- 0.8
    for (i in grad.corr.lines){
      lines(c(0,maxray*i),c(0,maxray*sqrt(1-i^2)),lty=3)
      lines(c(0,-maxray*i),c(0,maxray*sqrt(1-i^2)),lty=3)
    }
    # text radial
    for (i in grad.corr.full){
      text(1.05*maxray*i,1.05*maxray*sqrt(1-i^2),i,cex=0.8)
      text(-1.05*maxray*i,1.05*maxray*sqrt(1-i^2),-i,cex=0.8)
    }
  # sd concentriques autour de la reference
  seq.sd<-seq.int(0,2*maxray,by=(maxray/10))
  for (i in seq.sd){
    xcircle<-sd.r+(cos(discrete*pi/180)*i)
    ycircle<-sin(discrete*pi/180)*i
    for (j in 1:length(xcircle)){
      if 
      ((xcircle[j]^2+ycircle[j]^2)<(maxray^2)){points(xcircle[j],ycircle[j], 
                                                      col="darkgreen",pch=".")
        if 
        (j==10){text(xcircle[j],ycircle[j],signif(i,2),cex=0.5,col="darkgreen")}}
    }
  }    
  # sd concentriques autour de l'origine
  seq.sd<-seq.int(0,maxray,length.out=5)
  for (i in seq.sd){
    xcircle<-(cos(discrete*pi/180)*i)
    ycircle<-sin(discrete*pi/180)*i
    lines(xcircle,ycircle,lty=3,col="blue")
    text(min(xcircle),-0.03*maxray,signif(i,2),cex=0.5,col="blue")
    text(max(xcircle),-0.03*maxray,signif(i,2),cex=0.5,col="blue")
  }
  
  par(family='serif')
  text(0,-0.08*maxray,"Standard Deviation",cex=0.9,col="blue")
  text(0,-0.14*maxray,"Centered RMS Difference",cex=0.7,col="darkgreen")
  text(0,((1.1*maxray)+0.1),"Correlation Coefficient",cex=0.9)
  points(sd.r,0,pch=22,bg="darkgreen",cex=1.1)
  title(title, line=-2, family='serif')
  }
    
  # placer les points
  par(new = T)
  plot(c(-maxray,maxray),c(0,maxray),type="n",asp=1,bty="n",xaxt="n",yaxt="n",xlab="",ylab="", family='serif')
  points(sd.f*cos(acos(R)),sd.f*sin(acos(R)),pch=21,bg=couleur,cex=0.9)
  text(sd.f*cos(acos(R)), sd.f*sin(acos(R)), model_n, cex=0.8, pos=2, col=couleur) 
  
  ## calculate bias
  if (m_bias==TRUE) {
    m.r <- mean(x)
    m.f <- mean(y)
    bias <- m.f - m.r
    
    ## coordinates for model and observations
    dd <- rbind(mp = c(sd.f * R, sd.f * sin(acos(R))), rp = c(sd.r, 0))
    
    ## find equation of line passing through pts
    v1 <- solve(cbind(1, dd[,1])) %*% dd[,2]    
    
    ## find perpendicular line
    v2 <- c(dd[1,2] + dd[1,1]/v1[2], -1/v1[2])
    
    ## find point defined by bias
    nm <- dd[1,] - c(0, v2[1])
    nm <- nm / sqrt(sum(nm^2))
    bp <- dd[1,] + bias*nm
    
    ## plot lines
    arrows(x0 = dd[1,1], x1 = bp[1], y0 = dd[1,2], y1 = bp[2], col = couleur, length = 0.05, lwd = 1.5)
    lines(rbind(dd[2,], bp), col = couleur, lty = 3)
    lines(dd, col = couleur, lty = 3)
  } else {
    bp <- NULL
  }
}

#Model Statement
#Taylor(ref, model1, F, 1, 'red', T, "Model 1")
#Taylor(ref, model2, T, 2, 'blue', T)

#Comparing Function to Plotrix
#library(plotrix)
#taylor.diagram(ref,model1,pos.cor=FALSE)
#taylor.diagram(ref,model2,add=TRUE,col="blue")


###############################
#   PRINTING INCLUDED PLOTS   #
###############################
Plots <- c("Included Plot Elements and Templates", " ", "Base R Continuous Color Legend", "Base R Discrete Legend", "Base R Pie Chart",
           "Correlation Plots: Corrgram and Base R Correlation Plot Using the Psych Package", "Base R Scatter Plot with Thrills",
           "Base R Grouped Bar Chart", "Base R Stacked Bar Chart", "Base R Shaded Density Plot with Mathematical Annotations",
           "Base R Convex Hull Function Plot", "Base R Histogram with CDF", "Base R Kolmogorov-Smirnov Plot", "Base R CCDF Plot",
           "ggplot2 Violin Plot with Boxplot", "Base R Split Violin Plot", "Pirate Plot", "Base R Beeswarm Plot Overlay Plot with Dot Plot Subplot",
           "Base R Bubble Plot", "Stylized ggplot Panel Plot", "Base R Panel Plot", "Base R Jittered Group Scatter Plot with Extras", "Ternary Plot with Base R Overlay and Subplots",
           "Tab Plot for Exploring Missing Data", "Classic Base R Heat Maps", "Binned Continuous Variable Heat Map", "Base R Continuous Heat Map with Overlays", "Base R Contour Scatter Plot", 
           "Contour Network Plot", "Base R 3D Polar Plot", "Chord Plot", "Arc Diagrams implemented using the arcdiagram and ggplot2", "Alluvial Plot",
           "Hägerstrand Time-Space Cube", "Regression Diagnostics: Base R and ggfortify", "PCA Diagrams and Steps", "Panel Plot Comparing Multiple Model Fits",
           "Stylized Fitted Line and Confidence Interval: Base R", "Stylized ggplot2 Smoothed Regression Line with Leverage Lines", "Dataplot Style 4Plot","Dataplot Style 6Plot","Johnson-Neyman Interval for 2-Way Interactions",
           "Johnson-Neyman Intervals: Testing Slope Homogeneity for Two Groups", "Model Assessment: Repeated Density Ratios", "AUROC Analysis", "Complex Model Assessment: Taylor Diagram")

cat(Plots, sep='\n')

rm(Plots)
