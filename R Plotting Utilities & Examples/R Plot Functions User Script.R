#R Plot Utilities Basic User Script
#Jonathan H. Morgan
#9 December 2019

#Clear Out Console Script
cat("\014")

#Clearing Old Data
rm(list = ls())
gc()

#Work Machine
setwd("~/Desktop/FHP Micro Projects") 
getwd()

################
#   PACKAGES   #
################

library(ggplot2)
library(scales)
library(psych)
library(plotrix)
library(brms)
library(lme4)
library(rstanarm)
library(bayestestR)
library(sas7bdat)

#Pulling in R Ploting Functions
source("~/Desktop/FHP Micro Projects/R Plot Utilities_9Dec2019.R")

###########################################
#   VISUALIZATION UTILITIES & PLOT TYPES  #
###########################################

#BASE BREAKS: STYLIZING ggplots
# The base_breaks functions are used to format the axses of ggplots to align them with Base R plots.
# I use both plotting modalities, so these functions help keep things looking consistent.
  # base_breaks_x and base_breaks_y are for continuous variables. 
  # base_breaks_x_d an base_breaks_y_d are discrete.
  # base breaks_x and base_breaks_y include a label command as well, by default this is set to the breaks values.

#An example where the labels are used in the ggplot2 panel plot example.
model <- loess(wt ~ hp, data = mtcars)

#Predict fitted values for each observation in the original dataset
modelFit <- data.frame(predict(model, se = TRUE))

#Define data frame for ggplot
df <- data.frame(cbind(hp = mtcars$hp, wt = mtcars$wt, fit = modelFit$fit, upperBound = modelFit$fit + 2 * modelFit$se.fit
                       , lowerBound = modelFit$fit - 2 * modelFit$se.fit))

#Plotting
  #Build the plot using the fitted values from the predict() function
  #geom_linerange() and the second geom_point() in the code are built using the values from the predict() function

ggplot(df, aes(x=hp, y=wt)) + 
     theme(panel.background = element_rect(fill = 'white', linetype = 1),
        panel.grid.minor = element_line(colour = 'snow3'),
        plot.title = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(colour="grey20",size=12),
       axis.title.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=12),
        axis.title.y = element_text(colour="grey20",size=16),
        text = element_text(family="Times"),
        axis.ticks.length = unit(0.25, "cm")) +  
        guides(size=FALSE) +
  geom_point(aes(hp, fit, size=1)) +
  geom_point(alpha=.5) +
  geom_linerange(aes(ymin = lowerBound, ymax = upperBound)) +
  geom_smooth(method = "loess", alpha=0.5, fill='cornsilk') +
  geom_line(aes(x = hp, y = lowerBound), linetype = 2, col='red') +
  geom_line(aes(x = hp, y = upperBound), linetype = 2, col= 'red') +
  util$base_breaks_x(df$hp) +
  util$base_breaks_y(df$wt) +
  labs(y = "Weight", x = "Horsepower") +
  ggtitle("Example Predicted Values Plot") +
  theme(plot.margin = unit(c(1,1,0,1), "cm"))

rm(df, model, modelFit)

#COLOR RAMP LEGENDS
  #The Function legend is for generating continuous legends in Base R Plots. 
  #One note, the appearance of the legend depends on plot size. 

#Specifying the color range
colfunc<-colorRampPalette(c("red","yellow","springgreen","royalblue"))

#Example plot: Will need to use a matrix if additional subplots are needed.
layout(matrix(1:2,ncol=2), width = c(6,1),height = c(1,1))
par(mar = c(3, 3, 2, 1))  
plot(1:20, 1:20, pch = 19, cex=2, col = colfunc(20))

#Creating image raster based on the color ramp specified in the colfunc
legend_image <- as.raster(matrix(colfunc(20), ncol=1))

#Specifying the legend: Adjustments are required with differnt margins (just make and axes=TRUE and adjust)
par(mar = c(3, 0, 2, 0), xaxs = "i", yaxs = "i", family='serif')
plot(c(0,2),c(0,1),type = 'n', axes = FALSE,xlab = '', ylab = '')
title('Heat', line=-2.5, family='serif', font.main=2,  cex.main = 1.5, adj=0)

text(x=1.25, y = seq(0.11,0.885,l=5), labels = seq(0,1,l=5))
rasterImage(legend_image, 0, 0.10, 0.85, 0.90)

rm(legend_image)

#STANDARD ERROR BARS
  #plotsegraph: This is just a simple little function for plotting standard error bars 
  #in a Base R plot.

#Making Some Data
# Simulate data
n.each <- 1000
A1 <- rnorm(n.each, 2, 1)
A2 <- rnorm(n.each, 1.5, 2)
B1 <- rnorm(n.each, 4, 1.5)
B2 <- rnorm(n.each, 0, 1)
values <- c(A1, A2, B1, B2)
treatment <- rep(c("A", "B"), each=n.each*2)
group <- rep(c(1, 2, 1, 2), each=n.each)

max(A1)
max(B1)
max(a1.sd)
max(b1.sd)

#Creating Data Elements for Boxplot
a1.mean <- mean(A1)
b1.mean <- mean(B1)

a1.sd <- sd(A1)
b1.sd <- sd(B1)

#If you need t-distribution: error <- qt(0.975,df=n-1)*s/sqrt(n)
a1.error <- plotrix::std.error(A1)  #Being lazy
b1.error <- plotrix::std.error(B1)  

data <- as.matrix(cbind(a1.mean, b1.mean))
colnames(data) <- c('Group 1', 'Group 2')
rownames(data) <- c('Mean')

#Plotting
par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(3,5,3,1))
barplot(data,
        beside = TRUE, col = c("gray50", "dodgerblue"), 
        density=c(30, 40), angle= c(60),
        las = 1, xlab = " ", ylab = "Mean", family = 'serif', cex.names=1.5,
        cex.main = 1.5, axes = FALSE, ylim = c(0, (max(B1) + max(b1.error))), 
        cex.axis = 1.5, cex.lab=1.5)

y_axes <- seq(0, 8, by=0.5)
axis(2, y_axes, cex.axis = 1.5, family = 'serif')

at1 <- c(1.5, 3.5)
axis(1, at1, labels = F , cex.axis = 1.5, family = 'serif')

#Adding Error Bars
x <- c(1.5)
y <- c(3.5)

plot.errbars <- util$plotsegraph(x, a1.mean, a1.error, 0.1, color = "black") 
plot.errbars <- util$plotsegraph(y, b1.mean, b1.error, 0.1, color = "black") 

#Adding Title
title("Example Bar Plot with Error Bars", line=-0.25, family='serif', font.main=2,  cex.main = 2)

# SPLIT VIOLIN PLOTS
require(vioplot)
require(devtools)
require(digest)

#Note: You can do a lot more with this functin.
par(bty="n", mai = c(0.1, 0.1, 0.1, 0.1), mar=c(5,5,3,1))
plot(x=NULL, y=NULL, xlim = c(0.5, 2.5), ylim=c(min(values), max(values)),
     type="n", ann=FALSE, axes=F, family='serif', cex.lab=1.5)

par(family='serif')
axis(1, at=c(1, 2),  labels=c("A", "B"))
axis(2)

for (i in unique(treatment)) {
  for (j in unique(group)){
    util$vioplot2(values[which(treatment == i & group == j)],
            at = ifelse(i == "A", 1, 2),
             side = ifelse(j == 1, "left", "right"),
             col = ifelse(j == 1, "brown", "goldenrod2"),
            drawRect = TRUE,
            rectCol = "lightblue",
            add = T)
  }
}

title("Example Split Violin plot", xlab="Treatment")
legend("bottomright", fill = c("brown", "goldenrod2"), legend = c("Group 1", "Group 2"), box.lt)

rm(A1, A2, B1, B2, group, i, j, n.each, treatment, values, a1.error, a1.mean, a1.sd,
   at1, b1.error, b1.sd, b1.mean, x, y, y_axes, data, plot.errbars)

############################################
#   STATISTICAL TOOLS AND VISUALIZATIONS   #
############################################

#I am walking through two simple examples to demonstrate the various tools in a common workflow.
#The data comes from a 1908 sleep study, a dataset looking at state-level indices in the 1970s, and
#finally one example from my dissertation data. 

#*****************************************************************   -Hypothesis Testing-   ************************************************************************#
#Did the medication lead to extra sleep?

#Getting Data: Splitting into subgroups for the purposes of using the vioplot.singmann function.
data("sleep")
sleep$ID <- as.numeric(as.character(sleep$ID))
group_1 <- sleep[sleep$group == "1", ]$extra
group_2 <- sleep[sleep$group == "2", ]$extra

#Examining Distributions of the Experimental and Control Groups (Drug vs. Placedbo)
par(mar = c(5,5,2,5), cex.main=1.5, cex.lab=1.5, font.lab=1.3, bty='n')
x <- c(1, 2)
plot(x, c(-10, -10), type = "p", xlab=' ', ylab= '', ylim=c(min(sleep$extra), max(sleep$extra)), xlim = c(1, 4), main = " ", xaxt='n')

par(family='serif', cex.lab=1.5)
axis(1, at = c(2, 3), labels = c("Placebo", "Experimental"))

util$vioplot.singmann(group_1, add = TRUE, mark.outlier = FALSE, 
                 at = c(2), wex = 0.5, yaxt = "n")

util$vioplot.singmann(group_2, add = TRUE, mark.outlier = FALSE, 
                 at = c(3), wex = 0.5, yaxt = "n")

title('1908 Sleep Study', line=-0.1, family='serif', font.main=2,  cex.main = 1.5)

#Some Descriptive Support of an Effect: Calculating Svage-Dicke Density Ratio 
model <- rstanarm::stan_glm(extra ~ group, data=sleep)
model_bayes.factors <-  bayestestR::bayesfactor_parameters(model, null= 0, save_all_pars = TRUE)

#Not Great: Specifying One Sided Test because the entire expermental distribution is positive
model_bayes.factors <- bayesfactor_parameters(model, direction = ">")
#model_bayes.factors

#Plotting Distributions
plot(model_bayes.factors)

#Alternatively to get a bit more of an intuition about what is happening
model_data <- attr(model_bayes.factors, "plot_data")
plot_data <- model_data[[1]]
d_points <- model_data[[2]]

posteriors <- plot_data[plot_data$Distribution == "posterior", ]
priors <- plot_data[plot_data$Distribution == "prior", ]

g2_priors <- priors[priors$ind == "group2", ]
g2_posteriors <- posteriors[posteriors$ind == "group2", ]

rm(posteriors, priors)

#Plotting Density Ratios
par(cex.main = 1.5, mar = c(4, 4.5, 4.5, 1), mgp = c(3, 1.2, 0), cex.lab = 1.5, 
    font.lab = 2, cex.axis = 1.8, bty = "n", las = 1)

#Adding Densities
plot(g2_posteriors$x, g2_posteriors$y, type = "l", axes = FALSE, xlab = NA, ylab = NA, xlim = c(0, 25), 
     lwd = 2)
lines(g2_priors$x, g2_priors$y, lwd = 2, col = "grey70")

#Adding Axses 
axis(side = 1, at = seq(-15, 15, by = 5), pos = 0, lwd = 2, cex.axis = 1.5)
#axis(side = 2, at = seq(0, 0.5, by = 0.1), lwd = 2, cex.axis = 1.5)

#Adding Shading
polygon <- g2_posteriors[g2_posteriors$y <= d_points[4,2], ]
polygon <- polygon[polygon$x >= 0, ]
polygon( c(polygon$x, 0), c(polygon$y, min(polygon$y)), col='gray')

#Adding 0 Reference Lines
abline(v = 0, col = "red4", lwd = 2)
lines(c(-0.5, 0.5), y = c(d_points[4,2], d_points[4,2]), col = "red4", lwd = 2)
lines(c(-0.5, 0.5), y = c(d_points[2,2], d_points[2,2]), col = "red4", lwd = 2)

#Adding Likelihood Labels
at_1 <- round(d_points[2,2], digits=2)
at_2 <- round(d_points[4,2], digits=2)
mtext(side = 2, text = paste('L =',at_1), at = d_points[2,2], col = "red4", line = 0.1, cex = 1.5)
mtext(side = 2, text = paste('L =',at_2), at = d_points[4,2], col = "red4", line = 0.1, cex = 1.5)

text(10, median(g2_posteriors$y), expression(paste("BF = ", frac(".16", ".09") %~~% 1.77, sep = "")), 
     adj = 0, col = "red4", cex = 1.5)

#Adding legend
par(family='serif')
legend("topright", legend=c("Prior", "Posterior"), col=c("grey70", "black"), lwd=c(2, 2), bty='n', cex=1.1)

#Adding Title
title('Savage-Dickey Density Ratio', line= - 0.25, family='serif', font.main=2,  cex.main = 2)

rm(d_points, g2_posteriors, g2_priors, plot_data, polygon, at_1, at_2, group_1, group_2, model, model_bayes.factors, 
   model_data, x, sleep)

#Summary: Even with after specifying a one-sided test, the evidence is not great. A Bayes Factor of less than 3 is hardly
#         conclusive evidence. Arguably, specifying a better prior might improve the model, but I wouldn't go writing home about
#         this sleep drug.

#**************************************************************   -Multivariate Model Assessment-   ***************************************************************#
#In this example, I model the effects of illiteracy and the murder rate on state income based on data collected in the 1970s. 
#The approach outlined here could be asily be extended using Bayesian Model Averaging to handle questions of model uncertainty. 

#IMPORTANT: I use non-informative priors in these examples. Bayes Factors are sensitive to priors. 
#           When applying these approaches, the researcher needs to consider the priors selected carefully.

#For Bayes factor specific priors see:
  #Ly, A., Verhagen, J., & Wagenmakers, E.-J. (2016).
  #Harold Jeffreys’s default Bayes factor hypothesis tests: Explanation, extension, and application in psychology. 
  #Journal of Mathematical Psychology, 72(Supplement C), 19–32. https://doi.org/10.1016/j.jmp.2015.06.004

  #Rouder, J. N., Morey, R. D., Speckman, P. L., & Province, J. M. (2012). 
  #Default Bayes factors for ANOVA designs. Journal of Mathematical Psychology, 56(5), 356–374. 
  #https://doi.org/10.1016/j.jmp.2012.08.001

#Getting Data
library(jtools) 
states <- as.data.frame(state.x77)

#Looking at the Distributions of Income, High School Graduates, Illiteracy and Murder

#Estimating the Empirical Cumulative Distribution Functions of each Variable
ec_Income <- ecdf(states[[2]])
ec_HS <- ecdf(states[[6]])
ec_Illiteracy <- ecdf(states[[3]])
ec_Murder <- ecdf(states[[5]])

#DESCRIPTIVE ANALYSES

#Plotting Histograms and CDFs

#Income
par(mar = c(5,5,2,5))
h_Income <- hist(states[[2]], breaks=seq(3000, 6500, 100), xlim=c(3000, 6500), main='Histogram of Income', xlab='Income')
par(new = T)
plot(x = h_Income$mids, y=ec_Income(h_Income$mids)*max(h_Income$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h_Income$mids, y=ec_Income(h_Income$mids)*max(h_Income$counts), col ='red')
axis(4, at=seq(from = 0, to = max(h_Income$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red', las=0)

#High School Graduation
par(mar = c(5,5,2,5))
h_HS <- hist(states[[6]], breaks=seq(35, 70, 1), xlim=c(35, 70), main='Histogram of High School Graduation %', xlab='High School Graduation % (1970)')
par(new = T)
plot(x = h_HS$mids, y=ec_HS(h_HS$mids)*max(h_HS$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h_HS$mids, y=ec_HS(h_HS$mids)*max(h_HS$counts), col ='red')
axis(4, at=seq(from = 0, to = max(h_HS$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red', las=0)

#Illiteracy
par(mar = c(5,5,2,5))
h_Illiteracy <- hist(states[[3]], breaks=seq(0.5, 3.0, 0.1), xlim=c(0.5, 3), main='Histogram of Illiteracy', xlab='Illiteracy Rate (1970)')
par(new = T)
plot(x = h_Illiteracy$mids, y=ec_Illiteracy(h_Illiteracy$mids)*max(h_Illiteracy$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h_Illiteracy$mids, y=ec_Illiteracy(h_Illiteracy$mids)*max(h_Illiteracy$counts), col ='red')
axis(4, at=seq(from = 0, to = max(h_Illiteracy$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red', las=0)

#Murder
par(mar = c(5,5,2,5))
h_Murder <- hist(states[[5]], breaks=seq(0, 16, 1), xlim=c(0, 16), main='Histogram of Illiteracy', xlab='Murder and Non-Negligent Manslaughter Rate per 100,000 Population (1976)')
par(new = T)
plot(x = h_Murder$mids, y=ec_Murder(h_Murder$mids)*max(h_Murder$counts), col = rgb(0,0,0,alpha=0), axes=F, xlab=NA, ylab=NA)
lines(x = h_Murder$mids, y=ec_Murder(h_Murder$mids)*max(h_Murder$counts), col ='red')
axis(4, at=seq(from = 0, to = max(h_Murder$counts), length.out = 11), labels=seq(0, 1, 0.1), col = 'red', col.axis = 'red')
mtext(side = 4, line = 3, 'Cumulative Density', col = 'red', las=0)

#Looking at the correlations between the variables

#Subsetting Data of Interest
model_data <- states[c(2, 6, 3, 5)]

pairs.panels(model_data, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE) # show correlation ellipses 

#Alaska clearly is an outlier
model_data <- model_data[-c(2), ]
colnames(model_data)[[2]] <- c('Grad')

#BI-VARIATE & MULTIVARIATE REGRESSION FITS

#Examining Bi-Variates
HS_model <- lm(Income ~ Grad, data = model_data)
par(mfrow = c(2, 2))
plot(HS_model)   #Alaska, New Mexico, and Utah are outliers.

Illiteracy_model <- lm(Income ~ Illiteracy, data = model_data)
par(mfrow = c(2, 2))
plot(Illiteracy_model)   #Alaska again.

Murder_model <- lm(Income ~ Murder, data = model_data)
par(mfrow = c(2, 2))
plot(Murder_model)   #Alaska and the South.

#Quickly examining Multiple Models Fits
model_1 <- lm(Income ~ Grad, data = model_data)
summary(model_1)
BIC(model_1)
        
model_2 <- lm(Income ~ Grad + Illiteracy, data = model_data)
summary(model_2)
BIC(model_2)

model_3 <- lm(Income ~ Grad + Illiteracy + Murder, data = model_data)
summary(model_3)
BIC(model_3)

#The model with the interaction effect is best, wierdly the models with murder and illiteracy as independent main effects (no so great)
#Highlights that interaction effects are not simply an aggregate of thier main effects.
model_4 <- lm(Income ~ Grad + Illiteracy + Murder, Illiteracy*Murder, data = model_data) 
summary(model_4)
BIC(model_4)

#But How Much Better is 748.47 compared to 717?

#COMPARING MODELS

#Calculating Bayes Factor Models to address this question  -> This is pretty intensive (even for small models) so take care.
#   A Bayes factor greater than 1 can be interpereted as weak evidence against the compared-to model (the denominator).
#   One convention is that a Bayes factor greater than 3 can be considered as evidence against the denominator model 
#   (and vice versa, a Bayes factor smaller than 1/3 indicates substantial evidence in favor of the denominator model) (Wetzels et al. 2011).

#Do These One at a Time: save_all_pars makes this slow but is necessary for the bayesefactor_models statment
m0 <- brm(Income ~ 1, data = model_data, save_all_pars = TRUE)
m1 <- brm(Income ~ Grad, data = model_data, save_all_pars = TRUE)
m2 <- brm(Income ~ Grad + Illiteracy, data = model_data, save_all_pars = TRUE)
m3 <- brm(Income ~ Grad + Illiteracy + Murder, data = model_data, save_all_pars = TRUE)
m4 <- brm(Income ~ Grad + Illiteracy + Murder + Illiteracy*Murder, data = model_data, save_all_pars = TRUE) 

#Looking at Bayesfactors: Having trouble with the briding when I increase the iteratin count (consider doing it in stan))
comparison <- bayesfactor_models(m1, m2, m3, m4, denominator = m0)
comparison

#We can see that the full model is the best model - with a Bayes Factor 2.46 * 10,000,0000,000,000 
#compared to the null (intercept only).

#Assessing Overall Model Peformance Using a Taylor Diagram
m1.fit <- predict(m1)
m2.fit <- predict(m2)
m3.fit <- predict(m3)
m4.fit <- predict(m4)

pred.1 <- m1.fit[ , 1]
pred.2 <- m2.fit[ , 1]
pred.3 <- m3.fit[ , 1]
pred.4 <- m4.fit[ , 1]

util$Taylor(model_data$Income, pred.1, F, 1, 'red', T, "State Income")
util$Taylor(model_data$Income, pred.2, T, 2, 'blue', T, " ")
util$Taylor(model_data$Income, pred.3, T, 3, 'brown', T, " ")
util$Taylor(model_data$Income, pred.4, T, 4, 'pink', T, " ")

#Clearly the Interaction Effect is Critical to Understanding this Relationship
  #Roughly a 90% Pearson Correlation
  #The Estimates have less variation than is true in the real data
  #The residuals have some variation.

#Examining Interaction Effects with an Intepolated Polar Plot

#Note: The median is used when there are multiple z points. You can change this by changing the interp function to be either
#      "error", "strip", "mean", "median", or "user" in the function.

#By construction, the plot requires unique values because the same point (r, theta) can be expressed
#with an infinite number of different polar coordiantes.

#Polar Plots are most intuitive when the relationship being modeled has a cylcular pattern
#(e.g., temperature over the course of the year).

#Preparing Data and plotting function: 
model_data$z <- model_data$Illiteracy*model_data$Murder

#NOTE: Make the Plot screen larger before running this.
util$PolarImageInterpolate(model_data$Illiteracy, model_data$Murder, model_data$z,
                      contours=TRUE, legend=TRUE, axes=TRUE, points=TRUE, extrapolate = TRUE)

#Neverthless, an extrapolated polar plot is useful for examining bi-variate relationships as well.
#The shading from the origin indicates the directionality of the relationship, positive in this case.
#The spacing between the intervals indicates where the relationship is strongest, with uneven
#spacing indicating that we should examine whether the relationship is consistent across all points.
#The shape of the countour lines indicates the shape of the functions which is a nice
#feature compared to more conventinal topographic scatter plot representations.

#Is the relationship consistent across the entire space?
#To explore this last question, we calculate the Johnson Neyman Confidence interval using the 
#jnt function.

#jnt calculates the Johnson Neyman interval for the observed data for two-way interactions, plotting the
#areas where the moderator's effect on the predictor is not significant. 
#The theta_plot function visualizes the interval.
#The interactions package supports this and analyes with simulated values. 
#To replicate this function, we would have to simulate and then calculate the intervals. 
#But, I like having the math, my visualization is betterk and I am primary concerned with the observed data. 

#Plotting Income as a Function of Murder, Illiteracy, and the interaction between Murder and Illteracy for the purposes of 
#evaluating the interaction.

#Fitting the Model
income.lm <- lm(Income ~ Illiteracy * Murder, data = states)

#Examining the Confidence Interval
util$jnt(income.lm, predictor = "Illiteracy", moderator = "Murder", alpha = .05)
util$theta_plot(income.lm, predictor = "Illiteracy", moderator = "Murder", jn = T)

#The theta plot confirms the intuition from the Polar plot, the clump of more extreme observations is driving the effect.
#The moderating effect of murder on illiteracy is not consistent accross all values of illiteracy. 
#There appears to be a tippling point where the effect of murder becomes meaningful.

rm(comparison, h_HS, h_Illiteracy, h_Income, h_Murder, HS_model, Illiteracy_model, Murder_model, income.lm, m0,
   m1, m1.fit, m2, m2.fit, m3, m3.fit, m4, m4.fit, model_1, model_2, model_3, model_4, model_data, states,
   ec_HS, ec_Illiteracy, ec_Income, ec_Murder,  pred.1, pred.2, pred.3, pred.4)

#EXAMINING GROUP-LEVEL DIFFERENCES

#The final example comes from my dissertation data. 
#The research question is whether the effect of actor potency on actor evaluation is consistent for both sterotypically
#male identities compared to other identities. 
#Past research suggests that stereotypiclly male identities should experience 

#To test if there are meaningful group level differences in this effect I use the The JohnsonNeymanb_GC function to plot
#the Johnson Neyman interval to test whether the two groups have homogenous slopes (the null hypothesis) or heterogenous slopes.

#Getting Data
NC78_Events <- as.data.frame(read.sas7bdat("~/Desktop/FHP Micro Projects/R Plot Utilities Data/nc78_events.sas7bdat"))

#Subsetting Data to Isolate Actor Potency Models and Male and Female Actors
male.actors <- NC78_Events[ NC78_Events$Var_I == "IA1E" & NC78_Events$Actor_Male == 1,  ]
dist.actors <- NC78_Events[ NC78_Events$Var_I == "IA1E" & NC78_Events$Actor_GCategory %in% 'Distributed',  ]

ap_1 <- male.actors$Ap
ae_1 <- male.actors$DV

ap_2 <- dist.actors$Ap
ae_2 <- dist.actors$DV

#Fitting Data
pred_1 = lm(ae_1~poly(ap_1,2))
pred_2 = lm(ae_2~poly(ap_2,2))

par(mar=c(1, 1, 1, 1))
labels <- c('Actor Potency', 'Transient Actor Evaluation')
util$JohnsonNeyman_GC(ap_1, ap_2, ae_1, ae_2, labels)

#We find some evidence of hetrogenous slopes. 
#For both groups, the proponderance of the effect is driven by correlation between highly evaluated 
#and highly potent identities. We aslo see a gap in EPA space that sterotypically male actors do 
#not occupy (weak positions). In addition, these analyses sugget a slight u-shaped curve for 
#stereoytpically male actors, where weaker male actorrs are more highly evaluated, moderate 
#powerful actors are not highly evaluated, and then more potent male actor are highly evaluated.

rm(dist.actors, male.actors, NC78_Events, pred_1, pred_2, ae_1, ae_2, ap_1, ap_2, labels)