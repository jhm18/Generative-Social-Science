#Calculating KL Divergengence
#Jonathan H. Morgan
#22 October 2018

################
#   PACKAGES   #
################

library(LaplacesDemon)
library(entropy)

#######################
#   SIMPLE EXAMPLES   #
#######################

#Example Data
px <- pbeta(runif(100),17,6)
py <- pbeta(runif(100),3,3)

px <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.64)
py <- c(0.02, 0.004, 0.006,0.008, 0.01, 0.012, 0.014, 0.016, 0.928)

data_frame = data.frame(px, py)       #In R forums, df is often used to refer to a data frame.
data_frame

#Visualizing Distributions
ggplot(data_frame, aes(px)) +
  geom_density()

ggplot(data_frame, aes(py)) +
  geom_density()

#Calculating the Divergence Score and Chi Square

#LaplacesDemon
KLD(px,py)

#entropy
KL.plugin(px, py)

# and corresponding (half) chi-squared statistic
0.5*chi2.plugin(px, py)


