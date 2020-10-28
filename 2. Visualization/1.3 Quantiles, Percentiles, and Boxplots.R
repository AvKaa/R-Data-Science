library(dslabs)
data(heights)

# quantile function:
qunatile(data,q)
quantile(data, seq(0.01,0.99,0.01)) # 100% in steps of 1%

# use summary() on the heights$height variable to find the quartiles:
summary(heights$height)

# Find the percentiles of heights$height:
percentiles <- quantile(heights$height, seq(0.01,0.99,0.01))

# Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. 
# Note that quantile() returns a named vector. 
# You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#-------------------------------------------------------------------------------
# qnorm function:
# The qnorm() function gives the theoretical value of a quantile with probability p 
# of observing a value equal to or less than that quantile value given a normal distribution with 
# mean mu and standard deviation sigma:

qnorm(p, mu, sigma) # by default mu = 0 and sigma = 1
# => calling qnorm with no arguments gives quantiles for the standard normal distribution

qnorm(p)
# quantiles are defined such that p is the probability of a random observation less than or equal to the quantile

# pnorm function 
# The pnorm() function gives the probability that a value from a standard normal distribution 
# will be less than or equal to a z-score value z. Consider:

pnorm(-1.96) ??? 0.025 
# The result of pnorm() is the quantile. Note that:
qnorm(0.025) ??? ???1.96 
# qnorm() and pnorm() are inverse functions:
pnorm(qnorm(0.025)) = 0.025

#-------------------------------------------------------------------------------
# Theoretical quantiles

# qnorm() can be used to determine the theoretical quantiles of a dataset: 
# that is, the theoretical value of quantiles assuming that a dataset follows a normal distribution.
# Run the qnorm() function with the desired probabilities p, mean mu and standard deviation sigma. 

# Suppose male heights follow a normal distribution with a mean of 69 inches and standard deviation of 3 inches. 
# The theoretical quantiles are:
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)

# Theoretical quantiles can be compared to sample quantiles determined with the quantile function 
# in order to evaluate whether the sample follows a normal distribution.

#-------------------------------------------------------------------------------
# Qunatile-Quantile Plots
# define x and z
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)

# proportion of data below 69.5
mean(x <= 69.5)

# calculate observed and theoretical quantiles
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))

# make QQ-plot
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

# make QQ-plot with scaled values
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

#-------------------------------------------------------------------------------
" 
Boxplots:
The top and bottom of the box is 75th and 25th percentiles.
Whiskers shows the range
Distance between the two percentiles is the interquatile range







