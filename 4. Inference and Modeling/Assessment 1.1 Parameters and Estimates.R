# Exercise 5. se versus p

# Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the population. Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).
# Plot se versus p for the 100 different proportions.

# Use the seq function to generate a vector of 100 values of p that range from 0 to 1.
# Use the sqrt function to generate a vector of standard errors for all values of p.
# Use the plot function to generate a plot with p on the x-axis and se on the y-axis.

# Sampling Model Parameters and Estimates
library(tidyverse)
library(dslabs)
take_poll(25)    # draw 25 beads

# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1, length.out = 100)
p

# Create a variable `se` that contains the standard error of each sample average
see <- function(p){
  se <- sqrt(p*(1-p)/N)
}
se <- sapply(p,see)
se
# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)

#===============================================================================
# Exercise 6. Multiple plots of se versus p

# Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se when the sample sizes equal N=25, N=100, and N=1000.

# Your for-loop should contain two lines of code to be repeated for three different values of N.
# The first line within the for-loop should use the sqrt function to generate a vector of standard errors se for all values of p.
# The second line within the for-loop should use the plot function to generate a plot with p on the x-axis and se on the y-axis.
# Use the ylim argument to keep the y-axis limits constant across all three plots. The lower limit should be equal to 0 and the upper limit should equal 0.1 (it can be shown that this value is the highest calculated standard error across all values of p and N).

# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (i in sample_sizes){
  see <- function(p){
    se <- sqrt(p*(1-p)/i)
  }
  se <- sapply(p,see)
  se
  plot(p, se, ylim = c(0,0.1))
}


