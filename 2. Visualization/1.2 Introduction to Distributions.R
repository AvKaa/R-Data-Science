# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))

# CDF can be manually calculated by defining a function to compute the probability above
# This function can then be applied to a range of values across the range of the dataset to calculate a CDF:
a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

"
The CDF defines that proportion of data below a cutoff a . 
To define the proportion of values above a , we compute:
1???F(a) 
To define the proportion of values between a and b, we compute:
F(b)???F(a) 

Note that the CDF can help compute probabilities. The probability of observing a randomly chosen value between  a  and  b  is equal to the proportion of values between  a  and  b , which we compute with the CDF.
"

#--------------------------------------------------------------------------------
# Normal Distribution

# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]

# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# The scale function converts a vector of approximately normally distributed values into z-scores.
z <- scale(x) # equivalent to (x-average)/SD

# compute the proportion of observations that are within 2 standard deviations of the mean like this:
mean(abs(z) < 2) 
"
- About 68% of observations will be within one standard deviation of the mean (mu±sigma). 
In standard units, this is equivalent to a z-score of lzl <= 1.

- About 95% of observations will be within two standard deviations of the mean (mu±2sigma). 
In standard units, this is equivalent to a z-score of lzl <= 2.

- About 99.7% of observations will be within three standard deviations of the mean (mu±3sigma). 
In standard units, this is equivalent to a z-score of lzl <= 3.
"

#--------------------------------------------------------------------------------
# The Normal CDF and pnorm
  
# Code: Using pnorm to calculate probabilities  
# Given male heights x:

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# We can estimate the probability that a male is taller than 70.5 inches with:
1 - pnorm(70.5, mean(x), sd(x))


# Code: Discretization and the normal approximation
# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# probabilities in normal approximation match well
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

# probabilities in actual data over other ranges don't match normal approx as well
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
