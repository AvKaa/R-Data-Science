# Random Variables

# define random variable x to be 1 if blue, 0 otherwise
beads <- rep(c("red", "blue"), times = c(2, 3))
x <- ifelse(sample(beads, 1) == "blue", 1, 0)

# demonstrate that the random variable is different every time
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)
ifelse(sample(beads, 1) == "blue", 1, 0)

#===============================================================================
# Sampling Models

# A sampling model models the random behavior of a process as the sampling of draws from an urn.
# The probability distribution of a random variable is the probability of the observed value falling in any given interval.
# We can define a CDF  F(a)=Pr(S???a)  to answer questions related to the probability of S being in any interval.
# The average of many draws of a random variable is called its expected value.
# The standard deviation of many draws of a random variable is called its standard error.

# Monte Carlo simulation: Chance of casino losing money on roulette:

# sampling model 1: define urn, then sample
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) # define the urn for the sampling model
n <- 1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))    # 1000 independent draws
S <- sum(x)    # total winnings = sum of draws
S

# We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casino losing money.
n <- 1000    # number of roulette players
B <- 10000    # number of Monte Carlo experiments
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace = TRUE, prob = c(9/19, 10/19))    # simulate 1000 spins
  sum(X)    # determine total profit
})

mean(S < 0)    # probability of the casino losing money

# We can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S.
library(tidyverse)
s <- seq(min(S), max(S), length = 100)    # sequence of 100 values across range of S
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")

#===============================================================================
# Central Limit Theorem
"
These equations apply to the case where there are only two outcomes,  
a  and  b  with proportions  p  and  1???p  respectively. 
The general principles above also apply to random variables with more than two outcomes.

Expected value of a random variable: 
  ap+b(1-p)
Expected value of the sum of n draws of a random variable: 
  n*(ap+b(1-p))
Standard deviation of an urn with two values: 
  abs(b-a)*sqrt(p(1-p))
Standard error of the sum of n draws of a random variable:
  sqrt(n)*abs(b-a)*sqrt(p(1-p))


















