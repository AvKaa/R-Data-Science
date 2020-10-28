# The Central Limit Theorem in Practice
"
(X_hat = X¯)
Because  X¯  is the sum of random draws divided by a constant, the distribution of  X¯  is approximately normal.
We can convert  X¯  to a standard normal random variable  Z : 
Z=(X¯-E(X¯))/SE(X¯) 
The probability that  X¯  is within .01 of the actual value of  p  is:
Pr(Z <= 0.01/sqrt(p(1-p)/N))-Pr(Z <= 0.01/sqrt(p(1-p)/N))
The Central Limit Theorem (CLT) still works if  X¯  is used in place of  p . This is called a plug-in estimate. Hats over values denote estimates. Therefore:
standard error: SE^(X¯)=sqrt(X¯(1-X¯)/N) 
Using the CLT, the probability that  X¯  is within .01 of the actual value of  p  is:
Pr(Z <= 0.01/ssqrt(X¯(1-X¯)/N))-Pr(Z <= -0.01/ssqrt(X¯(1-X¯)/N)) 
Code: Computing the probability of  X¯  being within .01 of  p 
"
# Computing the probability of  X¯  being within .01 of p # hat denotes the estimate
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)

#===============================================================================
# A Monte Carlo Simulation for the CLT

# Monte Carlo simulation using a set value of p
p <- 0.45    # unknown p to estimate
N <- 100

# simulate one poll of size N and determine x_hat
x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
x_hat

# simulate B polls of size N and determine average x_hat
B <- 10000    # number of replicates
N <- 1000    # sample size per replicate
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})

#-------------------------------------------------------------------------------
# Histogram and QQ-plot of Monte Carlo results
library(tidyverse)
library(gridExtra)
p1 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(x_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
p2 <- data.frame(x_hat = x_hat) %>%
  ggplot(aes(sample = x_hat)) +
  stat_qq(dparams = list(mean = mean(x_hat), sd = sd(x_hat))) +
  geom_abline() +
  ylab("X_hat") +
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)

#===============================================================================
# The Spread
"
The spread between two outcomes with probabilities  p  and  1???p  is  2p???1 .
The expected value of the spread is  2X¯???1 .
The standard error of the spread is  2SE^(X¯) .
The margin of error of the spread is 2 times the margin of error of  X¯ .
"
#===============================================================================
# Bias: Why Not Run a Very Large Poll?

# Plotting margin of error in an extremely large poll over a range of values of p
library(tidyverse)
N <- 100000
p <- seq(0.35, 0.65, length = 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) +
  geom_line()






