# The Big Short: Interest Rates Explained

# Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#-------------------------------------------------------------------------------
# Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B, {
  defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE) 
  sum(defaults * loss_per_foreclosure)
})

#-------------------------------------------------------------------------------
# Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram(binwidth = 0.6, col = "black")

#-------------------------------------------------------------------------------
# Expected value and standard error of the sum (S) of 1,000 loans
n*(p*loss_per_foreclosure + (1-p)*0)    # expected value 
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))    # standard error

#-------------------------------------------------------------------------------
# Calculating interest rates for expected value of 0
"
We can calculate the amount [Math Processing Error] to add to each loan so that the expected value is 0 using the equation [Math Processing Error]. Note that this equation is the definition of expected value given a loss per foreclosure [Math Processing Error] with foreclosure probability [Math Processing Error] and profit [Math Processing Error] if there is no foreclosure (probability [Math Processing Error]).
We solve for [Math Processing Error] and calculate [Math Processing Error]:
"
x = - loss_per_foreclosure*p/(1-p)
x
# On a $180,000 loan, this equals an interest rate of:
x/180000

#-------------------------------------------------------------------------------
# Calculating interest rate for 1% probability of losing money
"
We want to calculate the value of [Math Processing Error] for which [Math Processing Error]. The expected value [Math Processing Error] of the sum of [Math Processing Error] loans given our definitions of [Math Processing Error], [Math Processing Error] and [Math Processing Error] is:

[Math Processing Error]
And the standard error of the sum of [Math Processing Error] loans, [Math Processing Error], is:

[Math Processing Error]
Because we know the definition of a Z-score is [Math Processing Error], we know that [Math Processing Error]. Thus, [Math Processing Error] equals:

[Math Processing Error]
z<-qnorm(0.01) gives us the value of [Math Processing Error] for which [Math Processing Error], meaning:

[Math Processing Error]
Solving for [Math Processing Error] gives:

[Math Processing Error]
"

#-------------------------------------------------------------------------------
# Calculating interest rate for 1% probability of losing money

l <- loss_per_foreclosure
z <- qnorm(0.01)
z
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x/180000    # interest rate
loss_per_foreclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p)) # expected value of the profit over n loans

#-------------------------------------------------------------------------------
# Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money

#===============================================================================
# Expected value with higher default rate and interest rate
p <- .04
loss_per_foreclosure <- -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#-------------------------------------------------------------------------------
# Equations: Probability of losing money
"
We can define our desired probability of losing money, [Math Processing Error], as:
  
[Math Processing Error]
If [Math Processing Error] is the expected value of the urn (one loan) and [Math Processing Error] is the standard deviation of the urn (one loan), then [Math Processing Error] and [Math Processing Error].

As in the previous video, we define the probability of losing money [Math Processing Error]. In the first equation, we can see that:
  
[Math Processing Error]
It follows that:
  
[Math Processing Error]
To find the value of [Math Processing Error] for which [Math Processing Error] is less than or equal to our desired value, we take [Math Processing Error] and solve for [Math Processing Error]:
  
[Math Processing Error]
"

#-------------------------------------------------------------------------------
# Calculating number of loans for desired probability of losing money
# number of loans required:
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#-------------------------------------------------------------------------------
# Monte Carlo simulation with known default probability
# This Monte Carlo simulation estimates the expected profit given a known probability of default [Math Processing Error]. 
# Note that your results will differ from the video because the seed is not set.
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)

#-------------------------------------------------------------------------------
# Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million










