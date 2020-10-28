# Exercise 1. Sample average
"
Write function called take_sample that takes the proportion of Democrats p and the sample size N as arguments and returns the sample average of Democrats (1) and Republicans (0).
Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.

Define a function called take_sample that takes p and N as arguments.
Use the sample function as the first statement in your function to sample N elements from a vector of options where Democrats are assigned the value '1' and Republicans are assigned the value '0' in that order.
Use the mean function as the second statement in your function to find the average value of the random sample.
"
# Write a function called `take_sample` that takes `p` and `N` as arguements and returns the average value of a randomly sampled population.
take_sample <- function(p,N){
  x_hat <- sample(c(1,0), size = N, replace = T, prob = c(p, 1-p))
  mean(x_hat)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p,N)

#===============================================================================
# Exercise 2. Distribution of errors - 1
"
Assume the proportion of Democrats in the population p equals 0.45 and that your sample size N is 100 polled voters. The take_sample function you defined previously generates our estimate, X¯.
Replicate the random sampling 10,000 times and calculate p???X¯ for each random sample. Save these differences as a vector called errors. Find the average of errors and plot a histogram of the distribution.

The function take_sample that you defined in the previous exercise has already been run for you.
Use the replicate function to replicate subtracting the result of take_sample from the value of p 10,000 times.
Use the mean function to calculate the average of the differences between the sample average and actual value of p.
"
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# The variable `B` specifies the number of times we want the sample to be replicated
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Create an objected called `errors` that replicates subtracting the result of the `take_sample` function from `p` for `B` replications
errors <- replicate(B, {
  p-take_sample(p,N)
})

# Calculate the mean of the errors. Print this value to the console.
mean(errors)

#===============================================================================
# Exercise 12. Estimating the probability of a specific value of X-bar
"
If p=0.45 and N=100, use the central limit theorem to estimate the probability that X¯>0.5.

Use pnorm to define the probability that a value will be greater than 0.5.
"
# Define `p` as the proportion of Democrats in the population being polled
p <- 0.45

# Define `N` as the number of people polled
N <- 100

# Calculate the probability that the estimated proportion of Democrats in the population is greater than 0.5. Print this value to the console.
se <- sqrt(p*(1-p)/N)
1-pnorm(0.5, mean = p, se)

#===============================================================================
# Exercise 13. Estimating the probability of a specific error size
"
Assume you are in a practical situation and you don't know p. Take a sample of size N=100 and obtain a sample average of X¯=0.51.
What is the CLT approximation for the probability that your error size is equal or larger than 0.01?

Calculate the standard error of the sample average using the sqrt function.
Use pnorm twice to define the probabilities that a value will be less than -0.01 or greater than 0.01.
Combine these results to calculate the probability that the error size will be 0.01 or larger.
"
# Define `N` as the number of people polled
N <-100

# Define `X_hat` as the sample average
X_hat <- 0.51

# Define `se_hat` as the standard error of the sample average
se_hat <- sqrt(X_hat*(1-X_hat)/N)

# Calculate the probability that the error is 0.01 or larger
p1 <- pnorm(0.01, 0, se_hat) 
p2 <- pnorm(-0.01, 0, se_hat)

1-p1+p2






