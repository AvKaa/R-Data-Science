# Exercise 1. Bank earnings
"
Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.
Create a random variable \(S\) that contains the earnings of your bank. Calculate the total amount of money lost in this scenario.

Using the sample function, generate a vector called defaults that contains n samples from a vector of c(0,1), where 0 indicates a payment and 1 indicates a default
Multiply the total number of defaults by the loss per foreclosure.
"

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03
p_payment <- 1-p_default

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults <- sample(c(0,1), n, replace = T, prob = c(p_payment,p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S <- sum(defaults*loss_per_foreclosure)
S

#===============================================================================
# Exercise 2. Bank earnings Monte Carlo
"
Run a Monte Carlo simulation with 10,000 outcomes for S, the sum of losses over 10,000 loans. Make a histogram of the results.

Within a replicate loop with 10,000 iterations, use sample to generate a list of 10,000 loan outcomes: payment (0) or default (1). Use the outcome order c(0,1) and probability of default p_default.
Still within the loop, use the function sum to count the number of foreclosures multiplied by loss_per_foreclosure to return the sum of all losses across the 10,000 loans. If you do not take the sum inside the replicate loop, DataCamp may crash with a "Session Expired" error.
Plot the histogram of values using the function hist.
"

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03
p_payment <- 1-p_default
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S <- replicate(B, {
  defaults <- sample(c(0,1), n, replace = T, prob = c(p_payment,p_default))
  sum(defaults*loss_per_foreclosure)
})

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

#===============================================================================
# Exercise 3. Bank earnings expected value
"
What is the expected value of S, the sum of losses over 10,000 loans? For now, assume a bank makes no money if the loan is paid.
Using the chances of default (p_default), calculate the expected losses over 10,000 loans.
"

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Calculate the expected loss due to default out of 10,000 loans
n*(p_default*loss_per_foreclosure + (1-p_default)*0)

#===============================================================================
# Exercise 4. Bank earnings standard error
"
What is the standard error of S?
Compute the standard error of the random variable S you generated in the previous exercise, the summed outcomes of 10,000 loans.
"

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Compute the standard error of the sum of 10,000 loans
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default)) 

#===============================================================================
# Exercise 5. Bank earnings interest rate - 1
"
So far, we've been assuming that we make no money when people pay their loans and we lose a lot of money when people default on their loans. Assume we give out loans for $180,000. How much money do we need to make when people pay their loans so that our net loss is $0?
In other words, what interest rate do we need to charge in order to not lose money?

If the amount of money lost or gained equals 0, the probability of default times the total loss per default equals the amount earned per probability of the loan being paid.
Divide the total amount needed per loan by the loan amount to determine the interest rate.
"

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x <- -loss_per_foreclosure*p_default/(1-p_default)

# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

#===============================================================================
# Exercise 6. Bank earnings interest rate - 2
"
With the interest rate calculated in the last example, we still lose money 50% of the time. What should the interest rate be so that the chance of losing money is 1 in 20?
In math notation, what should the interest rate be so that Pr(S<0)=0.05?
Remember that we can add a constant to both sides of the equation to get:
  Pr(S???E[S]SE[S]<???E[S]SE[S])
which is:
  Pr(Z<???[lp+x(1???p)]n(x???l)np(1???p)???????????????????????????)=0.05
Let z = qnorm(0.05) give us the value of z for which:
  Pr(Z???z)=0.05

Use the qnorm function to compute a continuous variable at given quantile of the distribution to solve for z.
In this equation, l, p, and n are known values. Once you've solved for z, solve for x.
Divide x by the loan amount to calculate the rate.
"

# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Generate a variable `z` using the `qnorm` function
z <- qnorm(0.05)
z

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
x <- -loss_per_foreclosure*(n*p_default - z*sqrt(n*p_default*(1-p_default)))/ ( n*(1-p_default) + z*sqrt(n*p_default*(1-p_default)))
x

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000
