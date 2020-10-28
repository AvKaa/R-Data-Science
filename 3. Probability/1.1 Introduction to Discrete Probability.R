# Monte Carlo Simulations

# Monte Carlo simulations model the probability of different outcomes 
# by repeating a random process a large enough number of times that the results 
# are similar to what would be observed if the process were repeated forever.

beads <- rep(c("red", "blue"), times = c(2,3))    # create an urn with 2 red, 3 blue
beads    # view beads object
sample(beads, 1)    # sample 1 bead at random

B <- 10000    # number of times to draw 1 bead
events <- replicate(B, sample(beads, 1))    # draw 1 bead, B times
tab <- table(events)    # make a table of outcome counts
tab    # view count table
prop.table(tab)    # view table of outcome proportions

# sample fucntion can be used directly again wtihout the replicate to repeat 
# the same experiment of picking 1 out of 5 beads over and over under the same conditions.
event <- sample(beads, B, replace = T)
prop.table(table(events))

#===============================================================================
# Setting the Random Seed

?set.seed
# .Random.seed - an integer vector, containing the random number generator (RNG) state for random number generation in R. 
# It can be saved and restored, but should not be altered by the user.

# R was recently updated to version 3.6 in early 2019. 
# In this update, the default method for setting the seed changed. 
# This means that exercises, videos, textbook excerpts and other code you encounter online may yield a different result based on your version of R.

set.seed(1)
set.seed(1, sample.kind="Rounding") # will make R 3.6 generate a seed as in R 3.5

#===============================================================================
# Using the mean Function for Probability

# In R, applying the mean() function to a logical vector returns the proportion of elements that are TRUE. 
# It is very common to use the mean() function in this way to calculate probabilities and we will do so throughout the course.

# Suppose you have the vector beads from a previous video:
beads <- rep(c("red", "blue"), times = c(2,3))
beads
[1] "red" "red" "blue" "blue" "blue"

# To find the probability of drawing a blue bead at random, you can run:
mean(beads == "blue")
[1] 0.6  
  
# This code is broken down into steps inside R. 
# First, R evaluates the logical statement beads == "blue", which generates the vector:
FALSE FALSE TRUE TRUE TRUE

# When the mean function is applied, R coerces the logical values to numeric values, changing TRUE to 1 and FALSE to 0:
0 0 1 1 1

# The mean of the zeros and ones thus gives the proportion of TRUE values. 
# As we have learned and will continue to see, probabilities are directly related to the proportion of events that satisfy a requirement.

#===============================================================================
# Independence
"
Conditional probabilities compute the probability that an event occurs given information about dependent events. 
For example, the probability of drawing a second king given that the first draw is a king is:
  Pr(Card 2 is a king???Card 1 is a king)=3/51 
If two events  A  and  B  are independent,  Pr(A???B)=Pr(A) .
To determine the probability of multiple events occurring, we use the multiplication rule.

The multiplication rule for independent events is:
Pr(A and B and C)=Pr(A)×Pr(B)×Pr(C) 
The multiplication rule for dependent events considers the conditional probability of both events occurring:
Pr(A and B)=Pr(A)×Pr(B???A) 
We can expand the multiplication rule for dependent events to more than 2 events:
Pr(A and B and C)=Pr(A)×Pr(B???A)×Pr(C???A and B)
"













