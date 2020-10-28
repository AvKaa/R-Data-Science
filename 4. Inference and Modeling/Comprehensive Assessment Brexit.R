# Brexit poll analysis - Part 1

# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
"
Final Brexit parameters
Define  p=0.481  as the actual percent voting 'Remain' on the Brexit referendum and  d=2p???1=???0.038  as the actual spread of the Brexit referendum with 'Remain' defined as the positive outcome:
"
p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread

# Question 1: Expected value and standard error of a poll
"
The final proportion of voters choosing "Remain" was  p=0.481 . Consider a poll with a sample of  N=1500  voters.
"
# a)
N <- 1500
p*N

# b)
se <- sqrt(N*p*(1-p))
se

# c)
ev_X_hat <- p
# Unnecessary Monte Carlo simulation:
B <- 10^6
ex <- sample(c(1,0), B, replace = T, prob = c(p,1-p))
mean(ex)

# d)
se <- sqrt(p*(1-p)/N)
se

# e)
ev_d <- d
ev_d

# f)
X_hat <- (d+1)/2
se <- 2*sqrt(X_hat*(1-X_hat)/N)
se

#-------------------------------------------------------------------------------
# Question 2: Actual Brexit poll estimates
"
Load and inspect the brexit_polls dataset from dslabs, which contains actual polling data for the 6 months before the Brexit vote. Raw proportions of voters preferring "Remain", "Leave", and "Undecided" are available (remain, leave, undecided) The spread is also available (spread), which is the difference in the raw proportion of voters choosing "Remain" and the raw proportion choosing "Leave".
Calculate x_hat for each poll, the estimate of the proportion of voters choosing 'Remain' on the referendum day ( p=0.481 ), given the observed spread and the relationship  d^=2X^???1 . Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below:
"
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
head(brexit_polls)

# a)
mean(brexit_polls$spread)

# b)
sd(brexit_polls$spread)

# c)
mean(brexit_polls$x_hat)

# d)
sd(brexit_polls$x_hat)

#-------------------------------------------------------------------------------
# Question 3: Confidence interval of a Brexit poll
"
Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
"
brexit_polls[1,10]

x_hat <- brexit_polls[1,10]
N <- brexit_polls[1,5]
z <- qnorm(0.975)
se_hat <- sqrt(x_hat*(1-x_hat)/N)
lower <- x_hat-z*se_hat
lower
upper <- x_hat+z*se_hat
upper

# The interval predicts a winner but does not cover the true value of p.

#===============================================================================
# Brexit poll analysis - Part 2

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481
d <- 2*p-1
d

"
Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of 2016-06-01' and later). We will calculate confidence intervals for all polls and determine how many cover the true value of  d .
First, use mutate() to calculate a plug-in estimate se_x_hat for the standard error of the estimate  SE^[X]  for each poll given its sample size and value of  X^  (x_hat). Second, use mutate() to calculate an estimate for the standard error of the spread for each poll given the value of se_x_hat. Then, use mutate() to calculate upper and lower bounds for 95% confidence intervals of the spread. Last, add a column hit that indicates whether the confidence interval for each poll covers the correct spread  d=???0.038 .
"
# Question 4: Confidence intervals for polls in June
z <- qnorm(0.975)

june_polls <- brexit_polls %>% 
  filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
  mutate(se_spread = 2*se_x_hat) %>%
  mutate(lower = spread-z*se_spread, upper = spread+z*se_spread) %>%
  mutate(hit=(lower <= d & upper >= d))

june_polls

# a)
nrow(june_polls)

# b)
cover_zero <- june_polls %>% 
  mutate(lower = x_hat-z*se_hat, upper = x_hat+z*se_hat) %>%
  mutate(hit = (lower <= p & upper >= p)) %>% 
  summarize(mean(hit))
1-cover_zero

# c)
remain <- june_polls %>% 
  mutate(hit = (lower > 0)) %>%
  summarize(mean(hit))
remain

# d)
true_d <- june_polls %>%
  summarize(mean(hit))
true_d

#-------------------------------------------------------------------------------
# Question 5: Hit rate by pollster
"
Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
Which of the following are TRUE?
"
head(june_polls)

june_polls %>% 
  group_by(pollster) %>%
  summarize(proportion_hits = mean(hit), n = n())
"
The results are consistent with a large general bias that affects all pollsters.
"
#-------------------------------------------------------------------------------
# Question 6: Boxplot of Brexit polls by poll type
"
Make a boxplot of the spread in june_polls by poll type.
Which of the following are TRUE?
"
june_polls %>%
  mutate(poll_type = reorder(poll_type, spread)) %>%
  ggplot(aes(poll_type, spread)) + 
  geom_boxplot() + 
  geom_point()

"
Telephone polls tend to show support 'Remain' (spread > 0).
Telephone polls tend to show higher support for 'Remain' than online polls (higher spread).
Online polls have a larger interquartile range (IQR) for the spread than telephone polls, indicating that they are more variable.
Poll type introduces a bias that affects poll results.
"
#-------------------------------------------------------------------------------
# Question 7: Combined spread across poll type
"
Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type. Recall that to determine the standard error of the spread, you will need to double the standard error of the estimate.
Use this code (which determines the total sample size per poll type, gives each spread estimate a weight based on the poll's sample size, and adds an estimate of p from the combined spread) to begin your analysis:
"
june_polls

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_p_hat = sqrt(p_hat*(1-p_hat)/N),
            se_spread = 2*se_p_hat,
            lower = spread-z*se_spread, 
            upper = spread+z*se_spread,
            interval = upper-lower, # for Q8
            hit = lower <= d & upper >= d)

combined_by_type

#-------------------------------------------------------------------------------
# Question 8: Interpreting combined spread estimates across poll type
"
Interpret the confidence intervals for the combined spreads for each poll type calculated in the previous problem.
Which of the following are TRUE about the confidence intervals of the combined spreads for different poll types?
"
"
Neither set of combined polls makes a prediction about the outcome of the Brexit referendum (a prediction is possible if a confidence interval does not cover 0).
The confidence interval for telephone polls is covers more positive values than the confidence interval for online polls.
Neither confidence interval covers the true value of  d=???0.038 .
"
#===============================================================================
# Brexit poll analysis - Part 3

# suggested libraries
library(tidyverse)

# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)

# final proportion voting "Remain"
p <- 0.481

# Question 9: Chi-squared p-value
"
Define brexit_hit, with the following code, which computes the confidence intervals for all Brexit polls in 2016 and then calculates whether the confidence interval covers the actual value of the spread  d=???0.038 :
"
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit) 

totals <- brexit_hit %>%
  group_by(poll_type,hit) %>%
  summarize(num = n()) %>%
  spread(poll_type, num)

chisq_test <- totals %>% 
  select(-hit) %>%
  chisq.test()
chisq_test$p.value

"
Online polls are more likely to cover the correct value of the spread and this difference is statistically significant.
"
#-------------------------------------------------------------------------------
# Question 10: Odds ratio of online and telephone poll hit rate
"
Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between the hit rate of online and telephone polls to determine the magnitude of the difference in performance between the poll types.
"
odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
  (totals[[1,2]] / sum(totals[[2]]))
odds_C

odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
  (totals[[1,3]] / sum(totals[[3]]))
odds_A

odds_C/odds_A

#-------------------------------------------------------------------------------
# Question 11: Plotting spread over time
"
Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type). Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. Include the individual data points colored by poll type. Add a horizontal line indicating the final value of  d=???.038 .
"
head(brexit_polls)

d_t <- brexit_polls %>%
  select(enddate, poll_type, spread) %>%
  group_by(poll_type) %>%
  ungroup() %>%
  ggplot(aes(enddate, spread, color = poll_type)) +  
  geom_point(show.legend = FALSE, alpha=0.4) + 
  geom_smooth(method = "loess", span = 0.4, alpha=0.2) +
  geom_hline(yintercept = -0.038)
d_t

#-------------------------------------------------------------------------------
# Question 12: Plotting raw percentages over time
"
Use the following code to create the object brexit_long, which has a column vote containing the three possible votes on a Brexit poll ('remain', 'leave', 'undecided') and a column proportion containing the raw proportion choosing that vote option on the given poll:
"
brexit_long <- brexit_polls %>%
  select(enddate, poll_type, remain, leave, undecided) %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote)) %>%
  ggplot(aes(enddate, proportion, color = vote)) +  
  geom_point(show.legend = FALSE, alpha = 0.4)  +  
  geom_smooth(method = "loess", span = 0.3, alpha = 0.2) +  
  scale_y_continuous(limits = c(0,1))
brexit_long


















