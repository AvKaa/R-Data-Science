# Election Forecasting
"
In our model:
The spread d ~ N(mu, tau) describes our best guess in the absence of polling data. We set mu = 0 and tau = 0.035 using historical data. (expected value mu and standard error tau)
The average of observed data X_bar l d ~ N(d, sigma) describes randomness due to sampling and the pollster effect.
Because the posterior distribution is normal, we can report a 95% credible interval that has a 95% chance of overlapping the parameter using E(p l Y) and SE(p l Y).
Given an estimate of E(p l Y) and SE(p l Y), we can use pnorm to compute the probability that d > 0.
It is common to see a general bias that affects all pollsters in the same way. This bias cannot be predicted or measured before the election. We will include a term in later models to account for this variability.
"
#-------------------------------------------------------------------------------
# Definition of results object
library(tidyverse)
library(dslabs)
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end = avg + 1.96*se)

#-------------------------------------------------------------------------------
# Computing the posterior mean, standard error, credible interval and probability
mu <- 0
tau <- 0.035 # based on historical data
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

posterior_mean
posterior_se

# 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

# probability of d > 0
1 - pnorm(0, posterior_mean, posterior_se)

#===============================================================================
# Mathematical Representations of Models
"
If we collect several polls with measured spreads  X1,...,Xj  with a sample size of  N , these random variables have expected value  d  and standard error  2p(1???p)/N????????????????????????????????? .
We represent each measurement as  Xi,j=d+b+hi+??i,j  where:
The index  i  represents the different pollsters
The index  j  represents the different polls
Xi,j  is the  j th poll by the  i th pollster 
d  is the actual spread of the election
b  is the general bias affecting all pollsters
hi  represents the house effect for the  i th pollster
??i,j  represents the random error associated with the  i,j th poll.
The sample average is now  X¯=d+b+1N???i=1NXi  with standard deviation  SE(X¯)=??2/N+??2b?????????????????????????????? .
The standard error of the general bias  ??b  does not get reduced by averaging multiple polls, which increases the variability of our final estimate.
"
#-------------------------------------------------------------------------------
# Simulated data with  Xj=d+??j
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

#-------------------------------------------------------------------------------
# Simulated data with  Xi,j=d+??i,j
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#-------------------------------------------------------------------------------
# Simulated data with  Xi,j=d+hi+??i,j (hi =  house effect for the ith pollster)
I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})

#-------------------------------------------------------------------------------
# Calculating probability of  d>0  with general bias

# Note that sigma now includes an estimate of the variability due to general bias  ??b=.025
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#===============================================================================
# Predicting the Electoral College
"
In the US election, each state has a certain number of votes that are won all-or-nothing based on the popular vote result in that state (with minor exceptions not discussed here).
We use the left_join() function to combine the number of electoral votes with our poll results.
For each state, we apply a Bayesian approach to generate an Election Day  d . We keep our prior simple by assuming an expected value of 0 and a standard deviation based on recent history of 0.02.
We can run a Monte Carlo simulation that for each iteration simulates poll results in each state using that state's average and standard deviation, awards electoral votes for each state to Clinton if the spread is greater than 0, then compares the number of electoral votes won to the number of votes required to win the election (over 269).
If we run a Monte Carlo simulation for the electoral college without accounting for general bias, we overestimate Clinton's chances of winning at over 99%.
If we include a general bias term, the estimated probability of Clinton winning decreases significantly.
"
# Top 5 states ranked by electoral votes
# The results_us_election_2016 object is defined in the dslabs package:
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

#-------------------------------------------------------------------------------
# Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))

# 10 closest races = battleground states
results %>% arrange(abs(avg))

# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by="state")

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

#-------------------------------------------------------------------------------
# Calculating the posterior mean and posterior standard error
# Note there is a small error in the video code: B should be defined as sigma^2/(sigma^2 + tau^2).

mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

#-------------------------------------------------------------------------------
# Monte Carlo simulation of Election Night results (no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV > 269)    # over 269 votes wins election

# histogram of outcomes
data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

#-------------------------------------------------------------------------------
# Monte Carlo simulation including general bias

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

#===============================================================================
# Forecasting
"
In poll results,  p  is not fixed over time. Variability within a single pollster comes from time variation.
In order to forecast, our model must include a bias term  bt  to model the time effect.
Pollsters also try to estimate  f(t) , the trend of  p  given time  t  using a model like:
Yi,j,t=d+b+hj+bt+f(t)+??i,j,t 
Once we decide on a model, we can use historical data and current data to estimate the necessary parameters to make predictions.
"
# Variability across one pollster
# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
    filter(pollster == "Ipsos" & state == "U.S.") %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
    summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
    geom_histogram(binwidth = 0.01, color = "black")

#-------------------------------------------------------------------------------
# Trend across time for several pollsters
polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-07-01") %>%
    group_by(pollster) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
    ggplot(aes(enddate, spread)) +
    geom_smooth(method = "loess", span = 0.1) +
    geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#-------------------------------------------------------------------------------
# Plotting raw percentages across time
polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-07-01") %>%
    select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
    rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
    gather(candidate, percentage, -enddate, -pollster) %>%
    mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
    group_by(pollster) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    ggplot(aes(enddate, percentage, color = candidate)) +
    geom_point(show.legend = FALSE, alpha = 0.4) +
    geom_smooth(method = "loess", span = 0.15) +
    scale_y_continuous(limits = c(30, 50))

#===============================================================================
# The t-Distribution
"
In models where we must estimate two parameters,  p  and  ?? , the Central Limit Theorem can result in overconfident confidence intervals for sample sizes smaller than approximately 30.
If the population data are known to follow a normal distribution, theory tells us how much larger to make confidence intervals to account for estimation of  ?? .
Given  s  as an estimate of  ?? , then  Z=X¯???ds/N???  follows a t-distribution with  N???1  degrees of freedom.
Degrees of freedom determine the weight of the tails of the distribution. Small values of degrees of freedom lead to increased probabilities of extreme values.
We can determine confidence intervals using the t-distribution instead of the normal distribution by calculating the desired quantile with the function qt().
"
# Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
one_poll_per_pollster %>%
  summarize(avg = mean(spread), moe = z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

# quantile from t-distribution versus normal distribution
qt(0.975, 14)    # 14 = nrow(one_poll_per_pollster) - 1
qnorm(0.975)
















