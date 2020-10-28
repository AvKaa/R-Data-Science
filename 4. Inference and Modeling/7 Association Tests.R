# Association Tests
"
We learn how to determine the probability that an observation is due to random variability given categorical, binary or ordinal data.
Fisher's exact test determines the p-value as the probability of observing an outcome as extreme or more extreme than the observed outcome given the null distribution.
Data from a binary experiment are often summarized in two-by-two tables.
The p-value can be calculated from a two-by-two table using Fisher's exact test with the function fisher.test().
"
# Research funding rates example
# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#-------------------------------------------------------------------------------
# Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

#===============================================================================
# Chi-Squared Tests
"
If the sums of the rows and the sums of the columns in the two-by-two table are fixed, then the hypergeometric distribution and  Fisher's exact test can be used. Otherwise, we must use the chi-squared test.
The chi-squared test compares the observed two-by-two table to the two-by-two table expected by the null hypothesis and asks how likely it is that we see a deviation as large as observed or larger by chance.
The function chisq.test() takes a two-by-two table and returns the p-value from the chi-squared test.
The odds ratio states how many times larger the odds of an outcome are for one group relative to another group.
A small p-value does not imply a large odds ratio. If a finding has a small p-value but also a small odds ratio, it may not be a practically significant or scientifically significant finding. 
Because the odds ratio is a ratio of ratios, there is no simple way to use the Central Limit Theorem to compute confidence intervals. There are advanced methods for computing confidence intervals for odds ratios that we do not discuss here.
"
# Chi-squared test
# compute overall funding rate
funding_rate <- totals %>%
    summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
    .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                    men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
           men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
           women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

#-------------------------------------------------------------------------------
# Odds ratio
# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

#-------------------------------------------------------------------------------
# p-value and odds ratio responses to increasing sample size
# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()














