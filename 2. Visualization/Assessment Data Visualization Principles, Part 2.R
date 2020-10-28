# Exercise 1: Customizing plots - watch and learn

library(dplyr)
library(ggplot2)
library(dslabs)

head(us_contagious_diseases)

dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting) 

head(dat)

state <- dat$state %>% reorder(rate)
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)

levels(state)

#===============================================================================
# Exercise 2: Customizing plots - redefining

library(dplyr)
library(ggplot2)
library(dslabs)
data(us_contagious_diseases)
dat <- us_contagious_diseases %>% 
  filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state,rate))

head(dat)

dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#===============================================================================
# Exercise 4: Making a box plot

# Order the regions by their median murder rate by using mutate and reorder.
# Make a box plot of the murder rates by region.
# Show all of the points on the box plot.

library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")

murders %>% 
  mutate(rate = total/population*100000) %>%
  mutate(region = reorder(region,rate,FUN = median)) %>%
  ggplot(aes(region,rate)) +
  geom_point() +
  geom_boxplot()










