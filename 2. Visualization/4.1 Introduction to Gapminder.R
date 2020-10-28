# Gapminder Dataset

library(tidyverse)
library(dslabs)
data(gapminder)
head(gapminder)


# compare infant mortality in Sri Lanka and Turkey
x <- gapminder %>%  filter(year == 2015 & country %in% c("Sri Lanka", "Turkey")) %>%
  select(country, infant_mortality)

x

#===============================================================================
# Life Expectancy and Fertility Rates

# basic scatterplot of life expectancy versus fertility
ds_theme_set()    # set plot theme
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy)) +
  geom_point()

# add color as continent
filter(gapminder, year == 1962) %>%
  ggplot(aes(fertility, life_expectancy, color = continent)) +
  geom_point()
