# Exercise 1. Life expectancy vs fertility - part 1

# Using ggplot and the points layer, create a scatter plot of life expectancy versus fertility for the African continent in 2012.
# Remember that you can use the R console to explore the gapminder dataset to figure out the names of the columns in the dataframe.
# In this exercise we provide parts of code to get you going. You need to fill out what is missing. But note that going forward, in the next exercises, you will be required to write most of the code.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
## fill out the missing parts in filter and aes
gapminder %>% filter(year %in% 2012 & region %in% c("Northern Africa","Middle Africa","Southern Africa","Western Africa","Eastern Africa")) %>%
  ggplot(aes(fertility ,life_expectancy)) +
  geom_point()

#-------------------------------------------------------------------------------
# Exercise 2. Life expectancy vs fertility - part 2 - coloring your plot

# Remake the plot from the previous exercises but this time use color to distinguish the different regions of Africa to see if this explains the clusters. Remember that you can explore the gapminder data to see how the regions of Africa are labeled in the data frame!
# Use color rather than col inside your ggplot call - while these two forms are equivalent in R, the grader specifically looks for color.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder %>% filter(year %in% 2012 & region %in% c("Northern Africa","Middle Africa","Southern Africa","Western Africa","Eastern Africa")) %>%
  ggplot(aes(fertility ,life_expectancy,color = region)) +
  geom_point()

#-------------------------------------------------------------------------------
# Exercise 3. Life expectancy vs fertility - part 3 - selecting country and region

# Create a table showing the country and region for the African countries (use select) that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
# Assign your result to a data frame called df.

library(dplyr)
library(dslabs)
data(gapminder)

df <- gapminder %>%  
  filter(year %in% 2012 & region %in% c("Northern Africa","Middle Africa","Southern Africa","Western Africa","Eastern Africa") & fertility <= 3 & life_expectancy >= 70) %>% 
  select(country,region) 

#-------------------------------------------------------------------------------
# Exercise 4. Life expectancy and the Vietnam War - part 1

# Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
# Save the table in an object called tab.

library(dplyr)
library(dslabs)
data(gapminder)

tab <- gapminder %>%  filter(country %in% c("Vietnam","United States") & year <= 2010) 

#-------------------------------------------------------------------------------
# Exercise 5. Life expectancy and the Vietnam War - part 2

# Use geom_line to plot life expectancy vs year for Vietnam and the United States and save the plot as p. The data table is stored in tab.
# Use color to distinguish the two countries.
# Print the object p.

p <- tab %>% 
  ggplot(aes(year,life_expectancy , color = country)) +
  geom_line()

#-------------------------------------------------------------------------------
# Exercise 6. Life expectancy in Cambodia

# Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder %>% filter(country == "Cambodia" & year <= 2010) %>% ggplot(aes(year,life_expectancy)) + geom_line()

#-------------------------------------------------------------------------------
# Exercise 7. Dollars per day - part 1

# Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
# Create the dollars_per_day variable for African countries for the year 2010.
# Remove any NA values.
# Save the mutated dataset as daydollars.

library(dplyr)
library(dslabs)
data(gapminder)

daydollars <- filter(gapminder, continent == "Africa" & !is.na(gdp) & year == 2010) %>% mutate(dollars_per_day = gdp/population/365)  

#-------------------------------------------------------------------------------
# Exercise 8. Dollars per day - part 2

# Now we are going to calculate and plot dollars per day for African countries in 2010 using GDP data.
# In the second part of this analysis, we will plot the smooth density plot using a log (base 2) x axis.

p <- daydollars %>%
  ggplot(aes(x=dollars_per_day))

p + geom_density() +   
  scale_x_continuous(trans = "log2") 

#-------------------------------------------------------------------------------
# Exercise 9. Dollars per day - part 3 - multiple density plots

# Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
#   Make sure you remove any NA values.
# Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
# Use facet_grid to show a different density plot for 1970 and 2010.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

daydollars <- gapminder %>%
  filter(continent == "Africa" & !is.na(gdp) & year %in% c(1970,2010)) %>% mutate(dollars_per_day = gdp/population/365)

daydollars

p <- daydollars %>%
  ggplot(aes(x = dollars_per_day)) +
  scale_x_continuous(trans = "log2")

p + geom_density() + facet_grid(year~.)

#-------------------------------------------------------------------------------
# Exercise 10. Dollars per day - part 4 - stacked density plot

# Much of the code will be the same as in Exercise 9:
#  Create the dollars_per_day variable as in Exercise 7, but for African countries in the years 1970 and 2010 this time.
# Make sure you remove any NA values.
# Create a smooth density plot of dollars per day for 1970 and 2010 using a log (base 2) scale for the x axis.
# Use facet_grid to show a different density plot for 1970 and 2010.
# Make sure the densities are smooth by using bw = 0.5.
# Use the fill and position arguments where appropriate to create the stacked density plot of each region.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

head(gapminder)

daydollars <- gapminder %>%
  filter(continent == "Africa" & !is.na(gdp) & year %in% c(1970,2010)) %>% mutate(dollars_per_day = gdp/population/365)

head(daydollars)

p <- daydollars %>%
  ggplot(aes(dollars_per_day, y = ..count.. , fill = region)) +
  scale_x_continuous(trans = "log2")

p + geom_density(alpha = 0.2, bw = 0.5, position = "stack") + facet_grid(year~.)

#-------------------------------------------------------------------------------
# Exercise 11. Infant mortality scatter plot - part 1

# Generate dollars_per_day using mutate and filter for the year 2010 for African countries.
#   Remember to remove NA values.
# Store the mutated dataset in gapminder_Africa_2010.
# Make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
# Use color to denote the different regions of Africa.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder_Africa_2010 <- gapminder %>%
  filter(continent == "Africa" & !is.na(gdp) & year == 2010) %>% mutate(dollars_per_day = gdp/population/365)

head(gapminder_Africa_2010)

gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +
  geom_point()

#-------------------------------------------------------------------------------
# Exercise 12. Infant mortality scatter plot - part 2 - logarithmic axis

# Transform the x axis to be in the log (base 2) scale.

gapminder_Africa_2010 %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region)) +   
  scale_x_continuous(trans = "log2") + 
  geom_point()

#-------------------------------------------------------------------------------
# Exercise 13. Infant mortality scatter plot - part 3 - adding labels

# The mutated dataset is preloaded as gapminder_Africa_2010.
# As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# As in the previous exercise, transform the x axis to be in the log (base 2) scale.
# Add a geom_text layer to display country names in addition to of points.

gapminder_Africa_2010 %>%   
  ggplot(aes(dollars_per_day, infant_mortality, color = region , label = country)) +       
  scale_x_continuous(trans = "log2") + 
  geom_point() +
  geom_text(data = gapminder_Africa_2010) 

head(gapminder_Africa_2010)

#-------------------------------------------------------------------------------
# Exercise 14. Infant mortality scatter plot - part 4 - comparison of scatter plots

# Generate dollars_per_day using mutate and filter for the years 1970 and 2010 for African countries.
#   Remember to remove NA values.
# As in the previous exercise, make a scatter plot of infant_mortality versus dollars_per_day for countries in the African continent.
# As in the previous exercise, use color to denote the different regions of Africa.
# As in the previous exercise, transform the x axis to be in the log (base 2) scale.
# As in the previous exercise, add a layer to display country names instead of points.
# Use facet_grid to show different plots for 1970 and 2010. Align the plots vertically.

library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)

gapminder_Africa_1970_2010 <- gapminder %>%
  filter(continent == "Africa" & !is.na(gdp) & !is.na(infant_mortality) & year %in% c(1970,2010)) %>% 
  mutate(dollars_per_day = gdp/population/365)

gapminder_Africa_1970_2010 %>%
  ggplot(aes(dollars_per_day, infant_mortality, color = region , label = country)) +       
  scale_x_continuous(trans = "log2") + 
  geom_point() +
  geom_text(data = gapminder_Africa_1970_2010) +
  facet_grid(year~.)














