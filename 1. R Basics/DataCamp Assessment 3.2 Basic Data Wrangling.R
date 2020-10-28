# Use the function mutate to add a column rank containing the rank, from highest to lowest murder rate. Make sure you redefine murders.

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)
# Defining rate
rate <-  murders$total/ murders$population * 100000
# Redefine murders to include a column named rank
rank <- rank(-rate)
murders <- mutate(murders, rank = rank)
head(murders)
# with the ranks of rate from highest to lowest
murders[with(murders,order(murders$rank)),]

#------------------------------------------------------------------------------
# Use filter to show the top 5 states with the highest murder rates. After we add murder rate and rank, do not change the murders dataset, just show the result. Note that you can filter based on the rank column.

# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
# Filter to show the top 5 states with the highest murder rates
filter(murders,rank <= 5)

#------------------------------------------------------------------------------
# Create a new data frame called no_south that removes states from the South region.
# How many states are in this category? You can use the function nrow for this.

# Use filter to create a new data frame no_south
no_south <- !murders$region == "South"
#no_south <- !no_south
no_south <- filter(murders,no_south)
no_south
# Use nrow() to calculate the number of rows
nrow(no_south)

#------------------------------------------------------------------------------
# Create a new data frame called murders_nw with only the states from the Northeast and the West.
# How many states are in this category?

# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders,region %in% c("Northeast","West"))
murders_nw
# Number of states (rows) in this category 
nrow(murders_nw)

#------------------------------------------------------------------------------
# Add a murder rate column and a rank column as done before
# Create a table, call it my_states, that satisfies both the conditions: it is in the Northeast or West and the murder rate is less than 1.
# Use select to show only the state name, the rate and the rank

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# Create a table, call it my_states, that satisfies both the conditions 
my_states <- filter(murders,rate < 1 & region %in% c("Northeast","West"))
my_states
# Use select to show only the state name, the murder rate and the rank
my_states %>% select(state,rate,rank)

#------------------------------------------------------------------------------
#Repeat the previous exercise, but now instead of creating a new object, show the result and only include the state, rate, and rank columns in that order.
#Use a pipe %>% to do this in just one line.

# Load library
library(dplyr)
## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))
# show the result and only include the state, rate, and rank columns, all in one line, in that order
murders %>% filter(region %in% c('Northeast', 'West') & rate < 1) %>%  select(state, rate, rank)

#------------------------------------------------------------------------------
# Write all above criterias into one line of code:

# Loading the libraries
library(dplyr)
data(murders)
# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate = total/population*100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast","West") & rate < 1) %>% select(state, rate, rank)
my_states













