library(dslabs)
data(murders)

i <- which.min(murders$population)
i

state <- murders$state

murders$state[i]
state[i]

# Store temperatures in an object 
temp <- c(35, 88, 42, 84, 81, 30)

# Store city names in an object 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Create data frame with city names and temperature 
city_temps <- data.frame(name = city, temperature = temp)

# Define a variable states to be the state names 
states <- murders$state
states
# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)
my_df <- data.frame(name = states, population = ranks)
my_df
# Create a data frame my_df with the state name and its rank
