# loading the dslabs package and the murders dataset
library(dslabs)
data(murders)

# determining that the murders dataset is of the "data frame" class
class(murders)
# finding out more about the structure of the object
str(murders)
# showing the first 6 lines of the dataset
head(murders)

# $ is the accessor x$y where x is the file name and y is the colums in the data frame

# using the accessor operator to obtain the population column
murders$population
# displaying the variable names in the murders dataset
names(murders)
# determining how many entries are in a vector
popped <- murders$population
length(popped)
# vectors can be of class numeric and character
class(popped)
class(murders$state)

# logical vectors are either TRUE or FALSE
# == is a logical operator
z <- 3 == 2
z
class(z)
# class is used to see wwhat type a charater is

# factors are another type of class
class(murders$region)
# obtaining the levels of a factor
levels(murders$region)


