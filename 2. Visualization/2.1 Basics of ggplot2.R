# You can associate a dataset x with a ggplot object with any of the 3 commands:
ggplot(data = x)
ggplot(x)
x %>% ggplot()

# A ggplot object can be assigned to a variable. If the object is not assigned to a variable, 
# it will automatically be displayed.
# You can display a ggplot object assigned to a variable by printing that variable.

library(tidyverse)
library(dslabs)
data(murders)

ggplot(data = murders) # associates the data set with the plotting object.

murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
print(p)    # this is equivalent to simply typing p
p
