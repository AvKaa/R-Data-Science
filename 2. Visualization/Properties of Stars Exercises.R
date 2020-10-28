library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)   # report 3 significant digits


# Q1.

mean(stars$magnitude)
sd(stars$magnitude)


# Q2.

stars %>% 
  ggplot(aes(magnitude,y = ..count.., fill = magnitude)) +
  geom_density(alpha = 0.2)


# Q3.

stars %>% 
  ggplot(aes(temp,y = ..count.., fill = magnitude)) +
  geom_density(alpha = 0.2)


# Q4.

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point()


# Q5.

stars %>%
  ggplot(aes(temp, magnitude)) +
  scale_y_reverse() +
  geom_point()
  

# Q8.

stars %>%
  select(temp, magnitude, star) %>%
  ggplot(aes(temp, magnitude, label = star)) +
  scale_y_reverse() +
  geom_label()

# Q9.

stars %>%
  ggplot(aes(temp, magnitude, label = type)) +
  scale_y_reverse() +
  geom_label()

stars






