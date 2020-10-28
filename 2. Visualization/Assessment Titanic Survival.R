options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

# ?titanic_train
titanic

titanic %>%
  select(Sex, Age) %>% filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  ggplot(aes(Age, y = ..count.., fill = Sex)) +
  geom_density(alpha = 0.2, bw = 2) 
  

# Q3

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
p

# Q4

p <- titanic %>%
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
p

# Q5

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2, bw = 2) 

# Q6

titanic

titanic %>%
  filter(!Fare == 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = F) +
  geom_jitter()

# Q7
# i)

titanic %>%
  filter(!Fare == 0) %>%
  ggplot(aes(Pclass, y = ..count.., fill = Survived)) +
  geom_bar(position = position_dodge())

# ii)

titanic %>%
  filter(!Fare == 0) %>%
  ggplot(aes(Pclass, y = ..count.., fill = Survived)) +
  geom_bar(position = position_fill())

# iii)

titanic %>%
  filter(!Fare == 0) %>%
  ggplot(aes(Survived, y = ..count.., fill = Pclass)) +
  geom_bar(position = position_fill())

# Q8

head(titanic)

titanic %>%
  ggplot(aes(Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) + 
  facet_grid(Pclass ~ Sex)







