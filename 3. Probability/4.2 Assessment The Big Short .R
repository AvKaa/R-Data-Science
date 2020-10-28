library(dslabs)
library(tidyverse)

# Q1
# a)
death_prob %>%
  filter(age == 50 , sex == 'Female')
# b)
# ap+b(1-p)
p_death = 0.003193
p_alive = 1-p_death
mu_1 = -150000*p_death+1150*p_alive
mu_1
# c)
# abs(b-a)*sqrt(p(1-p))
se_1 = abs(1150--150000)*sqrt(p_death*p_alive)
se_1
# d)
# n*(ap+b(1-p))
mu = mu_1*1000
mu
# e)
# sqrt(n)*abs(b-a)*sqrt(p(1-p))
se = sqrt(1000)*se_1
se
# f)
# central limit theorem: 
pnorm(0, mu, se)


# Q2
# a)
death_prob %>%
  filter(age == 50 , sex == 'Male')
# b)
p_death = 0.005013
p_alive = 1-p_death
mu = 700000
premium = ((mu/1000)--150000*p_death)/(p_alive)
premium
# c)
# sqrt(n)*abs(b-a)*sqrt(p(1-p))
se = sqrt(1000)*abs(premium--150000)*sqrt(p_death*p_alive)
se
# d)
pnorm(0, mu, se)


# Q3
# a)
p_death = 0.015
p_alive = 1-p_death
mu = (-150000*p_death+1150*p_alive)*1000
mu
# b)
se = sqrt(1000)*abs(1150--150000)*sqrt(p_death*p_alive)
se
# c)
pnorm(0, mu, se)
# d)
pnorm(-1000000, mu, se)
# e)
p <- seq(.01, .03, .001)
p_loss <- function(p){
  mu = (-150000*p+1150*(1-p))*1000
  se = sqrt(1000)*abs(1150--150000)*sqrt(p*(1-p))
  pnorm(0, mu, se)
}
sapply(p,p_loss)
# f)
p <- seq(.01, .03, .0025)
p_loss <- function(p){
  mu = (-150000*p+1150*(1-p))*1000
  se = sqrt(1000)*abs(1150--150000)*sqrt(p*(1-p))
  pnorm(-1000000, mu, se)
}
losses <- sapply(p,p_loss)
df <- data.frame(p, losses)
df


# Q4
# a)
set.seed(25)
n <- 1000
p_loss <- 0.015
X <- sample(c(0,1), n, replace=T, prob=c((1-p_loss),p_loss))
loss <- -150000*sum(X==1)/10^6 
profit <- 1150*sum(X==0)/10^6
loss+profit

# b)
set.seed(27)
B <- 10000
n <- 1000
p <- 0.015
profit <- replicate(B, {
  draws <- sample( c(-150000, 1150), n, prob=c(p, 1-p), replace = T) 
  sum(draws)
})
mean(profit < -1000000)


# Q5
# a)
n <- 1000
p <- 0.015
l <- -150000
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
# b)
mu_1 <- (l*p_death+x*p_alive)
mu_1
# c)
mu = mu_1*n
mu
# d)
set.seed(28)
B <- 10000
n <- 1000
profit <- replicate(B, {
  draws <- sample( c(-150000, x), n, prob=c(p, 1-p), replace = T) 
  sum(draws)
})
mean(profit < 0)


# Q6
# a)
set.seed(29)
B <- 10000
n <- 1000
p <- 0.015
profit <- replicate(B, {
  p <- p+sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(l,x), n, prob=c(p,1-p), replace = T)
  sum(draws)
})
mean(profit)
# b)
mean(profit < 0)
# c)
mean(profit < -1000000)

