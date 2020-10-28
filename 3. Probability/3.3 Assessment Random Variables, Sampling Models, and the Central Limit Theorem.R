# Questions 1 and 2: SAT testing

correct <- 1/5
inccorect <- 1 - correct

# Q1.
# a)
1/5
# b)
ap+b(1-p)
1*correct+-0.25*inccorect
# c)
n*(ap+b(1-p))
n*0
# d)
sqrt(n)*abs(b-a)*sqrt(p(1-p))
sqrt(44)*abs(-0.25-1)*sqrt(correct*inccorect)
# e)
1-pnorm(8, 0, 3.316625)
# f)
set.seed(21)
B <- 10000
n <- 44
S <- replicate(B, {
  X <- sample(c(-0.25,1), n, replace = T, prob = c(inccorect,correct))
  sum(X)
})
S
mean(S>8)


# Q2.
# a)
correct <- 1/4
incorrect <- 1-correct
44*(1*correct+0*incorrect)
# b)
p <- seq(0.25, 0.95, 0.05)
score <- sapply(p, function(v){
  e_points <- (1*v) + (-0.25 * (1-v))
  m <- n * e_points
  se <- sqrt(n) * abs(-0.25-1) * sqrt(v*(1-v))
  1-pnorm(35, m, se)
})
min(p[which(score > 0.8)])


# Q3.
p_win = 5/38
p_lose = 1-p_win
n = 500
# a)
ap+b(1-p)
mu <- 6*(p_win)+(-1)*(p_lose)
mu
# b)
abs(b-a)*sqrt(p(1-p))
sig <- abs(-1-6)*sqrt(p_win*p_lose)
sig
# c)
mu
# d)
sig/sqrt(n)
# e)
n*mu
# f)
sqrt(n)*sig
# g)
pnorm(0, mu*n, sig*sqrt(n))




