library(gtools)

set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9, 5.7)


# Q1.
# a)
avg <- mean(act_scores)
avg
# b)
s <- sd(act_scores)
s
# c)
sum(act_scores >= 36)
# d)
sum(act_scores > 30)/length(act_scores)
# e)
sum(act_scores <= 10)/length(act_scores)

# Q2.
x <- seq(1:36)
f_x <- dnorm(x, 20.9, 5.7)
plot(x, f_x)

# Q3.
# a)
z_scores <- (act_scores - avg)/s
sum(z_scores >= 2)/length(z_scores)
# b)
avg+2*s
# c)
qnorm(0.975, avg, s)

# Q4.
# a)
ceiling(qnorm(0.95, avg, s))
# b)
qnorm(0.95, 20.9, 5.7)
# c)
sample_qantiles <- seq(0.01, 0.99, 0.01)
quantile(act_scores, sample_qantiles)
# d)
theoretical_quantiles <- qnorm(sample_qantiles, 20.9, 5.7)
theoretical_quantiles
qqplot(x = theoretical_quantiles, y =  sample_qantiles)









