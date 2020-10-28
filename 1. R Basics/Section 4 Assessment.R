library(dslabs)
data(heights)


a <- heights$sex
m <- 1:length(heights$sex)
z <- ifelse(a == "Female",1,2)
sum(z)


b <- heights$height
y <- ifelse(b > 72, heights$height,0)
mean(y)


inches_to_ft <- function(x){
  ft <- x/12
}
x <- inches_to_ft(144)
w <- inches_to_ft(heights$height)
v <- ifelse(w < 5, 1,0)
sum(v)


# define a vector of length m
m <- 10
f_n <- vector(length = m)
# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}
# inspect f_n
f_n







