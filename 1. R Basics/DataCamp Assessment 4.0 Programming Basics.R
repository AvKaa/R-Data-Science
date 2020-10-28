sum_n <- function(x){
    v_n <- 1:x
    sum(v_n)
}
x <- sum_n(5)
x

altman_plot <- function(x,y){
  y_axis = y-x
  x_axis = x+y
  plot(x_axis,y_axis)
}

z <- altman_plot(6,9)
z

compute_s_n <- function(x){
  v_n = 1:x
  sum(v_n^2)
}

a <- compute_s_n(10)
a

# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Create a vector for storing results
s_n <- vector("numeric", 25)

# write a for-loop to store the results in s_n
m <- 25
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}







