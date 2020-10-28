"
https://www.guru99.com/r-apply-sapply-tapply.html

apply() :Takes Data frame or matrix as an input and gives output in vector, list or array. 
         apply() Function is primarily used to avoid explicit uses of loop constructs. 
         It is the most basic of all collections can be used over a matrice.

This function takes 3 arguments:

apply(X, MARGIN, FUN)
Here:
-x: an array or matrix
-MARGIN:  take a value or range between 1 and 2 to define where to apply the function:
-MARGIN=1`: the manipulation is performed on rows
-MARGIN=2`: the manipulation is performed on columns
-MARGIN=c(1,2)` the manipulation is performed on rows and columns
-FUN: tells which function to apply. Built functions like mean, median, sum, min, max and even user-defined functions can be applied>

Example:
"
m1 <- matrix(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum) # column operation
a_m1


"
lapply(): Is useful for performing operations on list objects and returns a list object of same length of original set. 
          lappy() returns a list of the similar length as input list object, each element of which is the result of applying FUN to the corresponding element of list. 
          lapply() takes list, vector or data frame as input and gives output in list.

lapply(X, FUN)
Arguments:
-X: A vector or an object
-FUN: Function applied to each element of x

l in lapply() stands for list. The difference between lapply() and apply() lies between the output return. 
The output of lapply() is a list. lapply() can be used for other objects like data frames and lists.

lapply() function does not need MARGIN.

Example:
"
movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <-lapply(movies, tolower)
str(movies_lower)

# We can use unlist() to convert the list into a vector.

movies_lower <-unlist(lapply(movies,tolower))
str(movies_lower)


"
sapply(): This function takes list, vector or data frame as input and gives output in vector or matrix. 
          It is useful for operations on list objects and returns a list object of same length of original set. 
          sapply() function does the same job as lapply() function but returns a vector.

sapply(X, FUN)
Arguments:
-X: A vector or an object
-FUN: Function applied to each element of x

Example: measuring the minimum speed and stopping distances of cars from the cars dataset.
"
dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)
lmn_cars
smn_cars

"
Slice Vector: We can use lapply() or sapply() interchangeable to slice a data frame. 
              We create a function, below_average(), that takes a vector of numerical values and returns a vector that only contains the values that are strictly above the average. 
              We compare both results with the identical() function.

Example:
"
below_ave <- function(x) {  
    ave <- mean(x) 
    return(x[x > ave])
}
dt_s<- sapply(dt, below_ave)
dt_l<- lapply(dt, below_ave)
identical(dt_s, dt_l)


"
tapply(): computes a measure (mean, median, min, max, etc..) or a function for each factor variable in a vector. 
          It is a very useful function that lets you create a subset of a vector and then apply some functions to each of the subset.

tapply(X, INDEX, FUN = NULL)
Arguments:
-X: An object, usually a vector
-INDEX: A list containing factor
-FUN: Function applied to each element of x

To understand how it works, let's use the iris dataset. This dataset is very famous in the world of machine learning. 
The purpose of this dataset is to predict the class of each of the three flower species: Sepal, Versicolor, Virginica. 
The dataset collects information for each species about their length and width.

As a prior work, we can compute the median of the length for each species. 
tapply() is a quick way to perform this computation.

Example:
"
data(iris)
tapply(iris$Sepal.Width, iris$Species, median)

















