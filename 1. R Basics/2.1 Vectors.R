# We may create vectors of class numeric or character with the concatenate function
codes <- c(380, 124, 818)
country <- c("italy", "canada", "egypt")

# We can also name the elements of a numeric vector
# Note that the two lines of code below have the same result (with and without quotes)
codes <- c(italy = 380, canada = 124, egypt = 818)
codes <- c("italy" = 380, "canada" = 124, "egypt" = 818)

class(codes) # Shows that the object codes continues to be a numeric vector

# We can also name the elements of a numeric vector using the names() function
codes <- c(380, 124, 818)
country <- c("italy","canada","egypt")
names(codes) <- country

seq(1,10) # List numbers from 1-10 can also be written as 1:10 
seq(1,10,2) # Sequence from 1-10 in steps of 2
1:10:5 # Is not the same as seq(1,10,5) this uses first 5 elements of the sequence 

# Subsetting
# Using square brackets is useful for subsetting to access specific elements of a vector
codes[2] # Same results with codes["canada"]
codes[c(1,3)] # Same results with codes[c("egypt","italy")]
codes[1:2]

# If the entries of a vector are named, they may be accessed by referring to their name
codes["canada"]
codes[c("egypt","italy")]

#Vector Coercion
x <- 1:5
y <- as.character(x)
y
z <- as.numeric(x)
z

# If R fails Coercion it returns NA






