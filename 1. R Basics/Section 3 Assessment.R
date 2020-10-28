library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers

heights
mean_heights = mean(heights$height)
stringsAsFactors = FALSE

ind1 <- heights$height >= mean_heights

ind2 <- heights$sex == "Female"

ind <- ind1 & ind2
sum(ind)

mean(ind2)

min(heights$height)
which.min(heights$height)

heights$sex[1032]

max(heights$height)

x <- 50:82
y <- !x %in% heights$height
sum(y)

z <- mean(heights$height)
z*2.54

females <- heights$sex == "Female"
heights2 <- filter(heights, females)
heights2

count(heights2)

mean(heights2$height)*2.54

data(olive)
head(olive)

plot(olive$palmitic, olive$palmitoleic)

hist(olive$eicosenoic)

boxplot(palmitic~region, data = olive)

