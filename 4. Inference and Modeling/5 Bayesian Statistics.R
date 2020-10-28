# Bayesian Statistics
"
In the urn model, it does not make sense to talk about the probability of  p  being greater than a certain value because  p  is a fixed value.
With Bayesian statistics, we assume that  p  is in fact random, which allows us to calculate probabilities related to  p .
Hierarchical models describe variability at different levels and incorporate all these levels into a model for estimating  p .
"
#===============================================================================
# Bayes' Theorem
"
Bayes' Theorem states that the probability of event A happening given event B is equal to the probability of both A and B divided by the probability of event B:
Pr(AB)=(Pr(BlA)*Pr(A))/Pr(B) 
Bayes' Theorem shows that a test for a very rare disease will have a high percentage of false positives even if the accuracy of the test is high.

Equations: Cystic fibrosis test probabilities
In these probabilities, + represents a positive test, - represents a negative test,  D=0  indicates no disease, and  D=1  indicates the disease is present.

Probability of having the disease given a positive test:  Pr(D=1l+) 
99% test accuracy when disease is present:  Pr(+lD=1)=0.99 
99% test accuracy when disease is absent:  Pr(-lD=0)=0.99 
Rate of cystic fibrosis:  Pr(D=1)=0.00025 
Bayes' theorem can be applied like this:

Pr(D=1l+)=(Pr(+lD=1)*Pr(D=1))/Pr(+) 
Pr(D=1l+)=(Pr(+lD=1)*Pr(D=1))/(Pr(+lD=1)*Pr(D=1)+Pr(+lD=0)*Pr(D=0) 
Substituting known values, we obtain:

(0.99*0.00025)/(0.99*0.00025+0.01*0.99975)=0.02
"
#-------------------------------------------------------------------------------
# Monte Carlo simulation
prev <- 0.00025    # disease prevalence
N <- 100000    # number of tests
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")    # number with disease
N_H <- sum(outcome == "Healthy")    # number healthy

# for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))

table(outcome, test)

#-------------------------------------------------------------------------------
# Bayes in Practice
"
The techniques we have used up until now are referred to as frequentist statistics as they consider only the frequency of outcomes in a dataset and do not include any outside information. Frequentist statistics allow us to compute confidence intervals and p-values.
Frequentist statistics can have problems when sample sizes are small and when the data are extreme compared to historical results.
Bayesian statistics allows prior knowledge to modify observed results, which alters our conclusions about event probabilities.
"

Pr(alb)= (Pr(bla)*Pr(a))/Pr(b)



















