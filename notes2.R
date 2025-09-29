
### perfect separation of the data?


# set the seed
set.seed(seed)
# simulate the beta
beta <- trueBeta(k)
beta[1] <- 0.01


# as the problem was partly fixed, it was difficult to provoke a case with a 
# not converging algorithm. That is why the sample size is chosen to be so low
# Actually as cross-validation and bootstrap often do not use all the data, these
# warning messages ar much more likely.

num <- 11
num <- 30


set.seed(1234)

# simulate the data
simsala <- simulation(num, k = k, beta = beta, linear = FALSE)
# logistic regression model
logit <- glm(y ~., family = "binomial", data = simsala)


summary(logit)


install.packages("detectseparation")
library("detectseparation")


glm(y ~ ., data = simsala,
    family = binomial("logit"),
    method = "detect_separation")
