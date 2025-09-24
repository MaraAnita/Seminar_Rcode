# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Comparison of Cross Validation and Bootstrap
# R-code
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------

# Empty the working dictionary
rm(list = ls())

# Load necessary packages
library("Metrics")
library("cv")
library("simstudy")
library("pingers")
library("bootstrap")

# -----------------------------------------------------------------------------
# Self-written Functions
# -----------------------------------------------------------------------------

# Function to calculate the classification error
class.err <- function(y, yhat) sum(y != yhat)


bootstrapPE <- function(data, B = 30, estimator = 3, linear = TRUE) {
  
  # This function estimates the prediction error of a
  #       - linear model using the mean squared error and for a
  #       - classification model using the total misclassification error.
  
  # The linear model is a least squares linear model and the classification 
  # model is a logistic regression.
  
  # A non-parametric bootstrap is used to obtain one of the following estimators:
  #              - First approach
  #              - Refined approach
  #              - The 0.632 bootstrap estimator
  
  # Parameters: 
  #   - data ........ the data in the specific form of a data frame, 
  #                   with "y" as the target variable
  #   - B ........... the number of bootstrap samples to be drawn, default = 30
  #   - estimator ... the estimator that will be used:
  #                        = 1 ... First approach
  #                        = 2 ... Refined approach
  #                        = 3 ... The 0.632 bootstrap estimator (default)
  #   - linear ...... if the model is a classification model or a 
  #                   linear model (default)
  
  # Output:
  #   - the bootstrap estimate of the prediction error
  
  
  ### Error-Messages:
  
  # if data is not a dataframe
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }
  
  # if B is not numeric or smaller or not positive
  if (!is.numeric(B) || B <=0) {
    stop("B must be numeric and > 0")
    }
  
  # if estimator is not 1, 2 or 3
  if (!any(estimator == c(1, 2, 3))) {
    stop("estimator must be numeric and 1, 2 or 3")
    }
  
  # if linear is not TRUE or FALSE
  if (!any(linear == c(TRUE, FALSE))) {
    stop("linear must be TRUE or FALSE")
    }
  
  ### Computations
  
  # size of original dataset
  n <- nrow(data)
  
  
  # To save the estimate of each sample
  est.pred.err <- numeric(B)
  
  # loop for each bootstrap sample
  for (i in 1:B) {
    
    # initiate a vector for the in-bootstrap-sample prediction error and 
    # the original-sample prediction prediction error
    BSsample <- numeric(0)
    originalsample <- numeric(0)
    notinsample <- numeric(0)
    

    # for linear regression
    if (linear) {
      
      # draw a Bootrap sample of the same size as the original dataset
      ind <- sample(1:n, size = n, replace = TRUE)
      set <- data[ind,] # with replacement
      
      # fit a liner model to the bootstrap sample
      lmod <- lm(y ~., data = set)


      # which estimator should be used?
      if (estimator == 1) {
        
        # original-sample prediction error
        est.pred.err[i] <- mse(predict.lm(lmod, data), data$y) 

      } else {
        
        if (estimator == 2) {
          ### The 2nd approach
          # original-sample prediction error
          originalsample <- mse(predict.lm(lmod, data), data$y)
          # compute the in-sample prediction error
          BSsample <- mse(predict.lm(lmod, set), set$y)
          
          # estimator
          est.pred.err[i] <- 2 * originalsample - BSsample
          
        } else {
          ### 0.632 estimator
          # data not contained in the bootstrap sample
          notset <- data[-unique(ind),]
          # not in bootstrap sample prediction error
          notinsample <- mse(predict.lm(lmod, notset), notset$y)
          # compute the in-sample prediction error
          BSsample <- mse(predict.lm(lmod, set), set$y)
          # estimator
          est.pred.err[i] <- 0.368 * BSsample + 0.632 * notinsample
          
        }
      }
      
      # for logistic regression
    } else {
      
      # draw balanced sample
      one <- simsala[simsala$y == 1, ]
      zero <- simsala[simsala$y == 0, ]
    
      ind <- as.numeric(c(sample(rownames(one), 
                                 size = floor(n/2), 
                                 replace = TRUE),
                          sample(rownames(one), 
                                 size = n - floor(n/2), 
                                 replace = TRUE)))
      
      set <- simsala[ind,]
      
      # logistic regression
      logit <- glm(y ~., family = "binomial", data = set)
      
      # which estimator should be used?
      if (estimator == 1) {
        # original sample error
        preds <- ifelse(predict(logit, newdata = data, type = "response") < 0.5, 
                        0, 1)
        ### The 1st approach
        est.pred.err[i] <- class.err(preds, data$y)
        
      } else {
        
        if (estimator == 2) {
          ### The 2nd approach
          
          # original sample error
          preds <- ifelse(predict(logit, newdata = data, 
                                  type = "response") < 0.5, 
                          0, 1)
          originalsample <- class.err(preds, data$y)
          
          # in-sample prediction error
          preds <- ifelse(predict(logit, newdata = set, 
                                  type = "response") < 0.5, 0, 1)
          BSsample <- class.err(preds, set$y)
          
          est.pred.err[i] <- 2 * originalsample - BSsample
          
        } else {
          ### 0.632 estimator
          # data not contained in the bootstrap sample
          notset <- data[-unique(ind),]
          # not in bootstrap sample prediction error
          preds <- ifelse(predict(logit, 
                                  newdata = notset, 
                                  type = "response") < 0.5, 0, 1)
          notinsample <- class.err(preds, notset$y)
        
          # in-sample prediction error
          preds <- ifelse(predict(logit, 
                                  newdata = set, 
                                  type = "response") < 0.5, 0, 1)
          BSsample <- class.err(preds, set$y)
          
          # estimator
          est.pred.err[i] <- 0.368 * BSsample + 0.632 * notinsample
        }
      }
    }
  }
  
  
  
  # Return the estimated prediction error
  return(mean(est.pred.err))
}


cvLogReg <- function(data, Folds = 10){
  
  # This function computes the prediction error a logistic regression model
  # using cross-validation
  
  # variables:
  #    - data ... the dataset
  #    - k ...... the number of folds
  
  # Output:
  #    - the total classification error
  
  
  # Part the data roughly into k !balanced! parts
  len <- nrow(simsala)
  
  # which group
  s <- rep(1:Folds, times = floor(len/Folds))
  # remaining datapoints t one group
  if (len%%Folds != 0) {
    s <- as.factor(c(s, 1:(len%%Folds)))
  }
  
  # to save each error
  err <- numeric(0)
  
  # for every fold
  for (i in 1:Folds){
    
    # split into train and test set
    train <- simsala[s != i,]
    test <- simsala[s == i,]
    
    # Fit logistic regression model on training data
    logit <- glm(y ~., family = "binomial", data = train)
    
    # Evaluate on the test data
    preds <- ifelse(predict(logit, 
                            newdata = test, 
                            type = "response") < 0.5, 0, 1)
    
    # save the classification error
    err <- c(err, class.err(preds, test$y))
    
  }
  
  # return the mean of the errors
  return(mean(err))
}



# -----------------------------------------------------------------------------
# Simulation of the data
# -----------------------------------------------------------------------------

trueBeta <- function(k, mean = 0, sd = 1) rnorm(k + 1, mean = mean, sd = sd)
# Function to simulate a normally distributed beta for a model with intercept


simulation <- function(k, n, beta, linear = TRUE) {
  
  # This function simulates the Data for a linear regression model or a binary 
  # classification model 
  
  # Parameters:
  #    k ... number of covariables
  #    n ... number of observations
  #    linear ... linear regression model or binary classification model
  #    beta ... the true beta
  
  # Output:
  # data set with columns y, x_1, ... x_k
  
  # Intercept and randomly distributed data matrix with mean 0 and sd 1
  X <- cbind(1, matrix(rnorm(n * k), nrow = n)) 
  
  if (linear) {
    # error term, randomly distributed with mean 0 and sd 1
    epsi <- rnorm(n, sd = 1)
    
  }  else {
    # higher variance to prevent perfect separation 
    epsi <- rnorm(n, sd = n/3)
    
  }
  
  # y = X * beta + eps
  y <- X %*% beta + epsi
  
  
  # for binary data
  if (!linear){
    probs <- 1 / (1 + exp(-y))
    y <- ifelse(probs <= 0.5, 0, 1)
  }
  
  # save as dataframe
  data <- data.frame(cbind(y, X[,-1])) # without intercept
  colnames(data) <- c("y", paste0("x", 1:k))
  
  # sort the data
  return(data[order(data$y),])
}


# -----------------------------------------------------------------------------
# Simulation study
# -----------------------------------------------------------------------------

# fix a seed
seed <- 1234

# sample size
# it valid to have a small sample size
n.size <- seq(30, 100, by = 1)
#n.size <- c(70, 80)

# number of explanatory variables
k <- 3

# Bootstrap and Cross Validation are fit repeatedly to to calculate 
# the mean and variance of the methods
howoften <- 30

# for the plots
lwd <- 1.8

###############################################################################
### Linear model
###############################################################################

{
  
# initiate vectors to save the values later
# actual prediction error
actual <- numeric(0)
m.actual <- numeric(0)
# mean
m.BE1 <- numeric(0)
m.BE2 <- numeric(0)
m.BE3 <- numeric(0)
m.CV10 <- numeric(0)
m.CVn <- numeric(0)
# Variance
v.BE1 <- numeric(0)
v.BE2 <- numeric(0)
v.BE3 <- numeric(0)
v.CV10 <- numeric(0)
v.CVn <- numeric(0)
# measure the time
time1 <- numeric(0)
time2 <- numeric(0)
time3 <- numeric(0)
time4 <- numeric(0)
time5 <- numeric(0)

# set the seed
set.seed(seed)

# simulate the beta
beta <- trueBeta(k)


# for different sample sizes
for (num in n.size) {
  
  # initiate vectors to save the values later
  BE1 <- numeric(0)
  BE2 <- numeric(0)
  BE3 <- numeric(0)
  CV10 <- numeric(0)
  CVn <- numeric(0)

  ### use every method to calculate the prediction error -howoften- times
  # always set the same seed before
  set.seed(seed)

  time1 <- c(time1, system.time(
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    # estimate the prediction error
    BE1 <- c(BE1, bootstrapPE(simsala, estimator = 1))
  }
  ) [1] / howoften) # find out how long -howoften- calculations of the 
                    # prediction error take and devide by howoften
  
  # always set the same seed before
  set.seed(seed)
  
  time2 <- c(time2, system.time(
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    BE2 <- c(BE2, bootstrapPE(simsala, estimator = 2))
  }
  ) [1] / howoften)
  
  
  # always set the same seed before
  set.seed(seed)
  
  time3 <- c(time3, system.time(
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    BE3 <- c(BE3, bootstrapPE(simsala, estimator = 3))
  }
  ) [1] / howoften)
  
  
  # always set the same seed before
  set.seed(seed)
  
  time4 <- c(time4, system.time(
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    lm1 <- lm(y ~., data = simsala)
    CV10 <- c(CV10, cv(lm1)$`CV crit`[[1]])  # cv(), 10-fold is default
  }
  ) [1] / howoften)
  
  # always set the same seed before
  set.seed(seed)
  
  time5 <- c(time5, system.time(
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    lm1 <- lm(y ~., data = simsala)
    CVn <- c(CVn, cv(lm1, k = "loo")$`CV crit`[[1]]) # "loo" for k = n
  }
  ) [1] / howoften)
  
  
  ### approximate the prediction error using more data
  simLarge <- simulation(num * 6, k = k, beta = beta)
  set.seed(seed)
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta)
    # first fit the linear model
    lm1 <- lm(y ~., data = simsala)
    # prediction error on a much larger sample
    actual <- c(actual, mse(predict.lm(lm1, simLarge), simLarge$y)) 
  }  
  
  ### mean and variance of each method
  m.BE1 <- c(m.BE1, mean(BE1))
  m.BE2 <- c(m.BE2, mean(BE2))
  m.BE3 <- c(m.BE3, mean(BE3))
  m.CV10 <- c(m.CV10, mean(CV10))
  m.CVn <- c(m.CVn, mean(CVn))
  m.actual <- c(m.actual, mean(actual))
  
  v.BE1 <- c(v.BE1, var(BE1))
  v.BE2 <- c(v.BE2, var(BE2))
  v.BE3 <- c(v.BE3, var(BE3))
  v.CV10 <- c(v.CV10, var(CV10))
  v.CVn <- c(v.CVn, var(CVn))
}

### plot the results

# delimiters of the plot windows
limits.m <- c(min(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual), 
              max(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual))
limits.v <- c(min(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn), 
              max(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn))
limits.time <- c(min(time1, time2, time3, time4, time5), 
                 max(time1, time2, time3, time4, time5))
# mean
plot(n.size, m.actual, type = "l", 
     main = "Linear Regression", 
     col = "black", 
     lwd = lwd, 
     ylim = limits.m,
     xlab = "sample size", ylab = "Mean of the prediction error")
lines(n.size, m.BE1, col = "lightgreen", lwd = lwd)
lines(n.size, m.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, m.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, m.CV10, col = "red", lwd = lwd)
lines(n.size, m.CVn, col = "darkred", lwd = lwd, lty = 2)

legend("topright", legend = c("additional test data", 
                              "first approach bootstrap estimator", 
                            "refined bootsrap estimator", 
                            "0.632 bootsrap estimator", 
                            "10-fold cross-validation", 
                            "n-fold cross-validation"),
       lty = c(1, 1, 2, 3, 1, 2), bty = "n", 
       col = c("black", "lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"), 
       lwd = lwd, 
       bg = "n")

# variance
plot(n.size, v.BE1, type = "l", 
     main = "Linear Regression", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.v,
     xlab = "sample size", ylab = "Variance of the prediction error")
lines(n.size, v.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, v.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, v.CV10, col = "red", lwd = lwd)
lines(n.size, v.CVn, col = "darkred", lwd = lwd, lty = 2)

legend("topright", legend = c("first approach bootstrap estimator", 
                              "refined bootsrap estimator", 
                              "0.632 bootsrap estimator", 
                              "10-fold cross-validation", 
                              "n-fold cross-validation"),
       lty = c(1, 2, 3, 1, 2), bty = "n", 
       col = c("lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"),
       lwd = lwd, 
       bg = "n")

# CPU time
plot(n.size, time1, type = "l", 
     main = "Linear Regression", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.time,
     xlab = "sample size", ylab = "CPU time of the methods in seconds")
lines(n.size, time2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, time3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, time4, col = "red", lwd = lwd)
lines(n.size, time5, col = "darkred", lwd = lwd, lty = 2)

legend("topleft", legend = c("first approach bootstrap estimator", 
                              "refined bootsrap estimator", 
                              "0.632 bootsrap estimator", 
                              "10-fold cross-validation", 
                              "n-fold cross-validation"),
       lty = c(1, 2, 3, 1, 2), 
       bty = "n", 
       col = c("lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"), 
       lwd = lwd, 
       bg = "n")
}

###############################################################################
### Classification model
###############################################################################

{
  
### initiate vectors to save the values later
# actual prediction error
actual <- numeric(0)
# mean
m.BE1 <- numeric(0)
m.BE2 <- numeric(0)
m.BE3 <- numeric(0)
m.CV10 <- numeric(0)
m.CVn <- numeric(0)
m.actual <- numeric(0)
# Variance
v.BE1 <- numeric(0)
v.BE2 <- numeric(0)
v.BE3 <- numeric(0)
v.CV10 <- numeric(0)
v.CVn <- numeric(0)
# measure the time
time1 <- numeric(0)
time2 <- numeric(0)
time3 <- numeric(0)
time4 <- numeric(0)
time5 <- numeric(0)

# set the seed
set.seed(seed)
# simulate the beta
beta <- trueBeta(k)
beta[1] <- 0.01


# for different sample sizes
for (num in n.size) {

  # initiate vectors to save the values later
  BE1 <- numeric(0)
  BE2 <- numeric(0)
  BE3 <- numeric(0)
  CV10 <- numeric(0)
  CVn <- numeric(0)
  
  ### use every method to calculate the prediction error -howoften- times
  time1 <- c(time1, system.time(
    for (o in 1:howoften) {
      # simulate the data
      simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
      BE1 <- c(BE1, bootstrapPE(simsala, estimator = 1, 
                                linear = FALSE))
    }
  ) [1] / howoften) # find out how long -howoften- calculations of the 
  # prediction error take and divide by howoften
  
  time2 <- c(time2, system.time(
    for (o in 1:howoften) {
      # simulate the data
      simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
      BE2 <- c(BE2, bootstrapPE(simsala, 
                                estimator = 2, 
                                linear = FALSE))
    }
  ) [1] / howoften)
  
  time3 <- c(time3, system.time(
    for (o in 1:howoften) {
      # simulate the data
      simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
      BE3 <- c(BE3, bootstrapPE(simsala, 
                                estimator = 3, 
                                linear = FALSE))
    }
  ) [1] / howoften)
  
  time4 <- c(time4, system.time(
    for (o in 1:howoften) {
      # simulate the data
      simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
      # logistic regression model
      CV10 <- c(CV10, cvLogReg(simsala, Folds = 10))
    
    }
  ) [1] / howoften)
  
  time5 <- c(time5, system.time(
    for (o in 1:howoften) {
      # simulate the data
      simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
      # logistic regression model
      CVn <- c(CVn, cvLogReg(simsala, nrow(simsala)))

    }
  ) [1] / howoften)
  
  ### approximate the prediction error using more data
  simLarge <- simulation(num * 6, k = k, beta = beta, linear = FALSE)
  set.seed(seed)
  
  for (o in 1:howoften) {
    # simulate the data
    simsala <- simulation(num, k = k, beta = beta, linear = FALSE)
    # logistic regression model
    logit <- glm(y ~., family = "binomial", data = simsala)
    
    # in-sample prediction error
    preds <- ifelse(predict(logit, newdata = simLarge, 
                            type = "response") < 0.5, 0, 1)
    # prediction error on a much larger sample
    actual <- c(actual, class.err(preds, simLarge$y) / 6) # divide by 6 to 
                                                          # scale correctly 
  } 
  
  ### mean and variance of each method
  m.BE1 <- c(m.BE1, mean(BE1))
  m.BE2 <- c(m.BE2, mean(BE2))
  m.BE3 <- c(m.BE3, mean(BE3))
  m.CV10 <- c(m.CV10, mean(CV10))
  m.CVn <- c(m.CVn, mean(CVn))
  m.actual <- c(m.actual, mean(actual))
  v.BE1 <- c(v.BE1, var(BE1))
  v.BE2 <- c(v.BE2, var(BE2))
  v.BE3 <- c(v.BE3, var(BE3))
  v.CV10 <- c(v.CV10, var(CV10))
  v.CVn <- c(v.CVn, var(CVn))
}



### plot the results

# delimiters of the plot windows
limits.m <- c(min(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual), 
              max(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual))
limits.v <- c(min(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn), 
              max(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn))
limits.time <- c(min(time1, time2, time3, time4, time5), 
                 max(time1, time2, time3, time4, time5))
# mean
plot(n.size, m.actual, type = "l", 
     main = "Logistic Regression", 
     col = "black", 
     lwd = lwd, 
     ylim = limits.m,
     xlab = "sample size", 
     ylab = "Mean of the predicted total classification error")
lines(n.size, m.BE1, col = "lightgreen", lwd = lwd)
lines(n.size, m.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, m.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, m.CV10, col = "red", lwd = lwd)
lines(n.size, m.CVn, col = "darkred", lwd = lwd, lty = 2)

legend("topleft", legend = c("additional test data", 
                              "first approach bootstrap estimator", 
                              "refined bootsrap estimator", 
                              "0.632 bootsrap estimator", 
                              "10-fold cross-validation", 
                              "n-fold cross-validation"),
       lty = c(1, 1, 2, 3, 1, 2), bty = "n", 
       col = c("black", "lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"), 
       lwd = lwd, 
       bg = "n")

# variance
plot(n.size, v.BE1, type = "l", 
     main = "Logistic Regression", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.v,
     xlab = "sample size", 
     ylab = "Variance of the predicted total classification error")
lines(n.size, v.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, v.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, v.CV10, col = "red", lwd = lwd)
lines(n.size, v.CVn, col = "darkred", lwd = lwd, lty = 2)

legend("topleft", legend = c("first approach bootstrap estimator", 
                              "refined bootsrap estimator", 
                              "0.632 bootsrap estimator", 
                              "10-fold cross-validation", 
                              "n-fold cross-validation"),
       lty = c(1, 2, 3, 1, 2), bty = "n", 
       col = c("lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"),
       lwd = lwd, 
       bg = "n")

# CPU time
plot(n.size, time1, type = "l", 
     main = "Logistic Regression", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.time,
     xlab = "sample size", ylab = "CPU time of the methods in seconds")
lines(n.size, time2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, time3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, time4, col = "red", lwd = lwd)
lines(n.size, time5, col = "darkred", lwd = lwd, lty = 2)

legend("topleft", legend = c("first approach bootstrap estimator", 
                              "refined bootsrap estimator", 
                              "0.632 bootsrap estimator", 
                              "10-fold cross-validation", 
                              "n-fold cross-validation"),
       lty = c(1, 2, 3, 1, 2), 
       bty = "n", 
       col = c("lightgreen", "seagreen", 
               "darkgreen", "red", "darkred"), 
       lwd = lwd, 
       bg = "n")
}





# ------------------------------------------------------------------------------
# Grafik for 0.632 estimator
# ------------------------------------------------------------------------------

# create function 
pe <- function(N) 1-((N-1)/N)^N

# set a sequence of sample sizes
rng <- seq(1, 1000)
# and create vector to save values later
save <- rng
  
# compute the value for every sample size
for (n in rng) {
  save[n] <- pe(n)
}

# plot the results
plot(rng[-1], save[-1], pch = 20, cex = 0.5, 
     main = "", 
     ylab = "1-((N-1)/N)^N", xlab = "N")
abline(h = 1-exp(-1), col = "blue")



