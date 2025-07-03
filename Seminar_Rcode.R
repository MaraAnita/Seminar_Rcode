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
library("caret")

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
  
    
  # initiate a vector for the in-bootstrap-sample prediction error and 
  # the original-sample prediction prediction error
  BSsample <- numeric(0)
  originalsample <- numeric(0)
  
  # loop for each bootstrap sample
  for (i in 1:B) {
    
    # draw a Bootrap sample of the same size as the original dataset
    set <- data[sample(1:n, size = n, 
                        replace = TRUE),] # with replacement
    
    if (linear) {
      
      # fit a liner model to the bootstrap sample
      lmod <- lm(y ~., data = set)
      
      
      # compute the in-sample prediction error
      BSsample <- c(BSsample, 
                    mse(predict.lm(lmod, set), set$y))
      # original-sample prediction error
      originalsample <- c(originalsample, 
                          mse(predict.lm(lmod, data), data$y)) 
      
    } else {
      
      # logistic regression
      logit <- glm(y ~., family = "binomial", data = set)
      
      # in-sample prediction
      preds <- ifelse(predict(logit, newdata = set, type = "response") < 0.5, 
                      0, 1)
      BSsample <- c(BSsample, 
                    class.err(preds, set$y))
      
      # original sample error
      preds <- ifelse(predict(logit, newdata = data, type = "response") < 0.5, 
                      0, 1)
      originalsample <- c(originalsample, 
                          class.err(preds, data$y))
      
      
    }
  }
  
  # which estimator shold be used?
  
  if (estimator == 1) {
    
    # The 1st approach
    est.pred.err <- mean(originalsample)
  
  } else {
    
    if (estimator == 2) {
      
      # The 2nd approach
      est.pred.err <- mean(originalsample) + mean(originalsample - BSsample)
      
    } else {
      
      # 0.632 estimator
      est.pred.err <- mean(originalsample) + 
        0.632 * mean(originalsample - BSsample)
      
    }
  }
  

  # Return the estimated prediction error
  return(est.pred.err)
}




# -----------------------------------------------------------------------------
# Simulation of the data
# -----------------------------------------------------------------------------

# Function to simulate a beta
trueBeta <- function(k, mean = 0, sd = 1) rnorm(k + 1, mean = mean, sd = sd)


simulation <- function(k, n, beta, linear = TRUE) {
  
  # This function simulates the Data for a linear regression model or a binary 
  # classification model 
  
  # Parameters:
  #    k ... number of covariables
  #    n ... number of observations
  #    linear ... linear regression model or binary classification model
  #    beta ... the true beta
  
  # Output:
  # data set with columns y, x_1, ... x_k, pred
  #  pred ... the true prediction X*beta
  
  
  ### simulate X and beta step by step
  
  # X_1
  i <- 1
  Xname <- paste0("x", i)
  def <- defData(varname = Xname, 
                 dist = "normal", 
                 formula = 0, 
                 variance = 1) 

  # if k > 1 generate more columns
  if (k > 1) {
    for (i in 2:k) {
      # name of the coulum
      Xname <- c(Xname, paste0("x", i))
      
      # X_i
      def <- defData(def, 
                     varname = Xname[i], 
                     dist = "normal", 
                     formula = 0, 
                     variance = 1) 

    }
  }
  
  # create the formula
  formula <- paste(beta, "*", c("1", Xname), collapse = " + ")
  
  if (linear) {
    
    # generate Y according to the formula
    def <- defData(def, 
                   varname = "y", 
                   dist = "normal", 
                   formula = formula, 
                   variance = 1)
  } else {
    
    # generate a binary Y according to the formula
    def <- defData(def, 
                   varname = "y", 
                   dist = "binary",
                   formula = formula, 
                   variance = 100000,
                   link = "logit")
  }
  
  data <- genData(n, def)   # generate the data
  
  # calculate the actual prediction error
  X <- data   # safe data separately
  X$id <- 1   # create intercept
  X$y <- NULL # delete the y 
  
  # calculate the true prediction
  pred <- as.matrix(X) %*% beta
  
  # in a classification model, the target variable Y is binary
  if (!linear) {
    # use the logit link function
    pred <- round(1 / (1 + exp(-pred))) # round due to numerical error
  }
  
  data$pred <- pred         # save the predicted value in the data frame
  data$id <- NULL           # delete id column
  return(data)              # return the data set
}



beta <- trueBeta(3)
sim <- simulation(3, 30, beta)

mse(sim$pred, sim$y)

# -----------------------------------------------------------------------------
# Simulation study
# -----------------------------------------------------------------------------

# fix a seed
seed <- 1234

# sample size
# it valid to have a small sample size
n.size <- seq(20, 100, by = 1)
#n.size <- c(20, 40)

# number of covaraites
k <- 3

# Bootstrap and Cross Validation are fit repeatedly to to calculate 
# the mean and variance of the methods
howoften <- 30

# for the plots
lwd <- 1

###############################################################################
### Linear model
###############################################################################

{
  
# initiate vectors to save the values later
# actual prediction error
actual <- numeric(0)
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
# simulate the data
simsi <- simulation(max(n.size), k = k, beta = beta)

# for different sample sizes
for (num in n.size) {
  # for every sample size enlarge the data set 
  # simsala <- list(simsi[[1]][1:num,], simsi[[2]][1:num])
  simsala <- simsi[1:num,]
  pred <- simsala$pred  # safe the actual prediction seperately
  simsala$pred <- NULL  # delete from the dataframe
  
  # initiate vectors to save the values later
  BE1 <- numeric(0)
  BE2 <- numeric(0)
  BE3 <- numeric(0)
  CV10 <- numeric(0)
  CVn <- numeric(0)

  ### use every method to calculate the prediction error -howoften- times
  time1 <- c(time1, system.time(
  for (o in 1:howoften) {
    BE1 <- c(BE1, bootstrapPE(simsala, estimator = 1))
  }
  ) [1] / howoften) # find out how long -howoften- calculations of the 
                    # prediction error take and devide by howoften
  
  time2 <- c(time2, system.time(
  for (o in 1:howoften) {
    BE2 <- c(BE2, bootstrapPE(simsala, estimator = 2))
  }
  ) [1] / howoften)
  
  time3 <- c(time3, system.time(
  for (o in 1:howoften) {
    BE3 <- c(BE3, bootstrapPE(simsala, estimator = 3))
  }
  ) [1] / howoften)
  
  time4 <- c(time4, system.time(
  for (o in 1:howoften) {
    
    lm1 <- lm(y ~., data = simsala)
    CV10 <- c(CV10, cv(lm1)$`CV crit`[[1]])  # cv(), 10-fold is default
  }
  ) [1] / howoften)
  
  time5 <- c(time5, system.time(
  for (o in 1:howoften) {
    
    lm1 <- lm(y ~., data = simsala)
    CVn <- c(CVn, cv(lm1, k = "loo")$`CV crit`[[1]]) # "loo" for k = n
  }
  ) [1] / howoften)
  
  
  ### approximate the prediction error using more data
  # first fit the linear model
  lm1 <- lm(y ~., data = simsala)
  # prediction error on a much larger sample
  simLarge <- simulation(num * 6, k = k, beta = beta)
  actual <- c(actual, mse(predict.lm(lm1, simLarge), simLarge$y)) 
  
  
  
  
  ### mean and variance of each method
  m.BE1 <- c(m.BE1, mean(BE1))
  m.BE2 <- c(m.BE2, mean(BE2))
  m.BE3 <- c(m.BE3, mean(BE3))
  m.CV10 <- c(m.CV10, mean(CV10))
  m.CVn <- c(m.CVn, mean(CVn))
  #m.actual <- c(m.actual, mean(actual))
  
  v.BE1 <- c(v.BE1, var(BE1))
  v.BE2 <- c(v.BE2, var(BE2))
  v.BE3 <- c(v.BE3, var(BE3))
  v.CV10 <- c(v.CV10, var(CV10))
  v.CVn <- c(v.CVn, var(CVn))
}



### plot the results

# delimiters of the plot windows
limits.m <- c(min(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, actual), 
              max(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, actual))
limits.v <- c(min(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn), 
            max(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn))
limits.time <- c(min(time1, time2, time3, time4, time5), 
              max(time1, time2, time3, time4, time5))
# 
lwd <- 2

# mean
plot(n.size, actual, type = "l", 
     main = "", 
     col = "black", 
     lwd = lwd, 
     ylim = limits.m,
     xlab = "sample size", ylab = "Mean of the prediction error")
lines(n.size, m.BE1, col = "lightgreen", lwd = lwd)
lines(n.size, m.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, m.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, m.CV10, col = "red", lwd = lwd)
lines(n.size, m.CVn, col = "darkred", lwd = lwd, lty = 2)

# varianve
plot(n.size, v.BE1, type = "n", 
     main = "", 
     col = "black", 
     lwd = lwd, 
     ylim = limits.v,
     xlab = "sample size", ylab = "Variance of the prediction error")
lines(n.size, v.BE1, col = "lightgreen", lwd = lwd)
lines(n.size, v.BE2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, v.BE3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, v.CV10, col = "red", lwd = lwd)
lines(n.size, v.CVn, col = "darkred", lwd = lwd, lty = 2)


# CPU time
plot(n.size, time1, type = "n", 
     main = "", 
     col = "black", 
     lwd = lwd, 
     ylim = limits.time,
     xlab = "sample size", ylab = "CPU time of the methods in seconds")
lines(n.size, time1, col = "lightgreen", lwd = lwd)
lines(n.size, time2, col = "seagreen", lwd = lwd, lty = 2)
lines(n.size, time3, col = "darkgreen", lwd = lwd, lty = 3)
lines(n.size, time4, col = "red", lwd = lwd)
lines(n.size, time5, col = "darkred", lwd = lwd, lty = 2)

}





###############################################################################
### Classification model
###############################################################################


{
  
  # initiate vectors to save the values later
  # actual prediction error
  actual <- numeric(0)
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
  # simulate the data
  simsi <- simulation(max(n.size), k = k, beta = beta, linear = FALSE)
  
  # for different sample sizes
  for (num in n.size) {
    # for every sample size enlarge the data set 
    # simsala <- list(simsi[[1]][1:num,], simsi[[2]][1:num])
    simsala <- simsi[1:num,]
    pred <- simsala$pred  # safe the actual prediction seperately
    simsala$pred <- NULL  # delete from the dataframe
    
    # initiate vectors to save the values later
    BE1 <- numeric(0)
    BE2 <- numeric(0)
    BE3 <- numeric(0)
    CV10 <- numeric(0)
    CVn <- numeric(0)
    
    ### use every method to calculate the prediction error -howoften- times
    time1 <- c(time1, system.time(
      for (o in 1:howoften) {
        BE1 <- c(BE1, bootstrapPE(simsala, estimator = 1, 
                                  linear = FALSE))
      }
    ) [1] / howoften) # find out how long -howoften- calculations of the 
    # prediction error take and devide by howoften
    
    time2 <- c(time2, system.time(
      for (o in 1:howoften) {
        BE2 <- c(BE2, bootstrapPE(simsala, estimator = 2, 
                                  linear = FALSE))
      }
    ) [1] / howoften)
    
    time3 <- c(time3, system.time(
      for (o in 1:howoften) {
        BE3 <- c(BE3, bootstrapPE(simsala, estimator = 3, 
                                  linear = FALSE))
      }
    ) [1] / howoften)
    
    time4 <- c(time4, system.time(
      for (o in 1:howoften) {
        
        # logistic regression model
        logit <- glm(y ~., family = "binomial", data = simsala)
        CV10 <- c(CV10, cv(logit, criterion = class.err)$`CV crit`[[1]])  # cv(), 10-fold is default
      }
    ) [1] / howoften)
    
    time5 <- c(time5, system.time(
      for (o in 1:howoften) {
        # logistic regression model
        logit <- glm(y ~., family = "binomial", data = simsala)
        CVn <- c(CVn, cv(logit, criterion = class.err, k = "loo")$`CV crit`[[1]]) # "loo" for k = n
      }
    ) [1] / howoften)
    
    
    ### approximate the prediction error using more data
    # first fit the linear model
    lm1 <- lm(y ~., data = simsala)
    # prediction error on a much larger sample
    simLarge <- simulation(num * 6, k = k, beta = beta)
    actual <- c(actual, class.err(predict.lm(lm1, simLarge), simLarge$y)/6) 
    
    ### mean and variance of each method
    m.BE1 <- c(m.BE1, mean(BE1))
    m.BE2 <- c(m.BE2, mean(BE2))
    m.BE3 <- c(m.BE3, mean(BE3))
    m.CV10 <- c(m.CV10, mean(CV10))
    m.CVn <- c(m.CVn, mean(CVn))
    #m.actual <- c(m.actual, mean(actual))
    
    v.BE1 <- c(v.BE1, var(BE1))
    v.BE2 <- c(v.BE2, var(BE2))
    v.BE3 <- c(v.BE3, var(BE3))
    v.CV10 <- c(v.CV10, var(CV10))
    v.CVn <- c(v.CVn, var(CVn))
  }
  
  
  
  ### plot the results
  
  # delimiters of the plot windows
  limits.m <- c(min(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, actual), 
                max(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, actual))
  limits.v <- c(min(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn), 
                max(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn))
  limits.time <- c(min(time1, time2, time3, time4, time5), 
                   max(time1, time2, time3, time4, time5))
  
  # mean
  plot(n.size, actual, type = "l", 
       main = "", 
       col = "black", 
       lwd = lwd, 
       ylim = limits.m,
       xlab = "sample size", ylab = "Mean of the total classification error")
  lines(n.size, m.BE1, col = "lightgreen", lwd = lwd)
  lines(n.size, m.BE2, col = "seagreen", lwd = lwd, lty = 2)
  lines(n.size, m.BE3, col = "darkgreen", lwd = lwd, lty = 3)
  lines(n.size, m.CV10, col = "red", lwd = lwd)
  lines(n.size, m.CVn, col = "darkred", lwd = lwd, lty = 2)
  
  # variance
  plot(n.size, v.BE1, type = "n", 
       main = "", 
       lwd = lwd, 
       ylim = limits.v,
       xlab = "sample size", ylab = "Variance of the total classification error")
  lines(n.size, v.BE1, col = "lightgreen", lwd = lwd)
  lines(n.size, v.BE2, col = "seagreen", lwd = lwd, lty = 2)
  lines(n.size, v.BE3, col = "darkgreen", lwd = lwd, lty = 3)
  lines(n.size, v.CV10, col = "red", lwd = lwd)
  lines(n.size, v.CVn, col = "darkred", lwd = lwd, lty = 2)
  
  
  # CPU time
  plot(n.size, time1, type = "n", 
       main = "", 
       lwd = lwd, 
       ylim = limits.time,
       xlab = "sample size", ylab = "CPU time of the methods in seconds")
  lines(n.size, time1, col = "lightgreen", lwd = lwd)
  lines(n.size, time2, col = "seagreen", lwd = lwd, lty = 2)
  lines(n.size, time3, col = "darkgreen", lwd = lwd, lty = 3)
  lines(n.size, time4, col = "red", lwd = lwd)
  lines(n.size, time5, col = "darkred", lwd = lwd, lty = 2)
  
}




