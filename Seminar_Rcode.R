# -----------------------------------------------------------------------------
# Comparison of Cross Validation and Bootstrap
# R-code
# -----------------------------------------------------------------------------

# Empty the working dictionary
rm(list = ls())

# Load necessary packages
library("Metrics")
library("cv")


###############################################################################
### Self-written Functions
###############################################################################

simulation <- function(Laenge, k, 
                            X.means = c(1, 1, 1, 1, 1, 1, 1), 
                            X.sd = c(1, 1, 1, 1, 1, 1, 1), 
                            eps.sd = 1, 
                            beta.mean = 0, 
                            beta.sd = 1,
                            linear = TRUE) {
  
  # This function simulates the Data for a linear or classification model 
  
  # Parameters:
  #     howoften .... how many datasets should be generated?
  #     Laenge ...... number of observations in the dataset
  #     k ........... number of covariates
  #     X.means ..... mean of the covariates
  #     X.sd ........ standard deviation of the covariates
  #     eps.sd ...... mean of the error term
  #     beta.mean ... mean of the betas
  #     beta.sd ..... standard deviation of the betas
  #     linear ...... classification model or linear model (default)

  # Output: list containing:
  #     [1] ... dataframe with simulated X and y
  #     [2] ... the actual error term
  

  X <- numeric(0)                  # X initialisieren
  for (i in 1:k) {                 # Schleife ueber alle Spalten
    X <- c(X, rnorm(n = Laenge,    # mittels normalverteilung
                    X.means[i], 
                    X.sd[i]))
  }
  
  data <- matrix(X, 
                 nrow = Laenge,
                 byrow = FALSE)             # Spaltenweise befuellen
  
  
  # simulate the error term
  eps <- rnorm(Laenge, 0, 0.75)
  
  # simulate beta
  beta <- rnorm(n = k+1, 
                mean = beta.mean, 
                sd = beta.sd)
  
  # simulate Y
  data <- as.data.frame(cbind(
    cbind(rep(1, times = Laenge), data) %*% beta + eps, data))

  
  colnames(data) <- c("y", paste0("x", seq(1:k)))   # Spalten benennen
  
  
  if (!linear) {
  
    # If y is bigger than the mean of y: 1, else 0
    data$y <- as.numeric(mean(data$y) > data$y)
    
  }
  
  return(list(data, eps))
}



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
                    sum(preds == set$y))
      
      # original sample error
      preds <- ifelse(predict(logit, newdata = data, type = "response") < 0.5, 
                      0, 1)
      originalsample <- c(originalsample, 
                          sum(preds == data$y))
      
      
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




###############################################################################
### Simulation
###############################################################################
seed <- 123

# sample size
n.size <- seq(20, 30)
# number of covaraites
k <- 5


# how many replications on the same dataset?
howoften <- 30




m.BE1 <- numeric(0)
m.BE2 <- numeric(0)
m.BE3 <- numeric(0)
m.CV10 <- numeric(0)
m.CVn <- numeric(0)
m.actual <- numeric(0)

v.BE1 <- numeric(0)
v.BE2 <- numeric(0)
v.BE3 <- numeric(0)
v.CV10 <- numeric(0)
v.CVn <- numeric(0)
v.actual <- numeric(0)


set.seed(seed)
simsi <- simulation(max(n.size), k = k)

for (num in n.size) {
  
  # have nested models
  simsala <- list(simsi[[1]][1:num,], simsi[[2]][1:num])
  
  # apply bootstrap and CV to each simulation
  BE1 <- numeric(0)
  BE2 <- numeric(0)
  BE3 <- numeric(0)
  actual <- numeric(0)
  CV10 <- numeric(0)
  CVn <- numeric(0)
  
  
  for (o in 1:howoften) {
    BE1 <- c(BE1, bootstrapPE(simsala[[1]], estimator = 1))
  }
  
  
  for (o in 1:howoften) {
    BE2 <- c(BE2, bootstrapPE(simsala[[1]], estimator = 2))
  }
  
  
  for (o in 1:howoften) {
    BE3 <- c(BE3, bootstrapPE(simsala[[1]], estimator = 3))
  }
  
  
  
  for (o in 1:howoften) {
    actual <- c(actual, mean(simsala[[2]]^2))
  }
  
  
  for (o in 1:howoften) {
    
    lm1 <- lm(y ~., data = simsala[[1]])
    
    CV10 <- c(CV10, cv(lm1)$`CV crit`[[1]])  
  }
  
  
  for (o in 1:howoften) {
    
    lm1 <- lm(y ~., data = simsala[[1]])
    
    CVn <- c(CVn, cv(lm1, k = "loo")$`CV crit`[[1]])
  }
  
  
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
  v.actual <- c(v.actual, var(actual))


}




# plot
nnn <- n.size[1:length(m.BE1)]

limits.m <- c(min(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual), 
              max(m.BE1, m.BE2, m.BE3, m.CV10, m.CVn, m.actual))
limits.v <- c(min(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn, v.actual), 
            max(v.BE1, v.BE2, v.BE3, v.CV10, v.CVn, v.actual))


lwd <- 2

plot(nnn, m.BE1, type = "l", 
     main = "", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.m,
     xlab = "sample size", ylab = "Mean of the prediction error")
lines(nnn, m.BE2, col = "seagreen", lwd = lwd)
lines(nnn, m.BE3, col = "darkgreen", lwd = lwd)
lines(nnn, m.CV10, col = "firebrick", lwd = lwd)
lines(nnn, m.CVn, col = "darkred", lwd = lwd)
lines(nnn, m.actual, col = "black", lwd = lwd)


plot(nnn, v.BE1, type = "l", 
     main = "", 
     col = "lightgreen", 
     lwd = lwd, 
     ylim = limits.v,
     xlab = "sample size", ylab = "Variance of the prediction error")
lines(nnn, v.BE2, col = "seagreen", lwd = lwd)
lines(nnn, v.BE3, col = "darkgreen", lwd = lwd)
lines(nnn, v.CV10, col = "firebrick", lwd = lwd)
lines(nnn, v.CVn, col = "darkred", lwd = lwd)
lines(nnn, v.actual, col = "black", lwd = lwd)







################################################################################
k <- 5

# seed to have a coherent result
set.seed(123)

actual <- numeric(0)

for (num in n.size) {
  
  simsi <- simulation(num, k)
  actual <- c(actual, mean(simsi[[2]]^2))

  
}


set.seed(123)

BE1 <- numeric(0)
BE2 <- numeric(0)
BE3 <- numeric(0)

for (num in n.size) {
  
  simsi <- simulation(num, k)
  
  BE1 <- c(BE1, bootstrapPE(simsi[[1]], estimator = 1))
  BE2 <- c(BE2, bootstrapPE(simsi[[1]], estimator = 2))
  BE3 <- c(BE3, bootstrapPE(simsi[[1]], estimator = 3))
  
}



set.seed(123)

CV10 <- numeric(0)
CVn <- numeric(0)


for (n in n.size) {
  
  simsi <- simulation(n, k, linear = TRUE)

  lm1 <- lm(y ~., data = simsi[[1]])
  
  CV10 <- c(CV10, cv(lm1)$`CV crit`[[1]])
  CVn <- c(CVn, cv(lm1, k = "loo")$`CV crit`[[1]])
  
}


# Difference to the actual value
actual - BE1


plot(n.size, BE1 - actual, type = "l", 
     main = "Simulation", 
     col = "seagreen", 
     ylim = c(min(c(BE1, BE2, BE3, CV10, CVn) - actual), 
              max(c(BE1, BE2, BE3, CV10, CVn) - actual)))
lines(n.size, BE2 - actual, col = "firebrick")
lines(n.size, BE3 - actual, col = "darkblue")
lines(n.size, CV10 - actual, col = "pink")
lines(n.size, CVn - actual, col = "lightblue")
abline(h = 0)


mean(BE1 - actual)
mean(BE2 - actual)
mean(BE3 - actual)
mean(CV10 - actual)
mean(CVn - actual)


var(BE1 - actual)
var(BE2 - actual)
var(BE3 - actual)
var(CV10 - actual)
var(CVn - actual)





### classification
for (n in n.size) {
  
  simsi <- simulation(n, k, linear = FALSE)
  
  BE1 <- c(BE1, bootstrapPE(simsi[[1]], estimator = 1, linear = FALSE))
  BE2 <- c(BE2, bootstrapPE(simsi[[1]], estimator = 2, linear = FALSE))
  BE3 <- c(BE3, bootstrapPE(simsi[[1]], estimator = 3, linear = FALSE))
  
}

plot(n.size, BE1, type = "l", 
     main = "Simulation", col = "seagreen")
lines(n.size, BE2, col = "firebrick")
lines(n.size, BE3, col = "darkblue")



simsi <- simulation(20,7)


bootstrapPE(simsi[[1]], estimator = 1)
bootstrapPE(simsi[[1]], estimator = 2)
bootstrapPE(simsi[[1]], estimator = 3)





# Can I somehow compute the actual prediction error??




###########
### Cross-Validation
##########



# first fit a linear model
lm1 <- lm(y ~., data = simsi)

# Function uses MSE per default

# 10 fold
cross.valid <- cv(lm1)
summary(cross.valid)
cross.valid$`CV crit`[[1]]

plot(cross.valid)
plot(cross.valid, what = "coefficients")

# LOOCV
cross.valid <- cv(lm1, k = "loo")
summary(cross.valid)

# noch schauen, was das macht:
# ev. gegen Bias
cross.valid <- cv(lm1, method = "Woodbury")
summary(cross.valid)
cross.valid <- cv(lm1, method = "naive")
summary(cross.valid)


# mehere Egebnisse speichern, später schauen, wie das geht
as.data.frame(cross.valid)




