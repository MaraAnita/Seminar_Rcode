install.packages("simstudy")
library(simstudy)


simulation <- function(k, n, linear = TRUE) {
  
  # This function simulates the Data for a linear regression model or a binary 
  # classification model 
  
  # Parameters:
      # k ... number of covariables
      # n ... number of observations
      # linear ... linear regression model or binary classification model
  
  # Output:
      # data set with columns y, x_1, ... x_k
  
  
  
  ### simulate X and beta step by step
  # X_1
  i <- 1
  Xname <- paste0("x", i)
  def <- defData(varname = Xname, 
                 dist = "normal", 
                 formula = 0, 
                 variance = 1) 
  # beta_1
  beta <- rnorm(n = 1, mean = 5, sd = 20)
  # update the formula
  formula <- paste("1 +", beta, "*", Xname)
  
  
  for (i in 2:k) {
    # name of the coulum
    Xname <- paste0("x", i)
    
    # X_i
    def <- defData(def, 
                   varname = Xname, 
                   dist = "normal", 
                   formula = 0, 
                   variance = 1) 
    # beta_i
    beta <- rnorm(n = 1, mean = 5, sd = 20)
    
    # update the formula
    formula <- paste(formula, "+", beta, "*", Xname)
    
  }
  
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
                   variance = 1,
                   link = "logit")
  }
  
  data <- genData(n, def)   # generate the data
  data$id <- NULL           # delete id colum
  return(data)              # return the dataset
}


