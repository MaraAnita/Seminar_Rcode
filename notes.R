
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
  data$id <- NULL           # delete id column
  return(data)              # return the data set
}

