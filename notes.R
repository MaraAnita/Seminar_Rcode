
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

# ------------------------------------------------------------------------------
# Grafik for Computational complexity
# ------------------------------------------------------------------------------

# change plot parameters
par(mfrow = c(2,2))



# Linear regression
formel1a <- function(B = 30, k = 10, n = 30, p = 3) {
  if (p > n) {
    returni <- B*p*n^2
  } else {
    returni <- B*n^3
  }
  return(returni)
}

formel2a <- function(B = 30, k = 10, n = 30, p = 3) {
  if (p > n) {
    returni <- B*p*n^2
  } else {
    returni <- B*n^3
  }
  return(returni)
}

formel3a <- function(B = 30, k = 10, n = 30, p = 3) {
  if (p > n) {
    returni <- B*p*n^2
  } else {
    returni <- B*n^3
  }
  return(returni)
}

formel4a <- function(B = 30, k = 10, n = 30, p = 3) {
  if (p > n) {
    returni <- k*n^2*p
  } else {
    returni <- k*n^3
  }
  return(returni)
}

formel5a <- function(B = 30, k = n, n = 30, p = 3) {
  if (p > n) {
    returni <- p*n^2
  } else {
    returni <- n^3
  }
  return(returni)
}

# Logistic regression
formel1b <- function(B = 30, k = 10, n = 30, p = 3, m=1) B*m*n*p

formel2b <- function(B = 30, k = 10, n = 30, p = 3, m=1) B*m*n*p


formel3b <- function(B = 30, k = 10, n = 30, p = 3, m=1) {
  if ((p*m) > n) {
    returni <- B*m*n*p
  } else {
    returni <- B*n^2
  }
  return(returni)
}

formel4b <- function(B = 30, k = 10, n = 30, p = 3, m=1) k * m * n * p

formel5b <- function(B = 30, k = n, n = 30, p = 3, m=1) m*n^2*p

# die grenzen der bereiche festlegen
range1 <- 1:50
range2 <- 1:30
range3 <- 30:100
range4 <- 3:20


# initiate vectors to save values
return1a <- 1:50
return2a <- 1:30
return3a <- 30:100
return4a <- 3:20
return1b <- 1:50
return2b <- 1:30
return3b <- 30:100
return4b <- 3:20

# 1
for (i in range1){
  
  return1a[i] <- formel1a(B = i)
  return2a[i] <- formel2a(B = i)
  return3a[i] <- formel3a(B = i)
  return4a[i] <- formel4a(B = i)
  #return5a[i] <- formel5a(B = i)
  
  return1b[i] <- formel1b(B = i)
  return2b[i] <- formel2b(B = i)
  return3b[i] <- formel3b(B = i)
  return4b[i] <- formel4b(B = i)
  #return5b[i] <- formel5b(B = i)
  
  
}

plot(range1, return1a)
lines(range1, return2a)
lines(range1, return3a)
lines(range1, return4a)




# plotten!

# 2
for (i in range2){
  
  return1a[i] <- formel1a(k = i)
  return2a[i] <- formel2a(k = i)
  return3a[i] <- formel3a(k = i)
  return4a[i] <- formel4a(k = i)
  return5a[i] <- formel5a(k = i)
  
  return1b[i] <- formel1b(k = i)
  return2b[i] <- formel2b(k = i)
  return3b[i] <- formel3b(k = i)
  return4b[i] <- formel4b(k = i)
  return5b[i] <- formel5b(k = i)
  
}

# plotten!


# 3
for (i in range3){
  
  return1a[i] <- formel1a(n = i)
  return2a[i] <- formel2a(n = i)
  return3a[i] <- formel3a(n = i)
  return4a[i] <- formel4a(n = i)
  return5a[i] <- formel5a(n = i)
  
  return1b[i] <- formel1b(n = i)
  return2b[i] <- formel2b(n = i)
  return3b[i] <- formel3b(n = i)
  return4b[i] <- formel4b(n = i)
  return5b[i] <- formel5b(n = i)
  
  
}

# plotten!


# 4
for (i in range4){
  
  return1a[i] <- formel1a(p = i)
  return2a[i] <- formel2a(p = i)
  return3a[i] <- formel3a(p = i)
  return4a[i] <- formel4a(p = i)
  return5a[i] <- formel5a(p = i)
  
  return1b[i] <- formel1b(p = i)
  return2b[i] <- formel2b(p = i)
  return3b[i] <- formel3b(p = i)
  return4b[i] <- formel4b(p = i)
  return5b[i] <- formel5b(p = i)
  
}

# plotten!

