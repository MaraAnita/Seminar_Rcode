# simulate the data
simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        

Folds <- 9

# Part the data roughly into k parts
len <- nrow(simsala)
s <- as.factor(c(rep(1:Folds, each = floor(len/Folds)), 
                 1:(len%%Folds)))


# split into train and test set
train <- simsala[!s == 1,]
test <- simsala[s == 1,]

# Fit logistic regression model on training data
logit <- glm(y ~., family = "binomial", data = train)

# Evaluate on the remaining data
predict.glm(logit, data = test)

?predict

# for every part
for (i in 1:Folds){
  
  # split into train and test set
  train <- simsala[!s == i,]
  test <- simsala[s == i,]
  
  # Fit logistic regression model on training data
  logit <- glm(y ~., family = "binomial", data = train)
  
  # Evaluate on the remaining data
  
  
  
}





cvLogReg <- function(data, Folds = 10){
  
  # This function computes the prediction error a logistic regression model
  # using cross-validation
  
  # variables:
  #    - data ... the dataset
  #    - k ...... the number of folds
  
  # Output:
  #    - the prediction error
  
  
  # Part the data roughly into k parts
  len <- nrow(simsala)
  s <- as.factor(c(rep(1:Folds, each = floor(len/Folds)), 
                   1:(len%%Folds)))
  
  # for every part
  for (i in 1:Folds){
    
    # split into train and test set
    train <- simsala[!s == i,]
    test <- simsala[s == i,]
    
    # Fit logistic regression model on training data
    logit <- glm(y ~., family = "binomial", data = data)
    
    # Evaluate on the test data
    
    
    
  }
  
  

  
  
  
  
  
  
  return()
}