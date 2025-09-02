# simulate the data
num <- 90
simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        

Folds <- 9

# Part the data roughly into k parts
len <- nrow(simsala)

s <- rep(1:Folds, each = floor(len/Folds))

if (len%%Folds != 0) {
  s <- as.factor(c(s, 1:(len%%Folds)))
}

# to save each error
err <- numeric(0)

# for every fold
for (i in 1:Folds){
  
  # split into train and test set
  train <- simsala[!s == i,]
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

mean(err)
