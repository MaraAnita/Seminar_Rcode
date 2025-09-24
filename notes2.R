
?set.seed

num <- 30
howoften <- 1

k <- 3

# simulate the beta
beta <- trueBeta(k)
beta[1] <- 0.01

70*101+sum(30:100)

simsala <- simulation(num, k = k, beta = beta, linear = FALSE)

# logistic regression model
logit <- glm(y ~., family = "binomial", data = simsala)
bootstrapPE(simsala, estimator = 1, linear = FALSE)

#y <- rbinom(n, size = 1, prob = probs)



2100*6









data <- simulation(k, n, trueBeta(k), linear = FALSE)



data[order(data$y),]


beta[1] <- 0.1
simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
BE1 <- c(BE1, bootstrapPE(simsala, estimator = 1, 
                          linear = FALSE))


simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
# logistic regression model
cvLogReg(simsala, nrow(simsala))

Folds <- 10

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
  i<-1
  # split into train and test set
  train <- simsala[s != i,]
  test <- simsala[s == i,]
  
  # Fit logistic regression model on training data
  logit <- glm(y ~., family = "binomial", data = train)
  summary(logit)
  # Evaluate on the test data
  preds <- ifelse(predict(logit, 
                          newdata = test, 
                          type = "response") < 0.5, 0, 1)
  
  # save the classification error
  err <- c(err, class.err(preds, test$y))
  
}

# return the mean of the errors
return(mean(err))



###########

data <- simsala


logit <- glm(y ~., family = "binomial", data = simsala)





sum(data$y)
n - sum(data$y)




logit <- glm(y ~., family = "binomial", data = data)

num <- 30

simsala <- simulation(num, k = k, beta = beta, linear = FALSE)        
bootstrapPE(simsala, estimator = 1, linear = FALSE)

# draw a Bootrap sample of the same size as the original dataset
ind <- sample(1:n, size = n, replace = TRUE)

# draw balanced sample???
one <- simsala[simsala$y == 1, ]
zero <- simsala[simsala$y == 0, ]




logit <- glm(y ~., family = "binomial", data = set)

#######################################

install.packages("GuessCompx")

library(GuessCompx)








########################################
install.packages("cyclocomp")

library(cyclocomp)


?cyclocomp

a <- c(1,2,3)

# nicht so wichtige Funktionen
cyclocomp(simulation)

# Funktionen als ganzes
cyclocomp(bootstrapPE)
cyclocomp(cv)
cyclocomp(cvLogReg)


# herausgeschriebene Funktionen einzeln
cyclocomp(unique.array)
cyclocomp(mean)
cyclocomp(class.err)
cyclocomp(predict.glm)
cyclocomp(predict.lm)
cyclocomp(lm)
cyclocomp(glm)
cyclocomp(sample)
cyclocomp(subset.data.frame)


### Tryout
cyclocomp(levels)
cyclocomp(tapply)
cyclocomp(Sys.time)


#####################################
# microbenchmark

install.packages("microbenchmark")

library(microbenchmark)

?microbenchmark
microbenchmark(sample(1:8))

##########################################


?complexity

install.packages("complexity")

library(complexity)

complexity(4,1,2,2,3,3,4)




##########################################
# Source code der unique Funktion

getS3method("unique", "array")

getS3method("cv", "lm")

getS3method("sample", "lm")


?sample
?lm.fit

sample
sample.int
