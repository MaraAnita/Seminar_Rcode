
install.packages("pingers")


citation("caret")

trainControl()
train()
resamples()


{
sim_data <- twoClassSim(n = 20, linearVars = 0)
#names(sim_data)[names(sim_data)=="Class"] <- "y"
logit <- glm(Class ~., family = "binomial", data = sim_data)
}

?twoClassSim


plot(sim_data)

sim_data2 <- twoClassSim(n = 100, linearVars = 2)

shuffle(sim_data)

?twoClassSim

head(sim_data2)

LPH07_1(100, class = TRUE)

SLC14_1(100, linearVars = 3)


?metrics
LPH07_2

caret:::twoClassSim

twoClassSim


ls("package:caret", pattern = "Sim")



library("bootstrap")
?bootpred







###########

sim <- simulation(4, 30, linear = FALSE)
sim
class.err(sim$pred, sim$y)


#### CV auf total classification error ändern!!!

simsi <- simulation(3, 2000000, linear = FALSE)
simsala <- simsi
simsala$pred <- NULL

install.packages("brglm2")
library("brglm2")


logit <- glm(y ~., family = "binomial", data = simsala, , method = "brglmFit")
CV10 <- c(CV10, cv(logit)$`CV crit`[[1]])  # cv(), 10-fold is default


cv <- cv(logit, criterion = class.err)
cv$details
cv

?cv()


class.err(1, 0)

###############

simulation <- function(Laenge, k, 
                       X.means = rep(1, length = k), 
                       X.sd =  rep(1, length = k), 
                       eps.sd = 80, 
                       beta.mean = 0, 
                       beta.sd = 3,
                       linear = TRUE) {
  
  # This function simulates the Data for a linear or classification model 
  
  # Parameters:
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
  eps <- rnorm(Laenge, 1, 0.75)
  
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





######


paste(c("h", "i"),"*", c("a", "b"), collapse = " + ")#
print(c("h", "i"))
??join()

?cat()


?predict()



data <- simulation(3, 20)


library(simstudy)

# Step 1: Define covariates
def <- defData(varname = "X1", dist = "normal", formula = 0, variance = 1)
def <- defData(def, varname = "X2", dist = "normal", formula = 0, variance = 1)

# Step 2: Define outcome with known coefficients and variance (error)
#def <- defData(def, varname = "Y", dist = "normal", 
#               formula = "2 + 1.5 * X1 - 2 * X2", variance = 1)

# Step 3: Generate data
set.seed(123)
data <- genData(10, def)
data$id <- 1

beta <- c(2, 1.5, -2)
as.matrix(data) %*% beta


data[, Y_hat_true := 2 + 1.5 * X1 - 2 * X2]  # true prediction








true_mse <- mean((data$Y_hat_true - data$Y)^2)
print(paste("True prediction error (MSE):", round(true_mse, 4)))







