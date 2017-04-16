#Load packages
library(ggplot2)
library(leaps)
library(glmnet)
library(boot)


#Load csv into a dataframe
filename_train <- "data.csv"
filename_test <- "data_test.csv"
dataset_train <- read.csv(filename_train, header = TRUE)
dataset_test <- read.csv(filename_test, header = TRUE)


#Scatter plot of X vs Y
ggplot(data = dataset_train, mapping = aes(x = x, y = y)) + geom_point(size = 1) + geom_smooth()


#Set seed value
set.seed(123457)


#Adding independent variables
dataset_train$X2 <- dataset_train$x ^ 2
dataset_train$X3 <- dataset_train$x ^ 3
dataset_train$X4 <- dataset_train$x ^ 4


#Compute LOOCV
attach(dataset_train)
fit1 <- glm(y ~ x, data = dataset_train, family = "gaussian")
fit2 <- glm(y ~ x + X2, data = dataset_train, family = "gaussian")
fit3 <- glm(y ~ x + X2 + X3, data = dataset_train, family = "gaussian")
fit4 <- glm(y ~ x + X2 + X3 + X4, data = dataset_train, family = "gaussian")

err1 <- cv.glm(dataset_train, fit1)$delta[2]
err2 <- cv.glm(dataset_train, fit2)$delta[2]
err3 <- cv.glm(dataset_train, fit3)$delta[2]
err4 <- cv.glm(dataset_train, fit4)$delta[2]
errors <- c(err1, err2, err3, err4)
model_names <- c('Model-1', 'Model-2', 'Model-3', 'Model-4')
errors <- rbind(model_names, errors)
errors


#Set another random seed
set.seed(123579)


#Recompute LOOCV
attach(dataset_train)
fit1 <- glm(y ~ x, data = dataset_train, family = "gaussian")
fit2 <- glm(y ~ x + X2, data = dataset_train, family = "gaussian")
fit3 <- glm(y ~ x + X2 + X3, data = dataset_train, family = "gaussian")
fit4 <- glm(y ~ x + X2 + X3 + X4, data = dataset_train, family = "gaussian")

err1 <- cv.glm(dataset_train, fit1)$delta[2]
err2 <- cv.glm(dataset_train, fit2)$delta[2]
err3 <- cv.glm(dataset_train, fit3)$delta[2]
err4 <- cv.glm(dataset_train, fit4)$delta[2]
errors <- c(err1, err2, err3, err4)
model_names <- c('Model-1', 'Model-2', 'Model-3', 'Model-4')
errors <- rbind(model_names, errors)
errors


#Adding independent variables from X^5 to X^10
dataset_train$X5 <- dataset_train$x ^ 5
dataset_train$X6 <- dataset_train$x ^ 6
dataset_train$X7 <- dataset_train$x ^ 7
dataset_train$X8 <- dataset_train$x ^ 8
dataset_train$X9 <- dataset_train$x ^ 9
dataset_train$X10 <- dataset_train$x ^ 10


#Performing best subset selection
regfit.full <- regsubsets(y~. - X, data = dataset_train, nvmax = 10)
regfit.summary <- summary(regfit.full)

#For Cp
which.min(regfit.summary$cp)
plot(regfit.summary$cp, xlab = "No. of variables", ylab = "Cp", type = 'l')
points(which.min(regfit.summary$cp), regfit.summary$cp[which.min(regfit.summary$cp)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "Cp")
coef(regfit.full, which.min(regfit.summary$cp))

#For BIC
which.min(regfit.summary$bic)
plot(regfit.summary$bic, xlab = "No. of variables", ylab = "BIC", type = 'l')
points(which.min(regfit.summary$bic), regfit.summary$bic[which.min(regfit.summary$bic)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "bic")
coef(regfit.full, which.min(regfit.summary$bic))

#For adjusted R-square
which.max(regfit.summary$adjr2)
plot(regfit.summary$adjr2, xlab = "No. of variables", ylab = "Adjusted R-square", type = 'l')
points(which.max(regfit.summary$adjr2), regfit.summary$adjr2[which.max(regfit.summary$adjr2)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "adjr2")
coef(regfit.full, which.max(regfit.summary$adjr2))


#Forward vs backward
regfit.fwd <- regsubsets(y~. - X, data = dataset_train, nvmax = 10, method = "forward")
regfit.summary <- summary(regfit.fwd)

#For Cp
which.min(regfit.summary$cp)
plot(regfit.summary$cp, xlab = "No. of variables", ylab = "Cp", type = 'l')
points(which.min(regfit.summary$cp), regfit.summary$cp[which.min(regfit.summary$cp)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "Cp")
coef(regfit.full, which.min(regfit.summary$cp))

#For BIC
which.min(regfit.summary$bic)
plot(regfit.summary$bic, xlab = "No. of variables", ylab = "BIC", type = 'l')
points(which.min(regfit.summary$bic), regfit.summary$bic[which.min(regfit.summary$bic)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "bic")
coef(regfit.full, which.min(regfit.summary$bic))

#For adjusted R-square
which.max(regfit.summary$adjr2)
plot(regfit.summary$adjr2, xlab = "No. of variables", ylab = "Adjusted R-square", type = 'l')
points(which.max(regfit.summary$adjr2), regfit.summary$adjr2[which.max(regfit.summary$adjr2)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "adjr2")
coef(regfit.full, which.max(regfit.summary$adjr2))


#Forward vs backward
regfit.fwd <- regsubsets(y~. - X, data = dataset_train, nvmax = 10, method = "backward")
regfit.summary <- summary(regfit.fwd)

#For Cp
which.min(regfit.summary$cp)
plot(regfit.summary$cp, xlab = "No. of variables", ylab = "Cp", type = 'l')
points(which.min(regfit.summary$cp), regfit.summary$cp[which.min(regfit.summary$cp)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "Cp")
coef(regfit.full, which.min(regfit.summary$cp))

#For BIC
which.min(regfit.summary$bic)
plot(regfit.summary$bic, xlab = "No. of variables", ylab = "BIC", type = 'l')
points(which.min(regfit.summary$bic), regfit.summary$bic[which.min(regfit.summary$bic)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "bic")
coef(regfit.full, which.min(regfit.summary$bic))

#For adjusted R-square
which.max(regfit.summary$adjr2)
plot(regfit.summary$adjr2, xlab = "No. of variables", ylab = "Adjusted R-square", type = 'l')
points(which.max(regfit.summary$adjr2), regfit.summary$adjr2[which.max(regfit.summary$adjr2)], 
       col = "red", cex = 2, pch = 20)
plot(regfit.full, scale = "adjr2")
coef(regfit.full, which.max(regfit.summary$adjr2))


#Lasso model
x1 <- model.matrix(y~. - X, data = dataset_train)[,-1]
y1 <- dataset_train$y
grid <- 10^seq(10, -2, length = 100)

set.seed(123457)
lasso_model = glmnet(x1, y1, alpha = 1, lambda = grid)
plot(lasso_model)

cv_errs = cv.glmnet(x1, y1, alpha = 1)
plot(cv_errs)

lamda_best = cv_errs$lambda.min
lamda_best

out = glmnet(x1, y1, alpha = 1, lambda = grid)
lasso_coef = predict(out, type = "coefficients", s = lamda_best)
lasso_coef

lasso_pred = predict(lasso_model, s = lamda_best, newx = model.matrix(y~. - X, data = dataset_test)[,-1])
mean((lasso_pred - dataset_test$y)^2)


fit3_pred <- predict.glm(fit3, data = dataset_test, type = "response")
mean((fit3_pred[1] - dataset_test$y)^2)

pred1 <- predict.glm(regfit.full[3], data = dataset_test, type = "response")
mean((pred1[1] - dataset_test$y)^2)


coef(regfit.full, which.min(regfit.summary$cp))

