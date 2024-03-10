library(e1071)

# Data Production for Linearly Inseperable Case
x <- matrix(rnorm(20 * 2), ncol = 2)
  # produce a 20x2 matrix of random values (splits the 40 values into 20 in each col)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
  # values in x in the same row corresponding to the position of values in y 
  # equal to 1 are increased by 1 (i.e the last 10 rows)
plot(x, col = (3 - y))
  # col gives us 10 values with colour = 4 and 10 with colour = 2

dat <- data.frame(x = x, y = factor(y))
  # encodes response var as factor for SVM



# Fitting and Plotting the SVM
svmfit <- svm(y ~ x.1 + x.2, data = dat, kernel = 'linear', cost = 0.1, scale = F)
  # fits a SVM to the data (be careful, x is not a variable, x.1 and x.2 are)

plot(svmfit, dat)



# Cross Validation for the Cost Parameter
tune.out <- tune(svm, # fct to tune via CV
                 y ~ x.1 + x.2, # formula to pass into svm()
                 data = dat, # data to use
                 kernel = 'linear', # param for svm()
                 scale = F, # param for svm()
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100))) 
                                            # CV values to test
  # tells us that cost = 5 is our best option

best.svm <- tune.out$best.model

plot(best.svm, dat)



# Using SVM for Predictions
xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

svm.pred <- predict(best.svm, newdata = testdat)

svm.valid <- table(svm.pred == ytest)
  # 8 incorrect predictions, 12 correct predictions, error rate of 40%

svm.lesspred <- predict(svmfit, newdata = testdat)
table(svm.lesspred == ytest)
  # 9 false, 11 true, so cost = 5 is better as expected (from CV)



# Linearly Separable Case
x[y == 1, ] <- x[y == 1, ] + 0.5
  # shifts all the y=1s in the previous data up a bit more so that it becomes lin sep
plot(x, col = (y + 5) / 2, pch = 19)
dat <- data.frame(x = x, y = as.factor(y))

lin.tune <- tune(svm, y~., data = dat, kernel = 'linear', scale = F,
                 range = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lin.best.svm <- lin.tune$best.model
  # has 9 support vectors for linearly separable data rather than 11 for lin insep

lin.pred <- predict(lin.best.svm, newdata = testdat)
table(lin.pred == ytest)
  # same as before

plot(lin.best.svm, dat)



# SVM for non-linear Kernels
N = 1000
xx = rnorm(N)
yy = 4 * xx^2 + 1 + rnorm(N)
class = sample(N, N/2)
yy[class] = yy[class] + 6
yy[-class] = yy[-class] - 6
x <- cbind(xx,yy)
plot(x[class,1], x[class,2], col = "red", xlab = "X", ylab = "Y", ylim = c(-6, 30)) +
  points(x[-class,1], x[-class,2], col = "blue")

y = rep(-1, N)
y[class] = 1

data = data.frame(x = x, y = as.factor(y))

train = sample(N, N/2)
data_train = data[train, ]
data_eval = data[-train, ]
  # splitting data, half for training and half for evaluation

best.poly.svm <- tune(svm, y~., data = data, kernel = 'polynomial', 
                      range = list(degree = c(1, 2,3), 
                      gamma = c(0.5, 1, 2, 0),
                      coef0 = c(0, 1, 2),
                      cost = c(0.01, 0.05, 0.1)))

best.poly.model <- best.poly.svm$best.model

plot(best.poly.model, data)
