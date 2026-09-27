library('faraway')
library('glmnet')
library('pls')
library('leaps')

data('seatpos')

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Shows Pearson cc values in assigned panel 

pairs(seatpos, upper.panel = panel.cor) # all possible plots of seatpos variables against each other



# Coeficient Shrinkage Methods

y <- seatpos$hipcenter # Use this as our response variable
x <- model.matrix(hipcenter ~ ., seatpos)[,-1] # [,-1] removes intercept and 'assign'
  # model matrix is the matrix of all observed values of all predictor variables
  # i.e the X in y = [X]b with y vector of estimated predictor vals, b vector of beta_hats in MLR

ridge <- glmnet(x, y, alpha = 0, nlambda = 200) 
  # Ridge (alpha=0) regression for 200 different values of lambda
  # %dev returned is % of SD explained by the model with coefs for corresponding lambda value 

plot(ridge, xvar = 'lambda') # plot of coefs values against lambda values from ridge regression

lasso <- glmnet(x, y, alpha = 1, nambda = 200) # Same as ridge reg, but for lasso
  # looks to minimise the l1-norm of coef vector here rather than l2-norm in ridge

plot(lasso, xvar = 'lambda') # regularisation paths of regression coefs against log lambda
plot(lasso) # regularisation paths of regression coefs against l1 norm



# Principle Component Analysis

s <- apply(x, 2, sd)  # calculates the column standard deviations
x.s  <- sweep(x, 2, s, "/")  
  # divides all columns by their standard deviations so all data has SD = 1
  # useful since we're using PCA to find how much variation each PC contributes
  # so all columns having the same variation makes it easier to compare

x.pr <- prcomp(x.s) # Finds PCs of x.s
# could have used scale = TRUE instead of the above two lines (s and x.s) 
# but we use them in data reconstruction

pc1var <- x.pr$sdev[1]
pc1evect <- unname(x.pr$rotation[,1])

plot(x.pr) # scree plot showing how much each principle component contributes to total variance of X

sum(x.pr$sdev^2) # = 8, so at least 95% of variance (= 7.6) is captured through first 4 PCs



# Data Compression and Reconstruction/Dimension Reduction

# x.pr$x # is the same as:
# t(x.pr$rot[,]) %*% (t(fuel)-m)
xT <- t(x.pr$x[,c(1,2,3,4)])  # for compression from p=8 dimensional to d=4 dimensional data

ms <- colMeans(x.s) 

R <- t(ms + x.pr$rot[,c(1,2,3,4)]%*% xT) # reconstruction 

comp.data <- rbind(x.s[,1:2], R[,1:2]) #(only used to compare reconstructed data to original)

plot(x.s[,1:2],
     xlim = c(min(comp.data[,1]), max(comp.data[,1])),
     ylim = c(min(comp.data[,2]), max(comp.data[,2])))
plot(R[,1:2], col=3,
     xlim = c(min(comp.data[,1]), max(comp.data[,1])),
     ylim = c(min(comp.data[,2]), max(comp.data[,2])))
plot(comp.data, col=c(rep(1,38),rep(3,38)))
# Visually demonstrates differences between original and reconstructed data 
# flick between successive plots to see movement

pcr.fit <- pcr(hipcenter ~ ., data = seatpos, scale = TRUE, validation = 'CV')
  # fits a PC regression model via Cross Validation, scales variables so SD=1 
  # for each segment in CV

validationplot(pcr.fit, val.type = 'MSEP') 
  # Mean Square Error of Prediction plot for pcr.fit model

min.pcr <- which.min(MSEP(pcr.fit)$val[1,1,]) - 1 
  # suggests PC regression model with 2 PCs is best (-1 to ignore intercept)
  # to minimize magnitude of error of predictions from actual values (MSEP).

  # This gives a different value to no. of predictors suggested when trying to
  # capture 95% of Variance.

coef(pcr.fit, ncomp = 2) 
  # suggested coefs using PC regression for 2 PCs
  # This model will capture the amount of variance described by the first 2 PCs (~86%)



# Predictive Performance Measurements and Cross Validation

cp.bestsub <- leaps(x, y, method = 'Cp')
  # best-subset selection of predictors for y: finds model with minimum RSS 
  # value of all pCk models with exactly k predictors - so end up with p potential
  # models - then chooses the model with lowest Cp of these p potential models

corr.ridge <- c()
corr.lasso <- c()
  # we will plot these later

for(i in 1:50){ 
  # we perform 50 repetitions of this in order to plot correlation of predicted
  # results to actual values

# first we need to split our data into training and testing data

trains <- sample(1:nrow(x), nrow(x)-10)
  # gives us the 28 random positions to sample from x for CV training 
  # and 10 samples for testing

x.train <- x[trains,] # takes values from x determined by 'trains'
x.test <- x[-trains,] # takes the leftovers for testing

y.train <- y[trains]
y.test <- y[-trains]

ridge.cv <- cv.glmnet(x.train, y.train, names = names(x), nfolds = 5, alpha = 0)
  # performs 5-fold CV producing a ridge regression model

rmin.lam <- ridge.cv$lambda.min
  # the value of lambda, whose corresponding model found when minimising the 
  # ridge cost function, has the lowest MSE

rcv.coefs <- coef(ridge.cv)
  # the coefficients for the MLR model chosen by CV ridge regression
  # suggests that Ht and Leg have the most correlation with hipcentre

lasso.cv <- cv.glmnet(x.train, y.train, nfolds = 5, alpha = 1)
  # CV to find a lasso regression model

lmin.lam <- lasso.cv$lambda.min
lcv.coefs <- coef(lasso.cv)
  # also get Ht and Leg as suggested predictors, but with slightly smaller coefs 

# we now measure the performance of these selected cross validated models

ridge.pred <- predict(ridge.cv, x.test, s = 'lambda.min')
lasso.pred <- predict(lasso.cv, x.test, s = 'lambda.min')
  # predictions of hipcentre using ridge/lasso regression models based on x.test data
  # s = 'lambda.min' means we use the model with lambda.min 
  # rather than the default lambda.1se

corr.ridge[i] <- cor(y.test, ridge.pred)
corr.lasso[i] <- cor(y.test, lasso.pred)
  # pearson correlation between predicted and actual hipcenter values
}

boxplot(corr.ridge, corr.lasso, 
        names = c('Ridge', 'Lasso'), 
        ylab = 'Test Correlation')
  # we see that the ridge regression is slightly better for these data

