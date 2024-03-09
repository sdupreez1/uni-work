library('nnet')
library('faraway')
library('ggplot2')

data(ozone)
O3 <- ozone$O3

# Naive Training of NN
o3.nn <- nnet(O3 ~ temp + ibh + ibt, data = ozone,
              size = 2, linout = TRUE)

EF <- sum((o3.nn$fitted.values - O3)^2) 
  # RSS = deviation from the mean not explianed by model
  # can also use o3.nn$value
SST <- sum((O3 - mean(O3))^2) 
  # SST = total deviation from the mean

EF - SST 
  # very small, so RSS and SST are very close, meaning that none of the variance
  # is captured by the model and hence the nn does effectively no prediction



# Standardising Data for Better NN Weights
oz.s <- scale(ozone)
  # standardises data so SD = 1 and mean = 0 for all columns
  # this results in better performance from nnet since weights of connections
  # in nn deal with same sized numbers

best.nn <- o3.nn
EF.best <- best.nn$value 
  # RSS of current best model
for(i in 1:100){  # loop to fit 100 nns and with different starting weights 
                  # and find best
  set.seed(i)
    # new seed for starting weights each iteration
  new.nn <- nnet(O3 ~ temp + ibh +ibt, data = oz.s,
                 size = 2, linout = TRUE)
  
  EF.new <- new.nn$value
  
  if(EF.new < EF.best){ # if new model has better SSR than current best, 
                        # then replace current best and its RSS
    best.nn <- new.nn
    EF.best <- new.nn$value
  }
}

SST.s <- sum((oz.s[,'O3'] - mean(oz.s[,'O3']))^2)
EF.best - SST.s
  # much more significant than before, hence this new model explains more 
  # of the total variance (i.e. amount not explained by model is lower)
  # and has much better predictive ability

summary(best.nn)
  # weights between nodes



# Plotting NN Predictions (for temp in range [-3,3] with ibh = ibt = 0)
xt.s <- expand.grid(temp=seq(-3,3,0.1),ibh=0,ibt=0)
  # standardised x values used

oz.s.pred.temp <- predict(best.nn, newdata = xt.s)
  # the values predicted by the nn which used standardised data

oz.pred.temp <- oz.s.pred.temp*sd(O3) + mean(O3)
xt <- xt.s*sd(ozone$temp) + mean(ozone$temp)
  # reverting standardised data to physically meaningful data

ggplot(data = xt, aes(temp, oz.pred.temp)) + 
  geom_point() + 
  geom_point(data = ozone, aes(temp, O3), col = 'red', shape = 'x')
  # plot of predictions in black and actual values in red  


# Plot of NN predictions for ibh in [-3,3]
xibh.s <- expand.grid(temp=0,ibh=seq(-3,3,0.1),ibt=0)

oz.s.pred.ibh <- predict(best.nn, newdata = xibh.s)

oz.pred.ibh <- oz.s.pred.ibh*sd(O3) + mean(O3)
xibh <- xibh.s*sd(ozone$ibh) + mean(ozone$ibh)

ggplot(data = xibh, aes(ibh, oz.pred.ibh)) + 
  geom_point() + 
  geom_point(data = ozone, aes(ibh, O3), col = 'red', shape = 'x')


# Plot of NN predictions for ibt in [-3,3]
xibt.s <- expand.grid(temp=0,ibh=0,ibt=seq(-3,3,0.1))

oz.s.pred.ibt <- predict(best.nn, newdata = xibt.s)

oz.pred.ibt <- oz.s.pred.ibt*sd(O3) + mean(O3)
xibt <- xibt.s*sd(ozone$ibt) + mean(ozone$ibt)

ggplot(data = xibt, aes(ibt, oz.pred.ibt)) + 
  geom_point() + 
  geom_point(data = ozone, aes(ibt, O3), col = 'red', shape = 'x')

  # all three have discontinuities, possibly due to large weightings in NN#
  # fix this using shrinkage methods


# Fitting a NN with shrinkage
shr.best.nn <- o3.nn
shr.EF.best <- shr.best.nn$value 
# RSS of current best model

for(i in 1:100){
  
  set.seed(i)
 
  shr.new.nn <- nnet(O3 ~ temp + ibh +ibt, data = oz.s,
                 size = 2, linout = TRUE, decay = 0.001) # decay creates shrinkage
  
  shr.EF.new <- shr.new.nn$value
  
  if(shr.EF.new < shr.EF.best){  
    shr.best.nn <- shr.new.nn
    shr.EF.best <- shr.new.nn$value
  }
}

shr.best.nn$value - best.nn$value
shr.best.nn$wts - best.nn$wts
  # shr.best has a slightly larger RSS, but there is a big difference between 
  # the weights of the models and hence predictive ability (better with smaller weights)



# Plots of Predictions from NNs with shrinkage applied
# temp
xt.s <- expand.grid(temp=seq(-3,3,0.1),ibh=0,ibt=0)

oz.s.pred.temp <- predict(shr.best.nn, newdata = xt.s)

oz.pred.temp <- oz.s.pred.temp*sd(O3) + mean(O3)
xt <- xt.s*sd(ozone$temp) + mean(ozone$temp)

ggplot(data = xt, aes(temp, oz.pred.temp)) + 
  geom_point() + 
  geom_point(data = ozone, aes(temp, O3), col = 'red', shape = 'x')


# ibh
xibh.s <- expand.grid(temp=0,ibh=seq(-3,3,0.1),ibt=0)

oz.s.pred.ibh <- predict(shr.best.nn, newdata = xibh.s)

oz.pred.ibh <- oz.s.pred.ibh*sd(O3) + mean(O3)
xibh <- xibh.s*sd(ozone$ibh) + mean(ozone$ibh)

ggplot(data = xibh, aes(ibh, oz.pred.ibh)) + 
  geom_point() + 
  geom_point(data = ozone, aes(ibh, O3), col = 'red', shape = 'x')


# ibt
xibt.s <- expand.grid(temp=0,ibh=0,ibt=seq(-3,3,0.1))

oz.s.pred.ibt <- predict(shr.best.nn, newdata = xibt.s)

oz.pred.ibt <- oz.s.pred.ibt*sd(O3) + mean(O3)
xibt <- xibt.s*sd(ozone$ibt) + mean(ozone$ibt)

ggplot(data = xibt, aes(ibt, oz.pred.ibt)) + 
  geom_point() + 
  geom_point(data = ozone, aes(ibt, O3), col = 'red', shape = 'x')

  # no discontinuities after applying shrinkage!

  # the discontinuities came from large jumps in O3 value predictions due to
  # the large weightings in the NN before shrinkage.



# NN Classification with Multiple Classes in Response Variable
data(iris)

y <- class.ind(iris$Species) 
  # 1 for data points with species corresponding to the column of this matrix

x <- iris[,1:3]
  # we use sepal.length, sepal.width, and petal.length as our predictors

training <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
  # 75 random numbers to use to produce training data
  # 25 from each species type to prevent one species being trained for better than the others

y_train <- y[training,]
x_train <- x[training,]

y_valid <- y[-training,]
x_valid <- x[-training,]



# Training a NN to predict Species
x.s <- scale(x_train)

iris.nn <- nnet(x = x_train, y = y_train, size = 2,
                softmax = T, # gives log-linear model
                decay = 0.00001,
                maxit = 200)

best.iris <- iris.nn
iris.EF <- best.iris$value

for(i in 1:100){
  
  set.seed(i)
  
  new.iris <- nnet(x = x.s, y = y_train, size = 2, softmax = T, decay = 0.00001,
                   maxit = 200) # could use CV to find better decay values
  
  new.EF <- new.iris$value
  
  if(new.EF < iris.EF){
    
    best.iris <- new.iris
    iris.EF <- new.EF
    
  }
}

y_pred <- round(predict(best.iris, newdata = scale(x_valid)))
pred_error <- table(y_valid - y_pred)
  # only 8 incorrect predictions from this model!
