library('ggplot2')
library('rpart')
library('rpart.plot')
library('caret')

data_surrogate <- data.frame(x = c(runif(20, 0, 0.4), 
                                   runif(40, 0.4, 0.65), 
                                   runif(30, 0.65, 1)),
                             y = c(rnorm(20, 1.5), 
                                   rnorm(40, 0), 
                                   rnorm(30, 2)))

data_surrogate_test <- data.frame(x = c(runif(200, 0, 0.4), 
                                   runif(400, 0.4, 0.65), 
                                   runif(300, 0.65, 1)),
                             y = c(rnorm(200, 1.5), 
                                   rnorm(400, 0), 
                                   rnorm(300, 2)))

ggplot(data_surrogate[1:20,], aes(x, y)) +
  geom_point(col = 'blue') +
  geom_point(aes(x, y), data_surrogate[21:60,], col = 'red') +
  geom_point(aes(x, y), data_surrogate[61:90,], col = 'green')

rtree = rpart(y~x, data_surrogate, model = TRUE)

printcp(rtree)  # this performs default 10-fold CV on rtree cp values
plotcp(rtree)
  # shows and plots the cp values used in CV for pruning and corresponding 
  # size of tree + prediction errors when this cp is used 
  # [suggests that ~cp = 0.07 good]

rtree.prune = prune.rpart(rtree, cp = 0.07)
  # pruned tree (reduces complexity/number of branches of tree) based on
  # cost-complexity parameter (cp {alpha in notes}) value 
  # [cp increases cost, but decreases complexity]

summary(rtree.prune)

rpart.plot(rtree)
rpart.plot(rtree.prune)
  # compare prune vs unpruned trees

rtree.pred = data.frame(x = data_surrogate_test$x,
                              y = predict(rtree, data_surrogate_test))
prune.rtree.pred = data.frame(x = data_surrogate_test$x,
                        y = predict(rtree.prune, data_surrogate_test))

mse.rtree = mean((rtree.pred$y - data_surrogate_test$y)^2)
mse.rtree.prune = mean((prune.rtree.pred$y - data_surrogate_test$y)^2)
  # pruned MSE < normal MSE

ggplot(data_surrogate[1:20,], aes(x, y)) +
  geom_point(col = 'blue') +
  geom_point(aes(x, y), data_surrogate[21:60,], col = 'red') +
  geom_point(aes(x, y), data_surrogate[61:90,], col = 'green') +
  geom_point(aes(x, y), rtree.pred, shape = 'x')

ggplot(data_surrogate[1:20,], aes(x, y)) +
  geom_point(col = 'blue') +
  geom_point(aes(x, y), data_surrogate[21:60,], col = 'red') +
  geom_point(aes(x, y), data_surrogate[61:90,], col = 'green') +
  geom_point(aes(x, y), prune.rtree.pred, shape = 'x')


set.seed(1)
library(party)
  # uses statistical tests [in ctree()], i.e p-values, to determine best 
  # predictors for tree splits unlike rpart which apparently tends to favor 
  # categorical predictors with many factors
library(MASS)
  # for the 'Boston' data

data("Boston")

train = sample(1:nrow(Boston), nrow(Boston)/2)

dat.train = Boston[train,]
dat.test = Boston[-train,]

boston.tree = ctree(medv~., dat.train, 
                    controls = ctree_control(mincriterion = 0.999))
  # mincriterion = 0.999 means a split only occurs if its p-value is < 0.001
  # this does the same as pruning so just make larger to prune more

  # weird to return predictions of nodes, but can tell roughly what predictions
  # are by using box plots at bottom

summary(boston.tree)

plot(boston.tree)

pred.boston.tree = data.frame(medv = predict(boston.tree, dat.test),
                              Boston[,1:13])
ggplot() + 
  geom_point(aes(dis, medv), pred.boston.tree) + 
  geom_point(aes(dis, medv), dat.train, col = 'red')
  # there are 11 distinct black lines y = {predicted value} as expected by 
  # the 11 terminal nodes in boston.tree

compare.medv.tree = data.frame(pred.medv = pred.boston.tree$medv,
                                 actual.medv = dat.test$medv)

ggplot(compare.medv.tree, aes(actual.medv, pred.medv)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = 'red')
  # plot of predicted medv values against actual medv values
  # the y=x line is the line we're aiming for the predictions to lie on
  # [would mean the prediction is equal to the actual value]

mse.boston.tree = mean((pred.boston.tree$medv - dat.test$medv)^2)

boston.tree.tree = tree(medv~., Boston)
  # another tree fit, just using tree::tree() rather than party::ctree()

boston.forest = cforest(medv~., dat.train, 
                        controls = cforest_control(mtry = 4, trace = T))

pred.boston.forest = data.frame(medv = predict(boston.forest, newdata = dat.test),
                                Boston)

mse.boston.forest = mean((pred.boston.forest$medv - dat.test$medv)^2)
  # forest MSE < tree MSE

compare.medv.forest = data.frame(pred.medv = pred.boston.forest$medv,
                          actual.medv = dat.test$medv)

ggplot(compare.medv.forest, aes(actual.medv, pred.medv)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = 'red')
  
varimp(boston.forest)
  # prints contribution of different variables to MSE of model

test.err = double(13)
for(mtry_t in 1:13){
  fit = cforest(medv~.,
               data = dat.train, 
               controls = cforest_control(mtry=mtry_t, ntree=100))
  pred = predict(fit, newdata = dat.test)
  test.err[mtry_t] = mean((pred - dat.test$medv)^2)
}
plot(test.err)
  # to find optimal no of variables to try at each split in random forest 
  # (i.e the mtry value)
mtry.best = which.min(test.err)
  # so mtry = 7 gives us the lowest MSE



# repeating all of the above for mtry = 7
boston.forest = cforest(medv~., dat.train, 
                        controls = cforest_control(mtry = 7, trace = T))

mse.boston.forest = mean((pred.boston.forest$medv - dat.test$medv)^2)

compare.medv.forest = data.frame(pred.medv = pred.boston.forest$medv,
                                 actual.medv = dat.test$medv)

ggplot(compare.medv.forest, aes(actual.medv, pred.medv)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, col = 'red')

varimp(boston.forest)



library(gbm)
  # we use gbm for boosting

set.seed (517)
boost.boston = gbm(medv~., data = dat.train, distribution = "gaussian",
                  n.trees = 1000, interaction.depth = 2)
summary(boost.boston)

pred.boost = predict(boost.boston, newdata = dat.test, n.trees =1000)
mse.boost = mean((pred.boost - dat.test$medv)^2)
