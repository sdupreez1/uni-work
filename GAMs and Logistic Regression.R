library(MASS)
library(faraway)
library(splines)
library(gam)

data('seatpos')


# Polynomial Regression
y = seatpos$hipcenter
x = seatpos$Ht

lmodel = lm(y ~ x)
pmodel = lm(y ~ poly(x, degree = 2))
  # we form both a SLR model and a polynomial linear model

lsumm = summary(lmodel)
psumm = summary(pmodel)
  # summaries of both models to find coefs and performance measures

x.sort = sort(x)

lpred = predict(lmodel, newdata = list(x = x.sort), se = TRUE)  
ppred = predict(pmodel, newdata = list(x = x.sort), se = TRUE)
  # predict() predicts hipcenter based on given height using model, 
  # we then use these to make CIs for both models

lCI  = cbind(lpred$fit - 2*lpred$se.fit, lpred$fit + 2*lpred$se.fit)
pCI  = cbind(ppred$fit - 2*ppred$se.fit, ppred$fit + 2*ppred$se.fit)

par(mfrow = c(1, 2)) # plot the next 2 graphs next to each other 

plot(x, y) + abline(lmodel) + matlines(x.sort, lCI) 
  # SLR model plot with CI
plot(x, y) + lines(x.sort, ppred$fit) + matlines(x.sort, pCI) 
  # polynomial model plot with CI



# Step-Wise Regression
step6 = lm(y ~ cut(x, 6)) 
  # linear step wise reg fct w 6 cut points (i.e 7 intervals)
spred = predict(step6, newdata = list(x = x.sort), se = TRUE) 
  # values predicted by stepwise model

sCI = cbind(spred$fit - 2*spred$se.fit, spred$fit + 2*spred$se.fit)
  # plus/minus 2*sd CI for model

plot(x, y) + lines(x.sort, spred$fit) + matlines(x.sort, sCI)
  # stepwise reg plot with CI



# Splines
cuts = unname(quantile(x))[2:4] 
  # using quartiles as knots here

spline = lm(y ~ bs(x, degree = 1, knots = cuts)) 
  # bs() fits a linear spline
pred.sp = predict(spline, newdata = list(x = x.sort), se = TRUE)
  # values predicted by linear spline model
spCI = cbind(pred.sp$fit - 2*pred.sp$se.fit, pred.sp$fit + 2*pred.sp$se.fit)
  # plus/minus 2*sd confidence intervals

plot(x, y) + lines(x.sort, pred.sp$fit, col = 'red') + 
  matlines(x.sort, spCI, col = 'red', lty = 3, lwd = 2)
  # plot of linear spline w 6 knots, plus confidence interval for prediction model

smooth = smooth.spline(x, y, df = 3, cv = TRUE) 
  # fits a cubic (df=3) smoothing spline, choosing lambda with cross-validation
  # smoothing spline inherently uses all x_i as knots so no need to specify

plot(x, y) + lines(smooth) 
  # cubic smoothing spline looks very linear



# General Additive Models (GAMs)
gam = gam(y ~ ns(Age, df = 5) + s(Thigh, df = 3) + x, data = seatpos)
  # fits GAM with a natural cubic spline, ns(), modelling y~ns(Age),
  # a smoothing spline, s(), modelling y~s(Thigh),
  # and SLR model, x, modelling y~x
  # note that s() is only for use inside of gam()

par(mfrow = c(1,3))
plot(gam, se = TRUE)
  # plots impact of each var in gam against height using the different 
  # specified models, also includes confidence intervals



# Logistic Regression
admit <- read.csv("https://www.maths.dur.ac.uk/users/hailiang.du/data/admit.csv")
  # data on whether or not an applicant is admitted based on gre, gpa, and rank of university

pairs(admit, col = admit[,1] + 1) 
  # colours points based on if applicant is admitted or not

admit$rank <- factor(admit$rank) 
  # changes rank to a factor rather than a number
  
glm.fit <- glm(admit ~ ., data = admit, family = binomial)
  # fits a logistic regression model to data, and assigns a value between 0 and 1
  # based on the weighted (weights determined by model) sum of the predictors

gpred = predict(glm.fit, type = 'response') 
  # uses model to predict admittance based on given data

admit.pred = round(gpred) 
  # if model predicted a value >=0.5 then we take that to mean it predicts admittance  
  # can take value predicted as 'probability' of acceptance, i.e >=0.5 means more likely to accept than not

errortable = table(admit.pred - admit$admit)
  # shows number of correct predictions (284) and missed predictions (116 =97+19)
  # hence model has an error rate of 116/400 = 29% in this case

