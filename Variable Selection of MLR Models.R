library('faraway')
library('corrplot')
library('leaps')
library('ggplot2')
library('reshape2')
library('tidyr')
library('tidyverse')

data('fat')



# Extracting and Plotting Data
fat1 <- fat[,-c(2,3,8)] # 252 rows/data, 15 cols/predictors

pairs(fat1) # unnecessary to have plots on both top and bottom panel

panel.cor <- function(x,y){
  usr <- par('usr'); on.exit(par(usr)) # executes par(usr) when fct ends to reset params
  par(usr = c(0,1,0,1))
  r <- round(cor(x,y), digits = 2) # round corr values to 2dp
  txt <- paste0(" ", r)
  text(0.5, 0.5, txt, cex = 0.8)
}

upper.panel <- function(x, y){
  points(x,y, pch = 19)
}

pairs(fat1, lower.panel = panel.cor,
      upper.panel = upper.panel) # corr coef values in lower panel, plots in upper

corrplot(cor(fat1), method = "number", type = "upper", diag = FALSE) 
corrplot.mixed(cor(fat1), upper = "ellipse", lower = "number",number.cex = .7)



# Regression Model
reg1 <- lm(brozek ~ adipos, fat1)

summary(reg1)

confint(reg1, level = 0.95)

new.data = data.frame(adipos = 22)
predict(reg1, newdata = new.data, 
        interval = 'confidence',level = 0.95) # 95% CI for height of reg1 at adipos = 22
predict(reg1, new.data, 
        interval = 'prediction', level = 0.95) # 95% CI for predicted value when observing adipos = 22

plot(fat1$adipos, fat1$brozek) + abline(coef(reg1), col = 'red')



# Residual Analysis
par(mfrow=c(1,2)) # to have two plots side-by-side
plot(reg1, which = 1)
plot(reg1, which = 2)
# fairly random spread of residuals about 0 and linear normal QQ, so linear model is reasonable



# Multiple Linear Regression (MLR)
reg2 <- lm(brozek ~ adipos + age, fat1) # R^2 = 0.5716
reg3 <- lm(brozek ~ ., fat1) # R^2 = 0.74903 (most strong of these 3 models)
reg4 <- lm(brozek ~.-age, fat1) # R^2 = 0.7452



# Variable Selection - helps find best model for MLR
fwd <- regsubsets(brozek~., fat1, nvmax = ncol(fat1)-1) # forward stepwise selection of variables
fsumm <- summary(fwd) # stars show which vars are best for a MLR model with n predictor vars

fwd.vals <- data.frame('nvars' = c(1:(ncol(fat1)-1)),
                       'rss' = fsumm$rss,
                       'rsqared' = fsumm$rsq,
                       'adjr2' = fsumm$adjr2,
                       'cp' = fsumm$cp,
                       'bic' = fsumm$bic) # matrix of model selection criteria for fwd

fwd.minvals <- apply(fwd.vals[,c(2,5,6)], 2, min) # find mins of each column (specified by the '2') in fwd.vals
fwd.maxvals <- apply(fwd.vals[,c(3,4)], 2, max)
fwd.minvars <- apply(fwd.vals[,c(2,5,6)], 2, which.min) # for how many variables does this value arise from
fwd.maxvars <- apply(fwd.vals[,c(3,4)], 2, which.max) 
  # note: apply() returns 1 dimensional object, not matrices

fwd.mvals <-c(fwd.maxvals, fwd.minvals) 
fwd.mvars <- c(fwd.maxvars, fwd.minvars)

fwd.ms <-cbind(fwd.mvals, fwd.mvars, deparse.level = 0) # combine max/min vals/vars into one df ('0' means no row names)

melt.crit <- melt(fwd.vals, id.vars = 'nvars', 
                  variable.name = 'criterion') 
  # combine all criterion cols into one var to use for multiple ggplots

fwfinal.ms <- rownames_to_column(as.data.frame(fwd.ms), # t() 'rotates'/transposes the whole df, this pipeline also labels criterion accordingly
                                var = 'criterion') |>
  rename(replace = c('V1' = 'value', 'V2' = 'nvars'))
  # changes min values/positions into a easier to use form for ggplot


ggplot(melt.crit, aes(nvars, value)) + # x = nvars, y = value for all plots
  geom_line() + 
  facet_wrap(criterion~., nrow = 3, ncol = 2, scales = 'free_y') +
  geom_point(data = fwfinal.ms, colour = 'red') # adds the min/maxes of all criterion (note: inputted df needs same labels as first ggplot() df)
  # plot all criterion on their own graphs with ylims proportional to the criterion's scale


bwd <- regsubsets(brozek~., data = fat1, method = 'backward', nvmax=ncol(fat1)) 
  # backward stepwise selection now
bsumm <- summary(bwd)

bwd.vals <- data.frame(nvars = c(1:(ncol(fat1)-1)),
                       rsq = bsumm$rsq,
                       rss = bsumm$rss,
                       adjr2 = bsumm$adjr2,
                       cp = bsumm$cp,
                       bic = bsumm$bic)

bwd.maxvals <- apply(bwd.vals[c(2, 4)], 2, max)
bwd.minvals <- apply(bwd.vals[c(3, 5, 6)], 2, min)
bwd.maxvars <- apply(bwd.vals[c(2, 4)], 2, which.max)
bwd.minvars <- apply(bwd.vals[c(3, 5, 6)], 2, which.min)

bwd.mvals <- c(bwd.maxvals, bwd.minvals)
bwd.mvars <- c(bwd.maxvars, bwd.minvars)

bwd.ms <- rbind(bwd.mvals, bwd.mvars, deparse.level = 0)

melt.bwd <- melt(bwd.vals, id.vars = 'nvars', variable.name = 'criterion')

bwfinal.ms <- rownames_to_column(as.data.frame(t(bwd.ms)), var = 'criterion') |>
  rename(replace = c('V1' = 'value', 'V2' = 'nvars'))

ggplot(melt.bwd, aes(nvars, value)) +
  geom_line() +
  facet_wrap(criterion~., nrow = 3, ncol = 2, scales = 'free_y') +
  geom_point(data = bwfinal.ms, colour = 'red')

# backward and forward stepwise selection of variables agree


