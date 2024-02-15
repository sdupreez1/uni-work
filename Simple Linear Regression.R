data(faithful)

w <- c(faithful$waiting)
d <- c(faithful$eruptions)

model <- lm(w ~ d) # produce a linear model demonstrating how w responds to d

plot <- ggplot(faithful, aes(x = d, y = w)) + # load data
        geom_point() + # plot data as point
        geom_abline(coef(model)) # add line using coefs from SLR model ('model')
plot

r <- cor(w, d, method = 'pearson') 
    # r = 0.901 so evidence of linear relationship

coefs <- coef(model) # model coefs
resids <- resid(model) # model residuals (y - yhat)

lsq.Q <- sum((resids)**2) # sum of squared resids 


summ <- summary(model)
# t-values for the SLR model give us t_i = b_i/s_(b_i) as the test statistic to use
# to find P(beta_i > |t_i|) (p-values), allowing us to decide if we accept or reject H_0.

# Here, our p-values are all very low, telling us that the model is most likely accurate

se <- summ$sigma # how much on average do the observed values deviate from their corresponding predictions
R2 <- summ$r.squared # strength of linear correlation

Sxx <- sum((d - mean(d))**2) 

# Inference of Coefs (to confirm values in 'summ')

se.beta0 <- summ$coefficients[1, 2] # standard error of beta_0
se.beta1 <- summ$coefficients[2, 2] # standard error of beta_1

beta0hat <- coefs[1] # b_0
beta1hat <- coefs[2] # b_1

t0 <- beta0hat/se.beta0 # test stat for beta_0 (agrees with value in 'summ')
t1 <- beta1hat/se.beta1 # test stat for beta_1 (agrees with value in 'summ')

p0 <- 2*pt(t0, nrow(faithful)-2, lower.tail = FALSE) # p-value for beta_0
p1 <- 2*pt(t1, nrow(faithful)-2, lower.tail = FALSE) # p-value for beta_1 (both times 2 for 2 tails)


t_halpha <- qt(0.975, nrow(faithful)-2)
beta0conf <- c(beta0hat - t_halpha*se.beta0, 
               beta0hat + t_halpha*se.beta0) # manual confint for beta0 

beta1conf <- c(beta1hat - t_halpha*se.beta1,
               beta1hat + t_halpha*se.beta1) # manual confint for beta1
# can also just use confint(model, level = 0.95)

wconf <- predict(model, newdata = data.frame(d = 3), 
                 interval = 'confidence', level = 0.95) 
# 95% CI for the height of regression line at d=3

wpred <- predict(model, newdata = data.frame(d = 3),
                 interval = 'prediction', level = 0.95)
# 95% CI for the prediction of w from a new entry of d=3 using 'model'

confcompare <- c(wpred[1] - wconf[1],
                 wpred[2] - wconf[2],
                 wpred[3] - wconf[3])
# PI and CI have same fit values, PI has wider range than CI
# PI limits are same distance above and below CI limits 
#(PI takes into account error induced by model AND prediction)

# Residual Analysis!

par(mfrow = c(1, 2)) # 1x2 graph display
res_yhat <- plot(x = fitted(model, data = d), y = resids) + abline(h=0) # resids vs fitted w vals
res_w <- plot(x = d, y = resids) + abline(h=0) # resids vs d vals

res_yhat
res_w

# resids are spread seemingly randomly above and below 0 so seems like a linear 
# model was reasonable to use here, but make sure of normality using normal Q-Q plot

resid_hist <- hist(resids)
nqq <- qqnorm(resids)

resid_hist
nqq

# very linear normal QQ plot and hist shows relatively even spread of resids
# about 0 so even more evidence supporting a linear model

