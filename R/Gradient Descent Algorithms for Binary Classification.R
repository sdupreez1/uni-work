

# Data Generation and Useful Functions

library(numDeriv)
library(nloptr)
library(knitr)

set.seed(2023)

data_generating_model <- function(n,w) {
  z <- rep( NaN, times=n*2 )
  z <- matrix(z, nrow = n, ncol = 2)
  z[,1] <- runif(n, min = -10, max = 10)
  p <- w[1] + w[2]*z[,1] 
  p <- exp(p) / (1+exp(p))
  z[,2] <- rbinom(n, size = 1, prob = p)
  return(z)
}

n_obs <- 500
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true) 
w_true <- as.numeric(glm(z_obs[,2]~1+z_obs[,1],family="binomial")$coefficients)

prediction_rule <- function(x,w) {
  h <- w[1]+w[2]*x
  h <- exp(h) / (1.0 + exp(h))
  return (h)
}

loss_fun <- function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  ell <- -y*log(h) - (1-y)*log(1-h)
  return (ell)
}

empirical_risk_fun <- function(w,z,n) {
  x = z[,1]
  y = z[,2]
  R <- 0.0
  for (i in 1:n) {
    R <- R + loss_fun(w,z[i,])
  }
  R <- R / n
  return (R)
}

learning_rate <- function(t, t0 = 3){
  eta = t0/t
  return(eta)
}

grad_loss_fun <-  function(w,z) {
  x = z[1]
  y = z[2]
  h <- prediction_rule(x,w)
  grd <- c(h-y, (h-y)*x)
  return (grd)
}

grad_risk_fun <- function(w,z,n) {
  grd <- 0.0
  for (i in 1:n) {
    grd <- grd + grad_loss_fun(w,z[i,])
  }
  grd <- grd / n
  return (grd)
}

w <- c(-0.1, 1.5)
grad_risk_fun(w, z_obs, n_obs)

w <- c(-0.3, 0.3)

erf_deriv <- function(w){
  erf_fun <- function(w, z = z_obs, n = n_obs){
    return(empirical_risk_fun(w, z, n))
  }
  derv <- numDeriv::grad(erf_fun, w)
  return(derv)
}


# Gradient Descent

gd <- function(w0, tmax = 300, eta = 0.5){
  wt = w0
  chain = c()
  for (i in 1:tmax){
    wt <- wt - eta*erf_deriv(wt)
    chain <- rbind(chain, wt)  
  }
  return(chain)
}

gd_vals_etaconst <- gd(w0 = c(-0.3, 3))

plot(gd_vals_etaconst[,1], type = 'line') + abline(h = w_true[1], col = 'red')

plot(gd_vals_etaconst[,2], type = 'l') + abline(h = w_true[2], col = 'red')

gd_etaseq <- function(w0, tmax = 300, t0){
  wt = w0
  chain = c()
  for (t in 1:tmax){
    wt <- wt - learning_rate(t, t0)*erf_deriv(wt)
    chain <- rbind(chain, wt)  
  }
  return(chain)
}

gd_vals_etaseq <- gd_etaseq(w0 = c(-0.3, 3), t0 = 20)

plot(gd_vals_etaseq[,1], type = 'line') + abline(h = w_true[1], col = 'red')

plot(gd_vals_etaseq[,2], type = 'line') + abline(h = w_true[2], col = 'red')


# Batch Stochasitic Gradient Descent

n_obs <- 1000000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~1+z_obs[,1],family ="binomial")$coefficients)

batch_sto_gd <- function(w0, 
                   tmax = 300, 
                   eta = 0.5, 
                   m = 10){
  wt = w0
  chain = c()
  
  for (t in 1:tmax){
  J = sample.int(n = n_obs, size = m, replace = TRUE)
  
  if(m == 1){ #i.e online SGD
    zbatch <- matrix(z_obs[J,],1,2)
  } else{  #i.e batch SGD
    zbatch <- z_obs[J,]
  }
  
  wt <- wt - eta*grad_risk_fun(wt, zbatch, m)   
  chain <- rbind(chain, wt)
  }
  
  return(chain)
}

sto_gd_etaconst <- batch_sto_gd(w0 = c(-0.3, 3))

plot(sto_gd_etaconst[,1], type = 'l') + abline(h = w_true[1], col = 'red')

# AdaGrad
set.seed(2023)
n_obs <- 1000000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~1+z_obs[,1],family="binomial")$coefficients)

adagrad <- function(w0, tmax, eta, m, eps){
  wt = w0
  chain = c()
  
  G = rep(0.0, times=length(w))
  
  for (t in 1:tmax){
    J = sample.int(n = n_obs, size = m, replace = TRUE)
    
    if (m ==1){ #online AdaGrad
      zbatch <- matrix(z_obs[J,],1,2)
    } else{ #batch AdaGrad
      zbatch <- z_obs[J,]
    }
    
    G <- G + (grad_risk_fun(wt, zbatch, m))^2
    
    wt <- wt - eta*(1/sqrt(G + eps))*grad_risk_fun(wt, zbatch, m)   
    chain <- rbind(chain, wt)
  }
  
  return(chain) 
}

onladagrad_vals <- adagrad(w0=c(-0.3,3), tmax=500, eta=1, m=1, eps=1e-06)
plot(onladagrad_vals[,1], type = 'line') + abline(h = w_true[1], col = 'red')


# Batch SGD with projection
n_obs <- 1000000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~ 1+z_obs[,1],family="binomial")$coefficients)

boundary <- 2.0 # this is the value |w|_{2}^{2} <= boundary
# auxiliary functions to compute the projection
eval_f0 <- function( w_proj, w_now ){ 
  return( sqrt(sum((w_proj-w_now)^2)) )
}
eval_grad_f0 <- function( w, w_now ){ 
  return( c( 2*(w[1]-w_now[1]), 2*(w[2]-w_now[2]) ) )
}
eval_g0 <- function( w_proj, w_now) {
  return( sum(w_proj^2) -(boundary)^2 )
}
eval_jac_g0 <- function( x, w_now ) {
  return(   c(2*w[1],2*w[2] )  )
}

w <- c(-0.1,0.3)
out <- nloptr(x0=c(0.0,0.0),
              eval_f=eval_f0,
              eval_grad_f=eval_grad_f0,
              eval_g_ineq = eval_g0,
              eval_jac_g_ineq = eval_jac_g0, 
              w_now=w,
              opts = list("algorithm" = "NLOPT_LD_MMA",
                          "xtol_rel"=1.0e-8) 
)

batch_sgd_proj <- function(w0, 
                           tmax = 1000, 
                           eta = 0.1, 
                           m = 1){
  wt = w0
  chain = c()
  
  for (t in 1:tmax){
    J = sample.int(n = n_obs, size = m, replace = TRUE)
    
    if(m == 1){ #i.e online SGD
      zbatch <- matrix(z_obs[J,],1,2)
    } else{  #i.e batch SGD
      zbatch <- z_obs[J,]
    }
    
    wt <- wt - eta*grad_risk_fun(wt, zbatch, m)
    
    out <- nloptr(x0=c(0.0,0.0),
                  eval_f=eval_f0,
                  eval_grad_f=eval_grad_f0,
                  eval_g_ineq = eval_g0,
                  eval_jac_g_ineq = eval_jac_g0, 
                  w_now=wt,
                  opts = list("algorithm" = "NLOPT_LD_MMA",
                              "xtol_rel"=1.0e-8)
    )
    
    wt <- out$solution
    chain <- rbind(chain, wt)
  }
  
  return(chain)
}

plot(batch_sgd_proj(w0 = c(-0.3, 0.3), eta = 0.1, tmax = 1500)[,2],
     type = 'line') + abline(h = w_true[2], col = 'red')


# Stochastic Variance Reduced Gradient Descent
set.seed(2023)
n_obs <- 100000
w_true <- c(0,1)  
z_obs <- data_generating_model(n = n_obs, w = w_true)
w_true <- as.numeric(glm(z_obs[,2]~ 1+ z_obs[,1],family = "binomial" )$coefficients)

svrgd <- function(w0, tmax = 500, eta = 0.5, kppa = 100, m = 1){
  wt = w0
  chain = c()
  cv_w <- wt
  
  erf_fun <- function(wt, z = z_obs, n=n_obs) {
    return( empirical_risk_fun(wt, z, n) ) 
  }
  
  cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #control variate
  
  for (t in 1:tmax){
    J = sample.int(n = n_obs, size = m, replace = TRUE)
    
    if(m == 1){ #i.e online SGD
      zbatch <- matrix(z_obs[J,],1,2)
    } else{  #i.e batch SGD
      zbatch <- z_obs[J,]
    }
    
    erf_fun <- function(wt, z = zbatch, n=m) {
      return( empirical_risk_fun(wt, z, n) ) 
    }
    
    wt <- wt - eta*(numDeriv::grad( erf_fun,wt) - numDeriv::grad(erf_fun,cv_w) + cv_grad_risk)
    chain <- rbind(chain, wt)
  
  if ( (t %% kppa) == 0) {
    cv_w <- wt  
    erf_fun <- function(wt, z = z_obs, n=n_obs) {
      return( empirical_risk_fun(wt, z, n) ) 
    }
    cv_grad_risk <- numDeriv::grad( erf_fun, cv_w ) #control variate
    }
  }
  return(chain)
}

plot(svrgd(w0 = c(-0.3, 3.0))[,1], type = 'line') + abline(h = w_true[1], col = 'red')




