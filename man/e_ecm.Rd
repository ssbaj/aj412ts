\name{e_ecm}
\alias{e_ecm}
\title{Calc: ECM model}
\usage{
e_ecm()
}
\description{
Estimating Error Correction Model model
}
\examples{
library(vars)   
library(aj412s)
dat <- comodity_price
   
# create Time Series Objects   
gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)   
silver <- ts(log(dat$silver),start=c(1993,11),frequency=12)   
plat <- ts(log(dat$plat),start=c(1993,11),frequency=12)   
pall <- ts(log(dat$pall),start=c(1993,11),frequency=12)   
   
# aTSA의 adf.test명령문사용
adf.test(gold) 

# 또는 vars의 urdf명령문사용
adfg1 <- ur.df(gold, type = 'trend', selectlags = c('BIC'))   
## trend, drift, none   
summary(adfg1)  # 1 lag, adf = -1.4027, crit = -3.99 -3.43 -3.13 (1pct  5pct 10pct)   
   
# create single object with two variables   
data <- ts.union(gold,silver)   
   
## estimate long run regression with constant   
gold.eq <- lm(gold~silver,data=data)   
summary(gold.eq)   
plot.ts(gold.eq$residuals)   
   
# check for unit root   
error.gold <- ur.df(gold.eq$residuals,lags=1,type='none')   
summary(error.gold)   
# we should actually use test statistics from Engle & Granger (1987) or Engle & Yoo (1987)   
# able to reject null of unit root in the residual - they are cointegrated   
   
# prepare data for ECM (data has 231 obs and lag 1st diff has 229)   
# gold.d = const + error.ecm1 + gold.d1 + silver.d1   
gold.d <- diff(gold)[-1]   
silver.d <- diff(silver)[-1]   
error.ecm1 <- gold.eq$residuals[-1:-2]   
gold.d1 <- diff(gold)[-(length(gold)-1)]   
silver.d1 <- diff(silver[-(length(silver)-1)])   
   
# estimate ECM   
ecm.gold <- lm(gold.d~error.ecm1+gold.d1+silver.d1)   
summary(ecm.gold)   
# Note that the alpha is insignificant and close to zero   
# Therefore gold is weakly exogenous with respect to the cointegrating parameters  
since it does not adjust to past deviations from long-run equilibrium. 
    
ecm.silver <- lm(silver.d~error.ecm1+gold.d1+silver.d1)   
summary(ecm.silver)   
# Note that the alpha is negative and significant, so silver does all the work   
# Implies there is Granger causality from gold to silver   
# It takes 1/0.078 periods to return to equilibrium   
}