# ERROR CORRECTION MODEL()

e_ecm<-function(explaining=0){
if(explaining==0) {
cat("    ", '\n')
cat("  library(vars) ; library(aj412ts) ", '\n')
cat("  dat <- read.csv('comodity_price.csv')  ", '\n')
cat("  또는, dat <- comodity_price # aj412ts의 내장데이터 ", '\n')
cat("  ", '\n')
cat("# create Time Series Objects  ", '\n')
cat("  gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)  ", '\n')
cat("  silver <- ts(log(dat$silver),start=c(1993,11),frequency=12)  ", '\n')
cat("  plat <- ts(log(dat$plat),start=c(1993,11),frequency=12)  ", '\n')
cat("  pall <- ts(log(dat$pall),start=c(1993,11),frequency=12)  ", '\n')
cat("  ", '\n')
cat("  adfg1 <- ur.df(gold, type = 'trend', selectlags = c('BIC'))  ", '\n')
cat("  ## trend, drift, none  ", '\n')
cat("  summary(adfg1)  # 1 lag, adf = -1.4027, crit = -3.99 -3.43 -3.13 (1pct  5pct 10pct)  ", '\n')
cat("  ", '\n')
cat("# create single object with two variables  ", '\n')
cat("  data <- ts.union(gold,silver)  ", '\n')
cat("  ", '\n')
cat("## estimate long run regression with constant  ", '\n')
cat("  gold.eq <- lm(gold~silver,data=data)  ", '\n')
cat("  summary(gold.eq)  ", '\n')
cat("  plot.ts(gold.eq$residuals)  ", '\n')
cat("  ", '\n')
cat("# check for unit root  ", '\n')
cat("  error.gold <- ur.df(gold.eq$residuals,lags=1,type='none')  ", '\n')
cat("  summary(error.gold)  ", '\n')
cat("# Test statistics from Engle & Granger (1987) or Engle & Yoo (1987)  ", '\n')
cat("# Reject Ho: Unit root in the residual == They are cointegrated  ", '\n')
cat("  ", '\n')
cat("# Prepare data for ECM (data has 231 obs and lag 1st diff has 229)  ", '\n')
cat("# gold.d = const + error.ecm1 + gold.d1 + silver.d1  ", '\n')
cat("  gold.d <- diff(gold)[-1]  ", '\n')
cat("  silver.d <- diff(silver)[-1]  ", '\n')
cat("  error.ecm1 <- gold.eq$residuals[-1:-2]  ", '\n')
cat("  gold.d1 <- diff(gold)[-(length(gold)-1)]  ", '\n')
cat("  silver.d1 <- diff(silver[-(length(silver)-1)])  ", '\n')
cat("  ", '\n')
cat("# Estimate ECM  ", '\n')
cat("   ", '\n')
cat("  ecm.gold <- lm(gold.d~error.ecm1+gold.d1+silver.d1)  ", '\n')
cat("   ", '\n')
cat("  summary(ecm.gold)  ", '\n')
cat("# Note that the alpha(error.ecm1의 계수값) is insignificant and close to zero  ", '\n')
cat("# Therefore gold is weakly exogenous with respect to the cointegrating parameters ", '\n')
cat("# since it does not adjust to past deviations from long-run equilibrium.", '\n')
cat("   ", '\n')
cat("  ecm.silver <- lm(silver.d~error.ecm1+gold.d1+silver.d1)  ", '\n')
cat("   ", '\n')
cat("  summary(ecm.silver)  ", '\n')
cat("# Note that the alpha(error.ecm1의 계수값) is negative and significant, so silver does all the work  ", '\n')
cat("# Implies there is Granger causality from gold to silver  ", '\n')
cat("# It takes 1/0.078 periods to return to equilibrium  ", '\n')
cat("    ", '\n')
}}
