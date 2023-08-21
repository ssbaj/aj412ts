\name{e_VECM}
\alias{e_VECM}
\title{VECM model}
\usage{
e_VECM()
}

\description{
Vector Error Correction Model
}

\examples{

# ------------------------------------------------------------------------  
(1) lag길이 결정        
VARselect(dat.VAR, lag.max=5, type='const')  
## type = const, trend, both, none   

(2) 공적분 점검          
ca.jo(x, type = c('trace', 'eigen'), ecdet = c('none', 'const', 'trend'), K=2) 
## K=2(Lag갯수), spec=c('longrun', 'transitory')  
## K=2 : lag order of the series (levels) in the VAR  
## season = NULL, dumvar = NULL)  
## spec : longrun=요한센모형 : transitory=변형된 요한센모형, '')  

(3) VECM모형 실행      
Result<-VECM(dat.VAR, lag=1, r=1, estim = 'ML', LRinclude = 'none') 
## r=1 => I(1) , estim = c('2OLS', 'ML'), LRinclude => 2OLS only   


(1)-(3)의 통합 실행        
------------------------------------------------------------------------  
library(tsDyn);library(vars); library(readxl) 
Adata<-read_excel('Eviews13.xlsx') 
lexpen<-ts(Adata$lexpen, start=c(1985,1), frequency=4) 
lgni<-ts(Adata$lgni, start=c(1985,1), frequency=4) 
dat.VAR<-cbind(lexpen, lgni) 

VARselect(dat.VAR, lag.max=5, type='const')  

johansen결과 <- ca.jo(dat.VAR, type = c('trace'), ecdet = c('none'), K = 2) 
summary(johansen결과)  

Result<-VECM(dat.VAR, lag=1, r=1, include='const', estim = 'ML', LRinclude = 'none')  
summary(Result)  

--- 충격반응함수 및 예측오차분산분해 ---    
VAR결과<-vec2var(johansen결과, r=1) 

--- Serial Correlation 
serial1<-serial.test(VAR결과, lags.pt=5, type='PT.asymptotic') 

--- Arch effects ---  
Arch1 <- arch.test(VAR결과, lags.multi=15, multivariate.only=TRUE) 

--- Normality of Residuals --- 
Normal1 <-normality.test(VAR결과, multivariate.only=TRUE) 

--- Impulse Response Function --- 
irf.lexpen<-irf(VAR결과,impulse='lgni',response='lexpen',n.ahead=20, boot=TRUE) 
plot(irf.lexpen, ylab='lexpen', main='GNI shock to Expen') 

irf.lgni<-irf(VAR결과,impulse='lexpen',response='lgni', n.ahead=20, boot=TRUE) 
plot(irf.lgni, ylab='GNI', main='Expen shock to GNI') 

--- Forecasting Error Variance Decomposition --- 
FEVD1 <- fevd(VAR결과, n.ahead=20 ) 
plot(FEVD1) 

--- Predictions ---- 
predictions <- predict(VAR결과, n.ahead=5, ci=0.95 ) 


}