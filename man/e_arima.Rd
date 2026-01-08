\name{e_Arima}
\alias{e_Arima}
\title{e_Arima}
\usage{
e_Arima()
}

\description{
e_Arima()

}

\examples{
e_Arima(0)

setwd('F:/dataEviews') 
library(readxl);library(strucchange);library(changepoint);library(r412) 
Adata<-read_excel('Eviews2_2.xlsx') 
# Adata<-dfclass::eviews2_2

gdp_sa<-ts(Adata$gdp_sa, start=c(1960,1), frequency=4) 
plot(gdp_sa) 

# Let's divide dataset
			 
e_sbreak() # Structural Change 설명
Result02 <- cpt.meanvar(dlog(gdp_sa), method = 'PELT') 

# we are going to use data after YR 2000
plot(Result02)

# 자료를 2000년 1분기부터 시작되는 자료로 지정
gdp_sa2<-window(gdp_sa, start=c(2000,1), frequency=4)

plot(gdp_sa)
			 
identt(gdp_sa2)

library(forecast)
ndiffs(gdp_sa2)

# Let's make stationary after first differencing

graphics.off() 
plot(diff(gdp_sa2)) 
identt(diff(gdp_sa2), 20) 

# include.drift -> 차분한 자료의 Arima에 상수항 포함. 2차 차분한 자료에는 상수항 없음
# CSS: Conditional Sum of Squares: 오차 초기값을 0으로 가정하고 잔차의 제곱합을 최소
# ML: Maximum Likelihood Estimation
# CSS-ML: CSS로 대략적인 시작 값을 찾고, ML로 정밀 계산

summary(Arima(gdp_sa2, order=c(1,1,0), include.drift=T, method='CSS') ) 
summary(Arima(gdp_sa2, order=c(0,1,1), seasonal=c(0,0,2), include.drift=T, method='ML')) 

R1 <- Arima(gdp_sa2, order=c(0,1,1),  # 1차 차분 / MA1모형
            seasonal=list(order=c(1,0,0), periods=12),  # AR1(12)모형
			include.drift=T,  # 상수항 포함
            fixed=c(NA,0,NA,NA), # 
			method='ML')) 

R2 <- Arima( log(gdp_sa), order=c(1,1,1), 
             seasonal=list(order=c(1,1,0), period=4),  # Seasonal(분기) 차분 및 적합지정: 
			 method='ML' )  
			  

summary(auto.arima(gdp_sa2, method='ML')) 

R1 <- Arima( (gdp_sa2), order=c(1,1,1), method='ML')  
checkresiduals(R1)  
qstat(R1)  
forecast(R1, h=5)  

R3 <- auto.arima( diff(log(gdp_sa2)), seasonal=FALSE, method='ML' )  
Simul <- arima.sim(model = list(ar=0.5, ma=0.3), n=100) 

}
