\name{e_arima}
\alias{e_arima}
\title{e_arima}
\usage{
e_arima()
}

\description{
e_arima()

}

\examples{
e_arima(0)

setwd('F:/dataEviews') 
library(readxl);library(forecast);library(strucchange);library(changepoint);library(r412) 
Adata<-read_excel('Eviews2_2.xlsx') 
gdp_sa<-ts(Adata$gdp_sa, start=c(1960,1), frequency=4) 
plot(gdp_sa) 

# Let's divide dataset
			 
e_structuralbreak(0) 
Result02 <- cpt.meanvar(dlog(gdp_sa), method = 'PELT') 

# we are going to use data after YR 2000
plot(Result02)

gdp_sa2<-window(gdp_sa, start=c(2000,1), frequency=4)
plot(gdp_sa)
			 
ident(gdp_sa2)
ndiffs(gdp_sa2)

# Let's make stationary after first differencing

graphics.off() 
plot(diff(gdp_sa2)) 
ident(diff(gdp_sa2), 33) 
			 
summary(Arima(gdp_sa2,order=c(1,1,0),include.drift=T,method='CSS') ) 
summary(Arima(gdp_sa2, order=c(0,1,1), seasonal=c(0,0,2),include.drift=T, method='ML')) 
summary(Arima(gdp_sa2,order=c(0,1,1),seasonal=c(0,0,2),fixed=c(NA,0,NA,NA),include.drift=T,method='ML')) 
summary(auto.arima(gdp_sa2, method='ML')) 

R1 <- Arima( (gdp_sa2), order=c(1,1,1), method='ML')  
checkresiduals(R1)  
qstat(R1)  
forecast(R1, h=5)  
R2 <- Arima( log(gdp_sa), order=c(1,1,1), 
             seasonal=list(order=c(1,1,0), period=4), method='ML' )  
R3 <- auto.arima( diff(log(gdp_sa2)), seasonal=FALSE, method='ML' )  
Simul <- arima.sim(model = list(ar=0.5, ma=0.3), n=100) 

}
