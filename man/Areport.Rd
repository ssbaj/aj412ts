\name{Areport}
\alias{Areport}
\title{Reporting Arima}
\usage{
Areport()
}
\description{
Reporting Arima
}
\examples{
setwd('F:/dataEviews')
library(readxl);library(forecast);library(strucchange);library(changepoint);library(r412)
Adata<-read_excel('Eviews2_2.xlsx')
gdp_sa<-ts(Adata$gdp_sa, start=c(1960,1), frequency=4)

Result1 <- auto.arima(gdp_sa, method='ML')
Areport(Result1)

Result2<-Arima(gdp_sa, order=c(0,1,2), method='ML')
Areport(Result2)
}