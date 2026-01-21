\name{e_stock}
\alias{e_stock}
\title{Loading Stock data}
\usage{
e_stock()
}
\description{
Loading Stock data
}
\examples{
  
 library(quantmod); library(xts); library(lubridate); library(tsbox); library(aj412s); library(aj412ts)

## --- xts패키지내 apply.monthly(), apply.quarterly(), apply.year() --- 
  Adata월별자료 <- apply.monthly(Adata, mean) 

## --- tsbox내 ts_frequency()
  Adata월별자료 <- ts_frequency(Adata, 'year', sum) 

## ------------------------------------------------------------------
  상해지수<-getSymbols('000001.ss', from=sdate, to=edate, auto.assign=F)
  선전지수<-getSymbols('399001.SZ', from=sdate, to=edate, auto.assign=F)
  홍콩항생지수<-getSymbols('^HSI', from=sdate, to=edate,	auto.assign=F)

  테슬라=getSymbols('TSLA', from=sdate, to=edate, auto.assign=F)
  테슬라<-테슬라[,4]
  names(테슬라) <- c('tesla')

  애플=getSymbols('AAPL', from=sdate, to=edate, auto.assign=F)
  애플<-애플[,4] 
  names(애플) <- c('apple') 

  삼성=getSymbols('005930.KS', from=sdate, to=edate, auto.assign=F) 
  삼성<-삼성[,4] 
  names(삼성) <- c('samsung') 
    
  현대=getSymbols('005380.KS', from=sdate, to=edate, auto.assign=F) 
  현대<-현대[,4] 
  names(현대) <- c('hyundai') 

}