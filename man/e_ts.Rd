\name{e_ts}
\alias{e_ts}
\title{Explaining: how to make time series data}
\usage{
e_ts()
}
\description{
How to make time series data
}
\examples{

# create Time Series Objects -------------
gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)

xts의 apply.quarterly() 사용 -------------
자료1<-xts(전산업생산지수$ind_m, order.by=전산업생산지수$date) 
자료2<-apply.quarterly(자료1, FUN=colMeans) 

xts를 ts로 전환 ------------- 
library(dsa)
tmp <- xts2ts(xtsdata) 

경제성장률(분기별 gdp) -------------
전년동기대비_GDP성장률 <- (gdp / dplyr::lag(gdp, 4) -1 )*100 


}