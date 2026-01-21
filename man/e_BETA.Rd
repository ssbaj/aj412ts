\name{e_BETA}
\alias{e_BETA}
\title{e_BETA}
\usage{
e_BETA()
}
\description{
e_BETA()
}
\examples{

library(quantmod)
sdate <- as.Date('2015-01-05')
edate <-as.Date('2022-08-24')
Price=getSymbols('^GSPC', from=sdate, to=edate, auto.assign=F)
R <- dailyReturn(Price[,4])

## 25 day - Moving Average and Volatility
R_ma <- rollapply(R,25,mean)
R_mv <- rollapply(R,25,sd)
Price2 <- getSymbols('AAPL', from=sdate, to=edate, auto.assign=F)  
R2 <- dailyReturn(Price2[,4])
R12 <- merge(R,R2)

# 50day BETA and 250day BETA
Beta <- rollapply(R12,50, by.column = F,  function(x) lm(x[,2] ~ x[,1])[[1]][[2]] )
Beta <- rollapply(R12,250, by.column = F,  function(x) lm(x[,2] ~ x[,1])[[1]][[2]] )
plot(Beta)


}
