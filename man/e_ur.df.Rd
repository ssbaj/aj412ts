\name{e_ur.df}
\alias{e_ur.df}
\title{e_ur.df}
\usage{
e_ur.df()
}
\description{
e_ur.df()
}
\examples{

library(aTSA); library(urca)
adf.test(dlog(gdp_sa))  # aTSA명령문
adftest<-ur.df(dlog(gdp_sa), type=c('none'),  selectlags=c('BIC'))
dYt = r*Yt-1 +e
tau2 : r=0
----------------------------------------------------------- 
ur.df(dlog(gdp_sa), type=c('drift'),  selectlags=c('BIC'))
dYt = a0 + r*Yt-1 +e
tau2 : r=0
phi1 : a0=r=0
-----------------------------------------------------------
adftest<-ur.df(dlog(gdp_sa), type=c('trend'),  selectlags=c('BIC'))
dYt = a0 + r*Yt-1 + a2*trend + e
tau3 : r=0
phi3 : r=a2=0
phi2 : a0=r=a2=0
}
