\name{e_hpfilter}
\alias{e_hpfilter}
\title{e_hpfilter}
\usage{
e_hpfilter()
}
\description{
e_hpfilter()
}
\examples{
e_hpfilter()
library(mFilter)
gdp_sa<-ts(Adata$gdp, start=c(1960,1), frequency=4)
gdp_sa<-window(gdp_sa, start=c(1985,1), frequency=4)
HPgdp_sa<-hpfilter(gdp_sa,freq = 1600)

NOTE: month=14400 // quarter=1600 // year=1000

plot(HPgdp_sa)
plot(HPgdp_sa$cycle, type='l', lty='twodash', col='red', lwd='2')
}
