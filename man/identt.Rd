\name{identt}
\alias{identt}
\title{Making: ACF PACF graph}
\usage{
identt()
}
\description{
Making ACF PACF graph
}
\examples{

## identification of data
library(readxl); library(aj412)
Adata<-read_excel('Eviews2.xlsx')
gdp_sa<-ts(Adata$gdp_sa, start=c(1960,1), frequency=4)
gdp_sa<-window(gdp_sa, start=c(1980,1), end=c(2016,2))
identt(Adata$gdp_sa)
}