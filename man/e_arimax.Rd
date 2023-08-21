\name{e_arimax}
\alias{e_arimax}
\title{Transfer function}
\usage{
e_arimax()
}
\description{
Transfer function model
}
\examples{
library(TSA); library(astsa)
sales_140<-window(sales,end=140)
lead_140<-window(lead,end=140)

lead_140_Z <- lead_140 - mean(lead_140)
sales_140_D <- diff(sales_140)
lead_140_D <- diff(lead_140_Z)

mod <- arimax(
   sales_140_D,
   order=c(0,0,1),
   include.mean=TRUE,
   xtransf=lead_140_D,
   transfer=list(c(1,3)),
# 계수: MA1,상수항,AR1,MA0, MA1,MA2,MA3
   fixed=c(NA, NA,   NA,  0,   0,   NA, NA),
   method="ML")
}