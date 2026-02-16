\name{prewhiten22}
\alias{prewhiten22}
\title{Make prewhitened data and ccf}
\usage{
prewhiten22()
}
\description{
Make prewhitened data and ccf
}
\examples{

library(aj412ts); library(TSA)
fit_x<-Arima(lead, order=c(0,0,1) )
prewhiten22(lead, sales, fit_x)

OR, if you want to get prewhitened data, x and y

prewhiten22(lead, sales, fit_x, showxy=TRUE)

}