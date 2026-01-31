uroot <- function(x, lag=4 , method=c("adf"), lag.short=FALSE, output=TRUE) {

if (base::missing(x)) {
cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", '\n')
cat(" \033[1;34m# TS자료: 연 -> 분기: library(tempdisagg) ", '\n')
cat(" \033[1;34m#                     td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette') ", '\n')
cat("    ", '\n')
cat(' # How to use --------------------------   ', '\n')
cat(" \033[1;33m   library(vars)   ", '\n')
cat(" \033[1;33m   VARselect(데이터셋$변수)   ", '\n')
return(cat("    uroot(데이터셋$변수, lag=4, method=c('adf', 'pp', 'kpss'), lag.short=TRUE, out=TRUE) "))  }


  if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
    cat('dplyr is necessary for this function','\n')
    cat('If an error occurs, connect to the network','\n')
    install.packages("dplyr")
  }
  

if (!require(aTSA)) {
    cat('Installing aTSA package for uroot .....', '\n')
    install.packages("aTSA")
  }
  
suppressPackageStartupMessages(library("aTSA"))

return(stationary.test(x, nlag=lag, method=method, lag.short=lag.short, output=output))
}

