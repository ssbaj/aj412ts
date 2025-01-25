uroot <- function(x, nlag=4, method=c("adf"), lag.short=TRUE) {

if (!require(aTSA)) {
    cat('Installing aTSA package for uroot .....', '\n')
    install.packages("aTSA")
  }
  
suppressPackageStartupMessages(library("aTSA"))

RE_uroot <- stationary.test(x, method=method, nlag=nlag, lag.short=lag.short)

return(RE_uroot)
}

