uroot <- function(x, nlag=4, method=c("adf")) {

if (!require(aTSA)) {
    cat('Installing aTSA package for uroot .....', '\n')
    install.packages("aTSA")
  }
  
suppressPackageStartupMessages(library("aTSA"))

RE_uroot <- stationary.test(x, method=method, nlag=nlag)

return(RE_uroot)
}

