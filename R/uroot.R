uroot <- function(x, nlag=4 , method=c("adf"), lag.short=TRUE, output=TRUE) {

if (base::missing(x)) {
            cat(' # How to use --------------------------   ', '\n')
	    return(cat("  uroot(데이터셋$변수, nlag=4, method=c('adf', 'pp', 'kpss'), lag.short=TRUE, out=TRUE) "))  }

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

return(stationary.test(x, nlag=nlag, method=method, lag.short=lag.short, output=output))
}

