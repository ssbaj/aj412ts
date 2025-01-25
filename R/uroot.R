uroot <- function(x, nlag=4, method=c("adf"), lag.short=TRUE, out=TRUE) {

if (base::missing(x)) {
            cat(' # How to use --------------------------   ', '\n')
	    return(cat("  uroot(df$cs, nlag=4, method=c('adf', 'pp', 'kpss'), lag.short=TRUE, out=TRUE) "))  }


if (!require(aTSA)) {
    cat('Installing aTSA package for uroot .....', '\n')
    install.packages("aTSA")
  }
  
suppressPackageStartupMessages(library("aTSA"))

return(stationary.test(x, method=method, nlag=nlag, lag.short=lag.short, out=out))
}

