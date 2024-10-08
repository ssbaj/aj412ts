#' Plot leading and lagging correlations
#' Source: https://github.com/KevinKotze/tsm/blob/master/man/leadlag.Rd
#' @param x1 A vector of numeric values
#' @param x2 A vector of numeric values
#' @param nlag A numberic scalar for the lag-order of the autoregressive (cyclical) part
#' @return Values for the stochastic trend and the stationary cycle
#' @examples

leadlag  <- function(x1,x2,nlag){

if (base::missing(x1)) {
	cat("\033[1;33m # ------------------------ \033[0m ", '\n')
	cat(" X<-c(1,2,3,4,5,6,7,8,9) ", '\n') 
	cat(" Y<-c(1,2,3,4,5,6,7,8,9) ", '\n') 
	cat(" leadlag(X, Y, nlag=3) ", '\n') 
	cat("  ", '\n')
	cat("\033[1;33m Index(nlag=3): 3 2 1 0 -1 -2 -3  \033[0m ", '\n') 
	cat("\033[1;34m 1 번째index(X선행, Y후행): 1 2 3 4 5 6 VS 4 5 6 7 8 9  \033[0m ", '\n') 
	cat("\033[1;34m 2 번째index(X선행, Y후행): 1 2 3 4 5 6 7 VS 3 4 5 6 7 8 9  \033[0m ", '\n') 
	cat("\033[1;34m 3 번째index(X선행, Y후행): 1 2 3 4 5 6 7 8 VS 2 3 4 5 6 7 8 9  \033[0m ", '\n') 
	cat("\033[1;31m 4 번째index(X현행, Y현행): 1 2 3 4 5 6 7 8 9 VS 1 2 3 4 5 6 7 8 9  \033[0m ", '\n') 
	cat(" 5 번째index(X후행, Y선행): 2 3 4 5 6 7 8 9 VS 1 2 3 4 5 6 7 8  ", '\n') 
	cat(" 6 번째index(X후행, Y선행): 3 4 5 6 7 8 9 VS 1 2 3 4 5 6 7  ", '\n') 
	cat(" 7 번째index(X후행, Y선행): 4 5 6 7 8 9 VS 1 2 3 4 5 6  ", '\n') 
	cat("\033[1;32m 상관계수: [1] 1 1 1 1 1 1 1 \033[0m ", '\n')
	cat("\033[1;33m # ------------------------ \033[0m ", '\n') 
	return( cat("  ", '\n') )
}


  # empty output matrix
  x <- rep(NaN,nlag*2+1)
  p <- x
  # make index to get the correct lead and lag structure
  index <- seq(nlag,-(nlag),-1)
  
  for (i in 1:(nlag*2+1)) {
    if (index[i]==0) {
	  xi <- cor(x1, x2)
    } else if (index[i]>0) {
	  xi <- cor(x1[1:(length(x1)-index[i])], x2[(index[i]+1):length(x2)])
    } else if (index[i]<0) {
      xi <- cor(x1[(1-index[i]):length(x1)], x2[1:(length(x2)+index[i])])
    }
    x[i] <- xi
  }
  return(x)
}
