#' ccgraph() Function
#' 
#' This function makes CCF graph
#' 

ccgraph=function(x,y,lags=15){
	n=length(x)
	r=c()
	for (k in 1:lags){r=c(r, cc(x,y,k)) }
	barplot(r, ylab='CCF', xlab='LAG', ylim=c(-1,1),
	         col='steelblue', border=NA, names.arg=1:lags)
	box()
	abline(h=0)
	abline(h=2/sqrt(n), lty=3, col='blue')
	abline(h=-2/sqrt(n), lty=3, col='blue')
	names(r)=1:lags
	return(r) }
