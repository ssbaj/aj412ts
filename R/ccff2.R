#' ccff2() Function
#' 
#' This function makes CCF graph
#' 

ccff2=function(x,y,lags=15){

cc=function(x,y,k=1){
                    n=length(x)
                    mx=mean(x); my=mean(y)
                    sx=sqrt(sum((x-mx)^2));
                    sy=sqrt(sum((y-my)^2))
                    if(k==0)  {x=x[1:n];y=y[1:n] } else{x=x[1:(n-k)] ; y=y[(k+1):n] }
                    cxy=sum((x-mx)*(y-my))
                    cxy/(sx*sy)
                }


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
