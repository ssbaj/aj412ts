#' ac() Function
#' 
#' This function calculate autocorrelation
#' 

ac=function(x, k=1){
	n=length(x); m=mean(x)
	c1=x[1:(n-k)]-m
	c2=x[(k+1):n]-m
	r=sum(c1*c2)/sum((x-m)^2)
	return(r) }
