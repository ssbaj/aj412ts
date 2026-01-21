#' pac() Function
#' 
#' This function calculates the partial autocorrelation.
#' 

pac<-function(x, k=1){ 
  n<-length(x)
  m<-mean(x)
  s<-sqrt(var(x))
  s<-as.vector(s)  ## s makes troubles
  z<-(x-m)/s
  y=c(z, rep(0,k))
  X=c(); b=c(1)
  if (k>0){
    for (i in 1:k){
      X=cbind( X, c(rep(0,i), z, rep(0,k-i) )) }
      bhat=solve(t(X)%*%X)%*%t(X)%*%y
      b=bhat[k] }
return(b)}

