#' cc() Function
#' 
#' This function makes CCF graph
#' 

cc=function(x, y, lags=15 ) {
    n=length(x)
    r1=c(); r2=c() ; r3=c()
    r0=cc(x,y,0)
    for (k in 1:(lags+1)) {r1=c(r1, cc(y,x ,k)) }
    for (k in 1:lags){r2=c(r2, cc(x,y ,k)) }
    for(i in 1:lags) {r3[(lags+1)-i]<-r2[i] }
    tmp<-c(r3,r0,r1)

    barplot(tmp, ylab='CCF', xlab='Lags', ylim=c(-1,1),
             col='steelblue', border=NA, names.arg=(-lags):(1+lags) )
    box()
   abline(h=0)
    abline(h=2/sqrt(n), lty=3, col='blue')
    abline(h=-2/sqrt(n), lty=3, col='blue')
    names(tmp)=(-lags):(1+lags)
 }

