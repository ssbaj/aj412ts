#' cc() Function
#' 
#' This function makes CCF graph
#' 

ccff=function(x, y, lags=15 ) {

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

