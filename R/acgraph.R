#' ident() Function
#' 
#' This function draws ACF & PACF
#' 

acgraph=function(xdatas, lags=15){

if (base::missing(xdatas)) {
	 return(cat("  identt(df, lags:You decide or blank:automatic_allocation ) "))}


 tm_length<-length(xdatas)
 if(tm_length<=40) {lags<-round(tm_length*(2/3), 0)}
 else if(tm_length<=60) {lags<-round(tm_length*(1.2/2), 0)}
 else if(tm_length<=100) {lags<-round(tm_length*(0.9/2), 0)}
 else {lags<-40}


  r=c(); v=c()
  n=length(xdatas)
  for (k in 1:lags){
     r=c(r, ac(xdatas,k)) }
  
  barplot(r, col='steelblue', ylim=c(-1, 1), border=NA, xlab='LAG', ylab="ACF", names.arg=1:lags, main=c('Number of Data', length(xdatas)) )
  box()
  abline(h=0)
  abline(h=2/sqrt(n), lty=3, col='black')
  abline(h=-2/sqrt(n), lty=3, col='black')
return(r)
}
  