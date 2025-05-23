#' qstat() Function
#' 
#' This function prints photomanteau test statistics.
#' 

qstat<-function(xdatas, lags=15){

if (base::missing(xdatas)) {
	 return(cat("  qstat(df, lags:You decide or blank:automatic_allocation ) "))}

cat(" ", '\n')
cat("\033[1;34m------------------------------------------------------- \033[0m ", '\n')
cat('\033[1;34mQ.prob값이 0.05보다 크면 시계열은 Stationary시계열이다 \033[0m ', '\n')
cat("\033[1;34m------------------------------------------------------- \033[0m ", '\n')
cat(" ", '\n')
 tm_length<-length(xdatas)
 if(tm_length<=40) {lags<-round(tm_length*(2/3), 0)}
 else if(tm_length<=60) {lags<-round(tm_length*(1.2/2), 0)}
 else if(tm_length<=100) {lags<-round(tm_length*(0.9/2), 0)}
 else {lags<-40}



   Q.stat=c(); Q.prob=c()
   for(i in 1:lags){
   Temp.stat<-Box.test(xdatas, lag=i, type='Ljung-Box')$statistic
   Q.stat=c(Q.stat, Temp.stat)
   Temp.prob<-Box.test(xdatas, lag=i, type='Ljung-Box')$p.value
   Q.prob=c(Q.prob, Temp.prob)}
   Temp=as.matrix(rbind(Q.stat, Q.prob))
   Temp=t(Temp) ; Temp.number=c()
   for(j in 1:lags){
   Temp.number=c(Temp.number, j) }
   rownames(Temp)<-Temp.number ; print(Temp) }
