#' dlog() Function
#' 
#' Taking log then differencing data
#' 

dlog<-function(x){
  
if (base::missing(x)) {
	 return(cat("  if class(x)='ts', dlog(x)=ts , else dlog(x)=c(NA, ...) "))}
	 
  
  if(class(x)=="ts") {
  tmp_ts<-diff(log(x))
  return(tmp_ts)
  }
  
  
  else{  
  tmp_k<-0
  
  for(i in 1:length(x)){
  if(is.na(x[i]) & !is.na(x[i+1])  ) {
  tmp_k <- (i+1)
  break
  }}
  
  
  if(tmp_k>0) {
  tmp_x<-x[tmp_k:length(x)]
  logx<-log(tmp_x)
  m2<- logx[-1]-logx[-length(tmp_x)]
  return( c(NA[1:tmp_k], m2) )
  }

    
  if(tmp_k==0) {
  logx<-log(x)
# return( c(NA, (logx[-1]-logx[-length(x)])) )
  return( (logx[-1]-logx[-length(x)]) )
  }

} }
