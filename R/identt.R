# identt() Function
#
# This function draws ACF & PACF
# 

identt<-function(xdatas, lagxx=15){

if (base::missing(xdatas)) {
	 return(cat("  identt(df, lag:You decide or blank:automatic_allocation ) "))}


 tm_length<-length(xdatas)
 if(tm_length<=40) {lagx<-round(tm_length*(2/3), 0)}
 else if(tm_length<=60) {lagx<-round(tm_length*(1.2/2), 0)}
 else if(tm_length<=100) {lagx<-round(tm_length*(0.9/2), 0)}
 else {lagx<-40}

if (base::missing(lagxx)) { lagx<-lagx } else { lagx<-lagxx }

 par(mfrow=c(1,2))
 acff(xdatas, lagx)
 pacff(xdatas, lagx) 
 par(mfrow=c(1,1))
 
 }

