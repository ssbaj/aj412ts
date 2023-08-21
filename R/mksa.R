## Seasonal Adjusted Time Series data 만들기 --------------

mksa<-function(df, col_x, start_year, start_qm, qm ){

if (base::missing(df)) {
   cat(" df<-mksa(df, 6, 2008, 1, 12) ", '\n')
   return( cat("  df: dataset, 6=nth column number, 2008=year, 1=start month, 12=frequency ", '\n') ) }

cat('Required Library: dplyr/seasonal/forecast', '\n')
suppressPackageStartupMessages(library("dplyr"))
df<-as.data.frame(df)
ncolums<-ncol(df)
df[,col_x]<-ts(df[,col_x], start=c(start_year, start_qm), frequency=qm)
tmp<-df[,col_x]
fit<-tmp%>%seasonal::seas(x11='')
tmp<-forecast::seasadj(fit)
df<-cbind(df, tmp)
colnames(df)[ncolums+1] <- paste0( colnames(df)[col_x] , '_sa' , sep='')
return(df)
}


