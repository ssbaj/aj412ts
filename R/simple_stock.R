simple_stock<-function(stock_data){

if (base::missing(stock_data)) {
	  cat("   ", '\n')
	  cat("  df<-getSymbols('139270.KS', from=sdate, end=edate, auto.assign=F) ", '\n')
      cat("  df2<-simple_stock(df) ", '\n')
      cat("  df3<-xts(df2$stockprice, order.by=df2$DATE) ", '\n')
      cat("  plot(df3) ", '\n')
      return( cat("   ", '\n') )
	}

df<-stock_data
df<-as.data.frame(df)
df<-df[ complete.cases(df), ]
df$DATE<-rownames(df)
df$DATE<-as.Date(df$DATE)
df<-df[,c(7, 4)]
colnames(df)<-c('DATE', 'stockprice')
df$stockreturn<-aj412s::percent_change(df$stockprice)
n<-nrow(df)
df<-df[(2:n), ]
df<-aj412s::mkindex(df)
rownames(df)<-df$INDEX

xtsdata <- xts(df$stockprice, order.by=df$DATE)  
plot(xtsdata)   

return(df)
}

