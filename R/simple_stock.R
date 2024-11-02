simple_stock<-function(stock_data){

if (base::missing(stock_data)) {
	  cat("  DATE는 as.Date로 자동 지정됨 ", '\n')
      cat("  NA미싱데이터는 자동을 삭제됨 ", '\n')
	  cat("  xtsdf <- xts(df$stockprice, order.by=df$DATE)  ", '\n')
	  return( cat("       plot(xtsdata, col = 'blue') ", '\n') )
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
df<-mkindex(df)
rownames(df)<-df$INDEX
return(df)
}

