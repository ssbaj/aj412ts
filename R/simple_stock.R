simple_stock<-function(stock_data){
df<-stock_data
df<-as.data.frame(df)
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

