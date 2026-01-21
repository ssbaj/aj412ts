simple_stock<-function(stock_data){

if(base::missing(stock_data)) {
cat("  library(aj412s); library(aj412ts); library(quantmod); library(xts) ", '\n')
cat("  dflist<-openxlsx('KRX ETF 전종목 기본정보.xlsx', header=T) ", '\n')
cat("  n<-nrow(dflist) ", '\n')
cat("   ", '\n')
cat("  for(i in 1:n){ ", '\n')
cat("  word1<-dflist[i, 2] ", '\n')
cat("  word2 <- '.KS' ", '\n')
cat("  combined <- paste(word1, word2, sep = '') ", '\n')
cat("  word3_title<-dflist[i, 3] ", '\n')
cat("  sdate<-as.Date('2019-01-03'); edate<-as.Date('2024-10-31') ", '\n')
cat("  df<-getSymbols(combined, from=sdate, end=edate, auto.assign=F) ", '\n')
cat("  df2<-simple_stock(df) ", '\n')
cat("  plot(df2$DATE, df2$stockprice, main = paste(word3_title, ', Index: ', i) , type='l' )   ", '\n')
cat("  readline(prompt = 'Press Enter to continue...') ", '\n')
cat("  } ", '\n')
return(cat("      ") ) }

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

