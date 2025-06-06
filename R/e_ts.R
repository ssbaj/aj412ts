# defining ts

e_ts<-function(explaining=0){
if(explaining==0) {
cat("    ", '\n')
cat("# create Time Series Objects ------------- ", '\n')
cat("  gold <- ts(log(dat$gold),start=c(1993,1),frequency=12)  ", '\n')
cat("    ", '\n')
cat(" ggsubseriesplot(gold) ", '\n')
cat(" ggseasonplot(gdp_sa, year.labels=T, year.labels.left=T)  ", '\n')
cat("    ", '\n')
cat("# xts의 apply.quarterly() 사용 ------------- ", '\n')
cat(" \033[1;32m 자료1<-xts(전산업생산지수$ind_m, order.by=전산업생산지수$date)   \033[0m ", '\n')
cat(" \033[1;32m 자료2<-apply.quarterly(자료1, FUN=colMeans)  \033[0m ", '\n')
cat("    ", '\n')
cat("# xts를 ts로 전환 ------------- ", '\n')
cat("  library(dsa)  ", '\n')
cat("  tmp <- xts2ts(xtsdata)  ", '\n')
cat("    ", '\n')
cat("# 경제성장률(분기별 gdp) ------------- ", '\n')
cat(" \033[1;32m 전년동기대비_GDP성장률 <- (gdp / dplyr::lag(gdp, 4) -1 )*100    \033[0m ", '\n')
cat("    ", '\n')

}}
