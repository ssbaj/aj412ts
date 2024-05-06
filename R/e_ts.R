# defining ts

e_ts<-function(explaining=0){
if(explaining==0) {
cat("    ", '\n')
cat("# create Time Series Objects  ", '\n')
cat("  gold <- ts(log(dat$gold),start=c(1993,11),frequency=12)  ", '\n')
cat("  silver <- ts(log(dat$silver),start=c(1993,11),frequency=12)  ", '\n')
cat("  plat <- ts(log(dat$plat),start=c(1993,11),frequency=12)  ", '\n')
cat("  pall <- ts(log(dat$pall),start=c(1993,11),frequency=12)  ", '\n')
}}
