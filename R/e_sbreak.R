e_sbreak<-function(explaining=0){

if(explaining==0) {

cat(" 			 ", '\n')
cat(" library(aj412s2); library(aj412ts); library(svars) ", '\n')
cat(" library(strucchange);library(changepoint); library(tidyverse) ", '\n')
cat(" library(lubridate) ", '\n')
cat(" 			 ", '\n')
cat(" ", '\n')
cat("\033[1;31m# 시계열 자료의 기간 변경 ----------------- \033[0m ", '\n')
cat("  이중식. Chapter 7 and Chapter 5    ", '\n')
cat("  내장명령문 aggregate를 이용한 시계열 자료의 변경: 월 -> 분기    ", '\n')
cat("  TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3    ", '\n')
cat("  tempdisagg를 이용한 시계열 자료의 변경: 연 -> 분기    ", '\n')
cat("  TS자료: 연 -> 분기: library(tempdisagg)   ", '\n')
cat("                      td1<-td(cs~1, to='quarterly', converstion='last', method='denton-cholette')   ", '\n')
cat("\033[1;31m# ----------------- \033[0m ", '\n')
cat(" ", '\n')
cat(" Result01 <- cpt.meanvar(Adata$growth, method = 'PELT')", '\n')
cat(" plot(Result01, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" change_point_indices <- cpts(Result01)", '\n')
cat(" df %>% slice(change_point_indices)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')
cat(" Result02 <- cpt.mean(Adata$growth, penalty = 'BIC', method = 'PELT')", '\n')
cat(" plot(Result02, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" cpts(Result02)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')
cat(" Result03 <- Fstats(tmp$dlgni ~ lag(tmp$dlm2), data=tmp)  ", '\n')
cat(" breakpoints(Result03)  ", '\n')
cat(" plot(Result03) ", '\n')
cat(" 			 ", '\n')
cat(" cusum <- efp(growth ~ lag(growth), type = 'OLS-CUSUM', data = Adata)", '\n')
cat(" plot(cusum)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')

}

}
