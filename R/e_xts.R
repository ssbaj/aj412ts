#' e_xts() Function
#' 

e_xts<-function(explaining=0){
if(explaining==0) {
cat(" ", '\n')
cat(" ", '\n')
cat("\033[1;31m# xts로 Ecos자료를 그래프로 그려보기 ----------------- \033[0m ", '\n')
cat(" ", '\n')
cat(" library(ecos); library(xts); library(aj412s); library(aj412ts)  ", '\n')
cat(" ecos.setKey(api_key = '') ", '\n')
cat(" set_data<-c('PDA G09W ELCA88 XHMB9I J1RNMGX') ", '\n')
cat(" gdp <- statSearch(stat_code='200Y004', item_code1='1400', cycle='Q')    ", '\n')
cat(" ", '\n')
cat("\033[1;31m df<-simple_ecos(ecos자료, '시작연도', '끝연도' )  \033[0m ", '\n')
cat("\033[1;31m 또는, df<-simple_ecos(ecos자료, '시작연도' )  \033[0m", '\n')
cat("  ", '\n')
cat(" df<-simple_ecos(gdp, '2010', '2019' ) ", '\n')
cat("  ", '\n')
cat("\033[1;32m simple_ecos()으로 만들어진 df에서 \033[0m", '\n')
cat("\033[1;32m df$data가 gdp임. df$yoy도 dataset에 저장됨 \033[0m ", '\n')
cat("\033[1;32m 변수: DATE, time(원자료의 time변수), data, yoy \033[0m", '\n')
cat("  ", '\n')
cat(" xtsdata <- xts(df$data, order.by=df$DATE) ", '\n')
cat(" plot(xtsdata, main = '월별자료', col = 'blue')   ", '\n')
cat(" ", '\n')
}
}