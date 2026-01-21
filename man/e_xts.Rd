\name{e_xts}
\alias{e_xts}
\title{how to make xts-type graph}
\usage{
ac()
}
\description{
Making xts-type graph
}
\examples{

 library(ecos); library(xts); library(aj412ts)   
 ecos.setKey(api_key = 'PDAG09WELCA88XHMB9IJ1RNMGX')  
 gdp <- statSearch(stat_code='200Y004', item_code1='1400', cycle='Q')     
  
 df<-simple_ecos(ecos자료, '시작연도', '끝연도' )    
 또는, df<-simple_ecos(ecos자료, '시작연도' )   
   
 df<-simple_ecos(gdp, '2010', '2019' )  
   
 simple_ecos()으로 만들어진 df에서  
 df$data가 gdp임. df$yoy도 dataset에 저장됨   
 변수: DATE, time(원자료의 time변수), data, yoy  
   
 xtsdata <- xts(df$data, order.by=df$DATE)  
 plot(xtsdata, main = '월별자료', col = 'blue')    

}