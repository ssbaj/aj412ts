\name{e_ecos}
\alias{e_ecos}
\title{Loading ECOS data}
\usage{
e_ecos()
}
\description{
Loading ECOS data
}
\examples{
## https://ecos.bok.or.kr/api/#/DevGuide/StatisticalCodeSearch
## https://ecos.bok.or.kr/api/#/AuthKeyApply
    
  library(ecos); library(xts); library(quantmod); library(xts); library(lubridate)
  my_key<-c('PDAG09WXHMB9IJ1RNMGX')
  sdate <- as.Date('2015-01-05')
  edate <-as.Date('2022-08-08')

  전산업생산지수<-statSearch(api_key=my_key, stat_code='901Y033', item_code1='A00', item_code2='2', cycle='M') 
  전산업생산지수<-전산업생산지수%>%select(time, data_value)%>%mutate(time=ym(time)) 
  names(전산업생산지수)<-c('date', 'ind_m') 
  전산업생산지수<-xts(전산업생산지수$ind_m, order.by=전산업생산지수$date) 
    
  분기별콜금리<-statSearch(api_key=my_key, stat_code='721Y001', item_code1='1020000', cycle='Q') 
  분기별콜금리<-분기별콜금리%>%select(time, data_value)%>%mutate(time=yq(time)) 
  names(분기별콜금리)<-c('date', 'call_q') 
    
  월별소비자물가지수<-statSearch(api_key=my_key,stat_code='901Y009',item_code1='0',cycle='M') 
  월별소비자물가지수<-월별소비자물가지수%>%select(time, data_value)%>%mutate(time=ym(time)) 
  names(월별소비자물가지수)<-c('date', 'cpi_m') 
  tscpi_m <- xts(월별소비자물가지수$cpi_m, order.by=월별소비자물가지수$date) 
  전년동기대비인플레이션 <- 100*(tscpi_m/stats::lag(tscpi_m, 12) -1 ) 
    
  M2평잔<-statSearch(api_key=my_key, stat_code='101Y003', item_code1='BBHS00', item_code2='2', cycle='M') 
  M2평잔<-M2평잔%>%select(time, data_value)%>%mutate(time=ym(time)) 
  names(M2평잔)<-c('date', 'M2_m') 

  경상수지<-statSearch(api_key=my_key, stat_code='301Y013', item_code1='000000', item_code2='2', cycle='M') 
  경상수지<-경상수지%>%select(time, data_value)%>%mutate(time=ym(time)) 
  names(경상수지)<-c('date', '경상수지') 

## --- xts패키지내 apply.monthly(), apply.quarterly(), apply.year() --- 
  Adata월별자료 <- apply.monthly(Adata, mean) 

## ------------------------------------------------------------------
  상해지수<-getSymbols('000001.ss', from=sdate, to=edate, auto.assign=F)
  선전지수<-getSymbols('399001.SZ', from=sdate, to=edate, auto.assign=F)
  홍콩항생지수<-getSymbols('^HSI', from=sdate, to=edate,	auto.assign=F)

  테슬라=getSymbols('TSLA', from=sdate, to=edate, auto.assign=F)
  테슬라<-테슬라[,4]
  names(테슬라) <- c('tesla')

  애플=getSymbols('AAPL', from=sdate, to=edate, auto.assign=F)
  애플<-애플[,4] 
  names(애플) <- c('apple') 

  삼성=getSymbols('005930.KS', from=sdate, to=edate, auto.assign=F) 
  삼성<-삼성[,4] 
  names(삼성) <- c('samsung') 
    
  현대=getSymbols('005380.KS', from=sdate, to=edate, auto.assign=F) 
  현대<-현대[,4] 
  names(현대) <- c('hyundai') 

}