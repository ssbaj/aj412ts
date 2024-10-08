#e_ecos

e_ecos<-function(explaining=0){
if(explaining==0) {
cat('\014')
cat("#------------------------------------------------------ ", '\n')
cat("    ", '\n')
cat("# apkey 다운로드 받는 곳 ", '\n')
cat("# https://ecos.bok.or.kr/api/#/AuthKeyApply ", '\n')
cat("  ", '\n')
cat("# ECOS자료 Code Book ", '\n')
cat("  https://ecos.bok.or.kr/api/#/DevGuide/StatisticalCodeSearch ", '\n')
cat("  ", '\n')
cat(" library(ecos); library(dplyr); library(aj412s); library(aj412ts)", '\n')
cat(" library(xts); library(mFilter)", '\n')
cat(" ecos.setKey(api_key = '') ", '\n')
cat("  ", '\n')
cat(" gdp분기 <- statSearch(stat_code='200Y004', item_code1='1400', cycle='Q') # 분기별 GDP   ", '\n')
cat(" gdp <- simple_ecos(gdp분기, '2010') # 분기별자료를 2010년부터 현재까지 솎아내기 ", '\n')
cat(" gdp <- mksad(gdp, 1, 3) # X11으로 계절조정. 1=DATE컬럼번호, 3=data컬럼번호 ", '\n')
cat(" # X11으로 계절조정된 자료는 gdp$x11_sa에 저장됨 ", '\n')
cat("  ", '\n')
cat(" library(mFilter)  ", '\n')
cat(" # gdp$x11_sa자료를 hp필터를 사용해 gdp의 cycle과 trend를 분리 ", '\n')
cat(" hp.gdp <- hpfilter(gdp$x11_sa,freq = 1600)  ", '\n')
cat(" trend<-as.vector(hp.gdp$trend) # trend솎아내기 ", '\n')
cat(" cycle<-as.vector(hp.gdp$cycle) # cycle솎아내기 ", '\n')
cat(" gdp<-cbind(gdp, cycle, trend) # 데이터 묶어내기 ", '\n')
cat("  ", '\n')
cat(" # 연이율로 변경한 잠재성장률 ", '\n')
cat(" gdp$pc_annual <- ((gdp$trend/lag(gdp$trend,1))^4 - 1)*100  ", '\n')
cat("  ", '\n')
cat(" # 경기변동 비지니스 사이틀 그래프 ", '\n')
cat(" xtscycle <- xts(gdp$cycle, order.by=gdp$DATE)   ", '\n')
cat(" plot(xtscycle, main = '월별자료', col = 'blue')   ", '\n')
cat("  ", '\n')
cat(" # 잠재성장률 그래프 ", '\n')
cat(" xtspc_annual <- xts(gdp$pc_annual, order.by=gdp$DATE) ", '\n')
cat(" plot(xtspc_annual, main = '월별자료', col = 'blue') ", '\n')
cat("  ", '\n')
cat("\033[1;31m# xts패키지: apply.monthly(), apply.quarterly(), apply.year() --- ", '\n')
cat("\033[1;32m# xts의 apply.quarterly() 사용예제 ------------- \033[0m  ", '\n')
cat(" 전산업생산지수 <- simple_ecos(전산업생산지수, '2015') # Monthly자료를 Quarterly자료로 전환하자", '\n')
cat(" TMP <- xts(전산업생산지수$data, order.by=전산업생산지수$DATE) ", '\n')
cat(" 전산업생산지수 자료는 \033[1;31m'전산업생산지수$data'\033[0m   ", '\n')
cat(" apply.yearly( TMP[, 1], FUN=colMeans ) ", '\n')
cat("    ", '\n')
cat("\033[1;32m# xts를 ts로 전환 -------------   \033[0m", '\n')
cat("  library(dsa)  ", '\n')
cat("  tmp <- xts2ts(xtsdata)  ", '\n')
cat("  ", '\n')
invisible(readline(prompt="Press [enter] to continue"))
cat("    ", '\n')
cat("\033[1;32mCreate Time Series Objects ------------- \033[0m ", '\n')
cat(" CD91일수익률   <- statSearch(stat_code='721Y001', item_code1='2010000', cycle='M')   ", '\n')
cat(" 국채10년수익률 <- statSearch(stat_code='721Y001', item_code1='5050000', cycle='M')   ", '\n')
cat(" CP91일수익률   <- statSearch(stat_code='721Y001', item_code1='4020000', cycle='M')   ", '\n')
cat(" Call금리 <- statSearch(stat_code='721Y001', item_code1='1020000', cycle='Q') ", '\n')
cat("  ", '\n')
cat(" gdp연간 <- statSearch(stat_code='200Y106', item_code1='1400', cycle='A') # 연간 GDP  ", '\n')
cat(" gdp분기 <- statSearch(stat_code='200Y004', item_code1='1400', cycle='Q') # 분기별 GDP  ", '\n')
cat(" 한국경제성장률 <- statSearch(stat_code='902Y015', item_code1='KOR', cycle='Q') #한국경제성장률 ", '\n')
cat(" 중국경제성장률 <- statSearch(stat_code='902Y015', item_code1='CHN', cycle='Q') #중국경제성장률 ", '\n')
cat(" 일본경제성장률 <- statSearch(stat_code='902Y015', item_code1='JPN', cycle='Q') #일본경제성장률 ", '\n')
cat(" 미국경제성장률 <- statSearch(stat_code='902Y015', item_code1='USA', cycle='Q') #미국경제성장률 ", '\n')
cat("  ", '\n')
cat(" 월별소비자물가지수 <- statSearch(stat_code='901Y009',item_code1='0',cycle='M') ", '\n')
cat(" 전년동기대비인플레이션 <- 100*(tscpi_m/stats::lag(tscpi_m, 12) -1 ) ", '\n')
cat(" M2평잔 <- statSearch(stat_code='101Y003', item_code1='BBHS00', cycle='M') ", '\n')
cat(" 경상수지 <- statSearch(stat_code='902Y009', item_code1='KR', cycle='Q') ", '\n')
cat(" 소비자물가지수 <- statSearch(stat_code='901Y009', item_code1='0', cycle='M')  ", '\n')
cat(" 제조업재고율 <- statSearch(stat_code='901Y026', item_code1='I33A', cycle='M')  ", '\n')
cat(" 전산업생산지수 <- statSearch(stat_code='901Y033', item_code1='A00', item_code2='2', cycle='M') ", '\n')
cat(" 노동생산성 <- statSearch(stat_code='901Y107', item_code1='A', item_code2='I70B', cycle='Q')  # 산업생산기준/ 제조업 ", '\n')
cat("  ", '\n')
invisible(readline(prompt="Press [enter] to continue"))
cat("  ", '\n')
cat(" 국내건설수주액 <- statSearch(stat_code='901Y020', item_code1='I42ABAA', cycle='M') # 건축.주택 ", '\n')
cat(" 건설기성액 <- statSearch(stat_code='901Y104', item_code1='I48ABA', item_code2='I37C', cycle='M') # 건축.계절조정 ", '\n')
cat(" 건축착공현황 <- statSearch(stat_code='901Y103', item_code1='2', item_code2='I47ABA', cycle='M') # 동수.주거용 ", '\n')
cat(" 건축허가현황 <- statSearch(stat_code='901Y037', item_code1='I43ABA', item_code2='2', cycle='M') # 동수.주거용 ", '\n')
cat("  ", '\n')
cat(" 미분양주택현황_전국 <- statSearch(stat_code='901Y074', item_code1='I410A', cycle='M') # 전국 ", '\n')
cat(" 미분양주택현황_수도권 <- statSearch(stat_code='901Y074', item_code1='I410R', cycle='M') # 수도권 ", '\n')
cat(" 미분양주택현황_서울 <- statSearch(stat_code='901Y074', item_code1='I410B', cycle='M') # 서울 ", '\n')
cat(" 미분양주택현황_경기 <- statSearch(stat_code='901Y074', item_code1='I410I', cycle='M') # 경기 ", '\n')
cat(" 미분양주택현황_인천 <- statSearch(stat_code='901Y074', item_code1='I410E', cycle='M') # 인천 ", '\n')
cat("  ", '\n')
cat(" 주택건설인허가실적_전국 <- statSearch(stat_code='901Y105', item_code1='ALL', cycle='M') # 전국 ", '\n')
cat(" 주택건설인허가실적_서울 <- statSearch(stat_code='901Y105', item_code1='SEO', cycle='M') # 서울 ", '\n')
cat(" 주택건설인허가실적_경기 <- statSearch(stat_code='901Y105', item_code1='GYE', cycle='M') # 경기 ", '\n')
cat(" 주택건설인허가실적_인천 <- statSearch(stat_code='901Y105', item_code1='INC', cycle='M') # 인천 ", '\n')
cat("  ", '\n')
invisible(readline(prompt="Press [enter] to continue"))
cat("  ", '\n')
cat("\033[1;31m전산업생산지수를 xts 그래프로 보기 ----------- \033[0m ", '\n')
cat(" 전산업생산지수 <- simple_ecos(전산업생산지수, '2015') ", '\n')
cat("\033[1;32m# simple_ecos()을 사용하면 데이터셋의 data가 전산업생산지수. yoy도 데이터셋에 저장됨 \033[0m ", '\n')
cat(" tmp_data <- xts(전산업생산지수$data, order.by=전산업생산지수$DATE) ", '\n')
cat(" plot(tmp_data, main = '월별자료', col = 'blue') ", '\n')
cat("\033[1;31m전산업생산지수를 ggplot2 그래프로 보기 ----------- \033[0m ", '\n')
cat(" ggplot(전산업생산지수, aes(x=DATE, y=data)) + geom_line() + theme_bw() ", '\n')
cat("  ", '\n')
cat("\033[1;32m경제지수 및 주가 ----------- \033[0m ", '\n')
cat(" sdate <- as.Date('2018-01-01') ", '\n')
cat(" edate <- as.Date('2024-08-25') ", '\n')
cat(" 코스피지수 <- getSymbols('^KS11', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 상해지수 <- getSymbols('000001.ss', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 선전지수 <- getSymbols('399001.SZ', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 홍콩항생지수 <- getSymbols('^HSI', from=sdate, to=edate,	auto.assign=F) ", '\n')
cat(" 테슬라 <- getSymbols('TSLA', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 애플 <- getSymbols('AAPL', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 삼성 <- getSymbols('005930.KS', from=sdate, to=edate, auto.assign=F) ", '\n')
cat(" 현대 <- getSymbols('005380.KS', from=sdate, to=edate, auto.assign=F) ", '\n')
cat("  ", '\n')
cat(" autoplot(상해지수[, '000001.SS.Close']) ", '\n')
cat(" autoplot(테슬라[,'TSLA.Close']) ", '\n')
cat("  ", '\n')
invisible(readline(prompt="Press [enter] to continue"))
cat("  ", '\n')
cat(" # xts로 상해지수/테슬라 그래프 ------------ ", '\n')
cat(" Example1 <- c(PDAG09) ", '\n')
cat(" Example2 <- c(WXHMB9) ", '\n')
cat(" Example3 <- c(IJ1RNMGX) ", '\n')
cat(" 상해지수 <- as.data.frame(상해지수); 상해지수$DATE <- rownames(상해지수) ", '\n')
cat(" 상해지수$DATE <- as.Date(상해지수$DATE); colnames(상해지수)[4] <- 'data'  ", '\n')
cat(" tmp <- xts(상해지수$data, order.by=상해지수$DATE) ", '\n')
cat(" plot(tmp, col='red') ", '\n')
cat("  ", '\n')

}}
