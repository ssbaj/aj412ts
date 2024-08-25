simple_ecos<-function(ecos_dataset, start_year, cycle=1 ){


if (base::missing(ecos_dataset)) {
	  cat("  Data는 순수한 numeric이고 ts지정은 않함 ", '\n')
      cat("  ECOS자료에서 원하는 연도부터 자료 추출 / YoY첨부 ", '\n')
	  cat("  2018년 부터 시작하는 자료만 남길 경우 ", '\n')
	  cat("  Monthly/Quarterly/Yearly자료는 자동인식함", '\n')
	  return( cat("  Adata<-simple_ecos(gdp, '2018') ", '\n') )
	}

## mkdate_series()함수 -------------------------
mkdate_series<-function(df, start_y, start_m, mq ){

if (base::missing(df)) {
	    cat("  df<-as.data.frame(df) ", '\n')
		cat("  Input date format: 연도=2015, 시작 월=5, 월별자료=12 or 분기별자료=4  ", '\n')
		return(cat("  df<-mkdate_series(df, 2015, 5, 12)   ") )  }

df<-as.data.frame(df)
n<-nrow(df)
tmp1<-rep(NA,n)
tmp2<-rep(NA,n)
tmp3<-rep(NA,n)

if(mq==12) {    ## monthly data
for(i in 1:n){
if(start_m==12) 
{tmp2[i]<-start_m
start_m<-1 
tmp1[i]<-start_y
start_y<-start_y+1}
else{
tmp2[i]<-start_m
start_m<-start_m+1
tmp1[i]<-start_y
} }

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}


if(mq==4) {  #quarterly data
for(i in 1:n){
if(start_m==10) 
{tmp2[i]<-start_m
start_m<-1
tmp1[i]<-start_y
start_y<-start_y+1}
else{
tmp2[i]<-start_m
start_m<-start_m+3
tmp1[i]<-start_y
} }

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}


for (i in 1:n){
tmp3[i]<-paste0( tmp1[i], '-',tmp2[i], '-01', sep='')
}

DATE<-as.Date(tmp3)

df<-cbind(df, DATE)
return(df)

}

## mkdate_series() 끝 --------------------------


if(class(start_year)=="numeric") {
  cat("Start year should be character format such as '2018' ", '\n')
  break
  }

if (!require(dplyr)) {
    cat('Installing dplyr package','\n')
	install.packages("dplyr")
  }


suppressPackageStartupMessages(library("dplyr"))
df<-as.data.frame(ecos_dataset)

df <- df %>% select("time","data_value")
n<-nrow(df)

for(i in 1:n){
tmp_time<-as.character(substring(df$time[i], 1, 4))
if(tmp_time == start_year) {
  nx<-i; break }
  }

## Yearly/Quarterly/Monthly데이터를 인식하는 코드
for(i in 1:2){
tmp_time<-as.character(substring(df$time[i], 5, 5))
if(tmp_time == "") {
  break }
else if(tmp_time == "0") {
  cycle<-12; break }
else if(tmp_time == "Q") {
  cycle<-4; break }
  }

## 원하는 연도부터 데이터를 추출하는 코드
df<-df[c(nx:n), ]
colnames(df)<-c('time', 'data')

## yoy의 계산
yoy <- diff(df$data, lag = cycle)

## percent of yoy의 계산
tmp<-na.omit(lag(df$data, cycle))
yoy_pct<-yoy/tmp*100

yNA<-rep(NA, cycle)
yoy<-c(yNA, yoy)
yoy_pct<-c(yNA, yoy_pct)
df<-cbind(df, yoy, yoy_pct)

start_year<-as.numeric(start_year)

if(cycle!=1) {
  df<-mkdate_series(df, start_year, 1, cycle )
  df<-df%>%relocate(DATE, .before='time')
  }

return(df)
}

