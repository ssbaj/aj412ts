simple_ecos <- function(ecos_dataset, start_year, end_year="9999"){

if (base::missing(ecos_dataset)) {
	  cat("  Data는 순수한 numeric이면 충분함. ts지정은 할 필요 없음 ", '\n')
      cat("  ECOS자료에서 원하는 연도부터 자료 추출 / QoQYoY자료가 자동 첨부됨 ", '\n')
	  cat("  Monthly/Quarterly/Yearly자료는 자동인식함", '\n')
	  cat("  예제1: 2018년 부터 시작하는 자료만 남길 경우 ", '\n')
	  cat("         Adata<-simple_ecos(gdp, '2018') ", '\n')
	  cat("  예제1: 2018년~2020년 자료만 남길 경우 ", '\n')
	  return( cat("       Adata<-simple_ecos(gdp, '2018', '2020') ", '\n') )
	}

if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
    cat('dplyr is necessary for this function','\n')
    cat('If an error occurs, connect to the network','\n')
    install.packages("dplyr")
  }
  
suppressPackageStartupMessages(library("dplyr"))

## simple_ecos2 function START ------------------
simple_ecos2<-function(ecos_dataset, start_year, cycle=1 ){

## ---------------------------------------------------------------
## mkdate_series()함수 -------------------------------------------
mkdate_series<-function(df, start_y, start_m=0, mq=0 ){
df<-as.data.frame(df)
n<-nrow(df)
tmp1<-rep(NA,n)
tmp2<-rep(NA,n)
tmp3<-rep(NA,n)

## monthly data START ----------------
if(start_m>0 & mq==12) { 

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
		} 
}

tmp1<-as.character(tmp1)
tmp2<-as.character(tmp2)

for (i in 1:n){
if(nchar(tmp2[i])==1) {
tmp2[i]<-paste0('0', tmp2[i], sep='')
} }
}

## monthly data END ----------------

# quarterly data START --------------------------

if(start_m>0 & mq==4) {
	for(i in 1:n){

if( (start_m != 3) & (start_m !=6 ) & (start_m != 9) & (start_m!=12) ) {
	cat(' ', '\n')
	cat('  Starting month should be one of 3, 6, 9, 12. ', '\n')
	cat(' ', '\n')
	break
}
		
		if(start_m==12) 
			{tmp2[i]<-start_m
			 start_m<-3
			 tmp1[i]<-start_y
			 start_y<-start_y+1}
		else{
			 tmp2[i]<-start_m
			 start_m<-start_m+3
			 tmp1[i]<-start_y
		} 
	}

	tmp1<-as.character(tmp1)
	tmp2<-as.character(tmp2)


	for (i in 1:n){
		if(nchar(tmp2[i])==1) {
		tmp2[i]<-paste0('0', tmp2[i], sep='')
		} }
	}

# quarterly data END --------------------------

if(start_m !=0) {
	for (i in 1:n){
		tmp3[i]<-paste0( tmp1[i], '-',tmp2[i], '-01', sep='')
		}

DATE<-as.Date(tmp3)
df<-cbind(df, DATE)
}

# yearly data START --------------------------
if(start_m==0) {
	for (i in 1:n){
	  tmp_start_y <- (start_y +i - 1)
	  tmp3[i]<-paste0( tmp_start_y,'-12-31', sep='')
	}
DATE<-as.Date(tmp3)
df<-cbind(df, DATE)
}
# yearly data END --------------------------

suppressPackageStartupMessages(library("dplyr"))
df <- df%>%relocate(DATE)

return(df)
}


## mkdate_series() 끝 --------------------------------------------
## ----------------------------------------------------------------

if(class(start_year)=="numeric") {
  cat("Start year should be character format such as '2018' ", '\n')
  break
  }

if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
	cat('dplyr is necessary for this function','\n')
	cat('If an error occurs, connect to the network','\n')
	install.packages("dplyr")
  }

suppressPackageStartupMessages(library("dplyr"))

df<-as.data.frame(ecos_dataset)
df <- subset(df, select=c('time', 'data_value'))

n<-nrow(df)

## First period time 
cat("  First Time of the Record:", df$time[1], '\n')

for(i in 1:n){
tmp_time<-as.character(substring(df$time[i], 1, 4))
if(tmp_time == start_year) {
  nx<-i; break }
  }


## Yearly/Quarterly/Monthly데이터를 인식하는 코드
tmp_time<-as.character(substring(df$time[i], 5, 5))
if(tmp_time == "") {
  cycle<-1 }
else if(tmp_time == "0") {
  cycle<-12; start_m=as.numeric(substring(df$time[i], 6, 6)) }
else if(tmp_time == "1") {
  cycle<-12; start_m=as.numeric(substring(df$time[i], 5, 6)) }
else if(tmp_time == "Q") {
  cycle<-4; start_m=as.numeric(substring(df$time[i], 6, 6)); start_m=start_m*3}


## 원하는 연도부터 데이터를 추출하는 코드
df<-df[c(nx:n), ]
colnames(df)<-c('time', 'data')

## QoQYoY의 계산
QoQYoY <- diff(df$data, lag = cycle)

## percent change의 계산
percent_change <- function(x) {
  c(NA, ( (x[-1]/x[-length(x)] ) -1) ) }

percentchange <- percent_change(df$data)

yNA<-rep(NA, cycle)
QoQYoY<-c(yNA, QoQYoY)
df<-cbind(df, QoQYoY, percentchange)

start_year<-as.numeric(start_year)

if(cycle!=1) {
  df<-mkdate_series(df, start_year, start_m, cycle )
  df<-df%>%relocate(DATE, .before='time')
  }

if(cycle==1) {
  df$DATE<-NA
  n2<-nrow(df)
  for (i in 1:n2){
    df$DATE[i] <- paste0(df$time[i],'-12-31', sep='')
   }
   df<-df%>%relocate(DATE, .before='time')
   df$DATE<-as.Date(df$DATE)
   }

   return(df)
}

## simple_ecos2 function END --------------------
## ----------------------------------------------

if(end_year=="9999") {
final_df1 <- simple_ecos2(ecos_dataset, start_year, cycle=1 )
return(final_df1)
}


if(end_year!="9999") {
tmp_df2 <- simple_ecos2(ecos_dataset, start_year )
ncount<-nrow(tmp_df2)

end_year <- as.numeric(end_year)
end_year <- end_year + 1
end_year <- as.character(end_year)

for(i in 1:ncount){
tmp_time<-as.character(substring(tmp_df2$time[i], 1, 4))
if(tmp_time == end_year) {
  nx_endyear<-i
  break }
}

final_df2<-tmp_df2[c(1:(nx_endyear-1)), ]
return(final_df2)
}


}

