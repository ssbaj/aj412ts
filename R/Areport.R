Areport<-function(fit, digits=4 ){

if (base::missing(fit)) {
	  cat("  #사용법 -------  ", '\n')
	  cat("       Areport(RESULT) ) ", '\n')
	  return( cat("       Areport(RESULT, fixed=c(NA, 0, 1, NA ...) ) ", '\n') )
	}


if (!requireNamespace("forecast", quietly = TRUE)) {
  # 패키지가 없을 경우 메시지 출력 후 실행 중단
  cat("------------------------------------------------------------\n")
  cat("에러: 'forecast' 패키지가 설치되어 있지 않습니다.\n")
  cat("다음 명령어를 사용하여 먼저 설치해 주세요: install.packages('forecast')\n")
  cat("------------------------------------------------------------\n")
  stop("필수 패키지 미설치로 인해 작업을 중단합니다.")
 }

# 2. 패키지가 존재하면 조용히 로드
# suppressMessages와 suppressWarnings를 사용하여 깔끔하게 실행합니다.
suppressMessages(suppressWarnings(library(forecast)))


generate_arima_report <- function(model) {
  order <- arimaorder(model)
  seasonal_order <- order[4:6]
  sprintf("ARIMA(%g,%g,%g)(%g,%g,%g)[%g]", 
          order[1], order[2], order[3], 
          seasonal_order[1], seasonal_order[2], seasonal_order[3], 
          frequency(model$x))
}

# cat(" #------------------------------------- ", '\n')  
  coefs <- fit$coef
  df_value <- fit$nobs - length(fit$coef)
  logLik_value <- logLik(fit)

cat("  차분을 적용한 유효 관측치 수:", fit$nobs, '\n')
cat("  자유도(Degree of freedom):", df_value, '\n')
cat(" #------------------------------------- ", '\n')  
  cat("  AIC:", AIC(fit), "\n")
  cat("  BIC:", BIC(fit), "\n")
  cat("  Log Likelihood:", logLik_value, "\n")
cat(" #------------------------------------- ", '\n')  
  r0=c();  r1=c();  r2=c(); r3=c()
  n<-length(fit$coef)

##  -----------
if( !is.null(fit[["call"]]$fixed) ) {
  fixed2<-formula(fit[["call"]]$fixed)
  n_fixed2<-length(fixed2)
  fixed22<-rep(NA, n_fixed2)
  for(k in 1:n_fixed2){
    fixed22[k]<-fixed2[[k]]
    }
}


# fixed()옵션에 맞추 표준오차를 재정렬하는 코드 시작 --------------

if( !is.null(fit[["call"]]$fixed) ) {

tmpSE22 <- sqrt( diag(fit$var.coef) )
j<-0

# 표본오차 값을 fixed에 맞춰 재배열하는 함수

fill_XNOTE <- function(fixed, tmpSE22) {
XNOTE <- numeric(length(fixed))  # XNOTE 변수를 생성
for (i in 1:length(fixed)) {
    if (is.na(fixed[i])) {
      j=j+1
      XNOTE[i] <- tmpSE22[j]
    } else {
      XNOTE[i] <- 0
    }
  }
  
  return(XNOTE)
 }

tmp.se <- fill_XNOTE(fixed22, tmpSE22)

} else {

tmp.se <- sqrt( diag(fit$var.coef) )

}



# fixed()옵션에 맞추 표준오차를 재정렬하는 코드 끝 ----------------


for(i in 1:n){
    r0=c(r0, fit$coef[i])
    r1=c(r1, tmp.se[i])
    tmp_r2=fit$coef[i]/tmp.se[i]
    r2=c(r2, tmp_r2)
    tmp_pr2 = 2*round( ( 1- pnorm( abs( tmp_r2 ) ) ), digits)
    r3=c(r3, tmp_pr2)
 }


tmp_df<-data.frame(rbind(r0, r1, r2, r3))
rownames(tmp_df)[1]<-"Coeff "
rownames(tmp_df)[2]<-"Std.Err "
rownames(tmp_df)[3]<-"z_values "
rownames(tmp_df)[4]<-"p_values "

## fixed옵션을 사용할 경우 t값이 NaN이거나 Inf이면 변수 삭제----------------
if( !is.null(fit[["call"]]$fixed) ) {
ncol_record<-c()
n<-ncol(tmp_df)

for(i in 1:n){
if(tmp_df[3,i] == 'NaN' ) {ncol_record<-c(ncol_record, i) }
}

if(length(ncol_record) !=0 ){
tmp_df<-tmp_df[, -ncol_record]
}

}

tmp_df <- data.frame( t(tmp_df) )

print( round(tmp_df, digits))

report <- generate_arima_report(fit)
cat("  ", '\n')  
cat("Selected ARIMA Model: ", report, "\n")
cat("  ", '\n')  
}
