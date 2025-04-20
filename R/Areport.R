Areport<-function(fit, digits=4 ){

if (base::missing(fit)) {
	  cat("  #사용법 -------  ", '\n')
	  cat("       Areport(RESULT) ) ", '\n')
	  return( cat("       Areport(RESULT, fixed=c(NA, 0, 1, NA ...) ) ", '\n') )
	}

generate_arima_report <- function(model) {
  order <- arimaorder(model)
  seasonal_order <- order[4:6]
  sprintf("ARIMA(%g,%g,%g)(%g,%g,%g)[%g]", 
          order[1], order[2], order[3], 
          seasonal_order[1], seasonal_order[2], seasonal_order[3], 
          frequency(model$x))
}
cat("  ", '\n')  
cat(" #------------------------------------- ", '\n')  
  coefs <- fit$coef
  df_value <- fit$nobs - length(fit$coef)
  logLik_value <- logLik(fit)
  
  cat("  Number of Data for Analysis:", fit$nobs, '\n')
  cat("  Degree of Freedom:", df_value, '\n')
cat(" #------------------------------------- ", '\n')  
  cat("  AIC:", AIC(fit), "\n")
  cat("  BIC:", BIC(fit), "\n")
  cat("  Log Likelihood:", logLik_value, "\n")
cat(" #------------------------------------- ", '\n')  
  r0=c();  r1=c();  r2=c(); r3=c()
  n<-length(fit$coef)

## 명령문의 fixed 값을 솎아내는 코드 -----------
if(!is.null(fit[["call"]]$fixed) ) {
  fixed2<-formula(fit[["call"]]$fixed)
  n_fixed2<-length(fixed2)
  fixed<-rep(NA, n_fixed2)
  for(k in 1:n_fixed2){
    fixed[k]<-fixed2[[k]]
    }
}

# fixed()옵션에 맞추 표준오차를 재정렬하는 코드 시작 --------------

if( !is.null(fit[["call"]]$fixed) & length(fixed) !=0 ) {

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

tmp.se <- fill_XNOTE(fixed, tmpSE22)

} else {

tmp.se <- sqrt( diag(fit$var.coef) )

}


# fixed()옵션에 맞추 표준오차를 재정렬하는 코드 끝 ----------------


for(i in 1:n){
    r0=c(r0, fit$coef[i])
    r1=c(r1, tmp.se[i])
    tmp_r2=fit$coef[i]/tmp.se[i]
    r2=c(r2, tmp_r2)
    tmp_pr2 = 2*round( ( 1- pt( abs( tmp_r2 ) , df_value ) ), digits)
    r3=c(r3, tmp_pr2)
 }


tmp_df<-data.frame(rbind(r0, r1, r2, r3))
rownames(tmp_df)[1]<-"Coeff"
rownames(tmp_df)[2]<-"s.e"
rownames(tmp_df)[3]<-"t-value"
rownames(tmp_df)[4]<-"p-value"


print( round(tmp_df, digits))

report <- generate_arima_report(fit)
cat("Selected ARIMA Model: ", report, "\n")
}
