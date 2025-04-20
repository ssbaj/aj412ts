Areport<-function(fit, digits=4, fixed=NULL ){

if (base::missing(fit)) {
	  cat("  사용법:  ", '\n')
          cat("  Areport(RESULT) ", '\n')
          cat("  fixed로 제외하는 AR 또는, MA구조가 있다면 0으로 지정 하세요", '\n')
	  return( cat("       Areport(RESULT, fixed=c(NA, 0, NA, ...) ) ", '\n') )
	}


n_fixed<-length(fixed)
zero_fixed_i<-0

if(n_fixed != 0 ){
  for(m in 1:n_fixed){
  if( !is.na(fixed[m] ) & fixed[m] == 0) {zero_fixed_i<-m}
  }
}


generate_arima_report <- function(model) {
  order <- arimaorder(model)
  seasonal_order <- order[4:6]
  sprintf("ARIMA(%g,%g,%g)(%g,%g,%g)[%g]", 
          order[1], order[2], order[3], 
          seasonal_order[1], seasonal_order[2], seasonal_order[3], 
          frequency(model$x))
}

  coefs <- fit$coef
  df_value <- length(fit$x) - length(coefs)
  logLik_value <- logLik(fit)
  
  cat("\nDegree of Freedom:", df_value)
  cat("\nCoefficient Estimates:\n")
  print(coefs)
  
  cat("\nAIC:", AIC(fit))
  cat("\nBIC:", BIC(fit))
  cat("\nLog Likelihood:", logLik_value, "\n")
    
  r0=c();  r1=c();  r2=c(); r3=c()
  n<-length(fit$coef)
  
  for(i in 1:n){
    if(i==zero_fixed_i) {next} 
    r0=c(r0, fit$coef[i])
    r1=c(r1, sqrt(fit$var.coef[i,i]) )
    tmp_r2 = fit$coef[i]/sqrt(fit$var.coef[i,i])
    r2=c(r2, tmp_r2)
    tmp_pr2 = 2*round( ( 1- pt( abs( tmp_r2 ) , df_value ) ), 4)
	r3=c(r3, tmp_pr2)
  }
tmp_df<-data.frame(rbind(r0, r1, r2, r3))
rownames(tmp_df)[1]<-"Coeff"
rownames(tmp_df)[2]<-"s.e"
rownames(tmp_df)[3]<-"t-value"
rownames(tmp_df)[4]<-"p-value"


print(round(tmp_df, digits))

report <- generate_arima_report(fit)
cat("\nSelected ARIMA Model: ", report, "\n")
}
