cajorls_result<-function(result) {

 if (base::missing(result)) {
  cat(" \033[1;36m# VECM 분석결과 ---------- \033[0m", '\n' )
  return( cat("\033[1;36m   COMMAND: cajorls(RESULT) \033[0m", '\n') )
  }
  
  coef <- coef(result$rlm)  # 계수 추정치
  tmp<-data.frame(coef)
  
  coef2<-as.vector(coef)

  se <- sqrt(diag(vcov(result$rlm)))  # 표준오차
  se<-as.vector(se)

  t_values <- coef / se
  t_values<-as.vector(t_values)
  
  df <- result$rlm$df.residual  # 잔차의 자유도
  p_values <- 2 * (1 - pt(abs(t_values), df))
  p_values<-as.vector(p_values)

  no_cols<-length(colnames(tmp))
  no_rows<-length(rownames(tmp))

  r=c()
  for(i in 1:no_cols){
    for(j in 1:no_rows){
       tmp_word<-paste0(colnames(tmp)[i], "::", rownames(tmp)[j], sep="")
  	   r=c(r, tmp_word)
    }
  }

summary_table <- data.frame(r, coef2, se, t_values, p_values)
colnames(summary_table) <- c('variables', 'coeff','Std.Err','t-value','p-value')

cat('  \033[1;34m# VECM 분석결과 \033[0m','\n')
cat(' # ------------------------------------ ','\n')
cat('  \033[1;34m# Error Correction Term \033[0m ','\n')
print(result$beta)
cat(' # ------------------------------------ ','\n')
cat('  \033[1;34m# VAR model \033[0m','\n')
print(summary_table)

}

