#' e_granger() 
#' 

e_granger<-function(explaining=0){
if(explaining==0) {
cat(" ", '\n')
cat(" ", '\n')
cat("\033[1;31m# vars 패키지로 granger검정 ----------------- \033[0m ", '\n')
cat(" ", '\n')
cat(" library(vars)  ", '\n')
cat("  ", '\n')
cat(" # 두 개의 시계열 데이터를 합친 데이터프레임 생성 (예: x, y) ", '\n')
cat(" data_var <- cbind(x, y) ", '\n')
cat("  ", '\n')
cat(" # 최적 시차 선택 (AIC, HQ, SC, FPE 기준 제공) ", '\n')
cat(" lag_select <- VARselect(data_var, lag.max = 10, type = 'const') ", '\n')
cat(" best_lag <- lag_select$selection['AIC(n)'] # AIC 기준 최적 시차 추출 ", '\n')
cat(" var_model <- VAR(data_var, p = best_lag, type = 'const') ", '\n')
cat("  ", '\n')
cat(" # x가 y를 Granger 인과하는지 검정 ", '\n')
cat(" granger_x_to_y <- causality(var_model, cause = 'x') ", '\n')
cat(" print(granger_x_to_y$Granger) ", '\n')
cat(" p-value < 0.05: 귀무가설을 기각. 즉, 인과관계가 존재한다고 판단합니다. ", '\n')
cat("  ", '\n')
cat(" # y가 x를 Granger 인과하는지 검정 ", '\n')
cat(" granger_y_to_x <- causality(var_model, cause = 'y') ", '\n')
cat(" print(granger_y_to_x$Granger) ", '\n')
cat(" p-value < 0.05: 귀무가설을 기각. 즉, 인과관계가 존재한다고 판단합니다. ", '\n')
cat(" ", '\n')
}
}