\name{e_granger}
\alias{e_granger}
\title{ca.jo model}
\usage{
e_granger()
}

\description{
Granger Causality Test
}

\examples{

library(vars)   
   
 # 두 개의 시계열 데이터를 합친 데이터프레임 생성 (예: x, y)  
 data_var <- cbind(x, y)  
   
 # 최적 시차 선택 (AIC, HQ, SC, FPE 기준 제공)  
 lag_select <- VARselect(data_var, lag.max = 10, type = 'const')  
 best_lag <- lag_select$selection['AIC(n)'] # AIC 기준 최적 시차 추출  
 var_model <- VAR(data_var, p = best_lag, type = 'const')  
   
 # x가 y를 Granger 인과하는지 검정  
 granger_x_to_y <- causality(var_model, cause = 'x')  
 print(granger_x_to_y$Granger)  
 p-value < 0.05: 귀무가설을 기각. 즉, 인과관계가 존재한다고 판단합니다.  
   
 # y가 x를 Granger 인과하는지 검정  
 granger_y_to_x <- causality(var_model, cause = 'y')  
 print(granger_y_to_x$Granger)  
 p-value < 0.05: 귀무가설을 기각. 즉, 인과관계가 존재한다고 판단합니다.

}