#' diff22() Function
#' 
#' This function conduct difference
#' 

diff22 <- function(data, column_name, diff_num) {
  
if (base::missing(data)) {
   cat(" \033[1;36m# df<-diff22(df, 변수명, 차분 숫자) \033[0m", '\n' )
   return( cat(" \033[1;36m  차분 숫자 = 1: 1차 차분, 4: 분기 차분, 12: 계절 차분 \033[0m", '\n' ) ) }

  
  # column_name을 문자열로 받아 해당 열 추출
  col_vector <- data[[deparse(substitute(column_name))]]
  
  # 차분 계산: 앞부분의 빈 자리를 NA로 채움
  # diff(vector, lag = n)은 뒤 데이터에서 앞 데이터를 뺀 값을 반환함
  diff_values <- c(rep(NA, diff_num), diff(col_vector, lag = diff_num))
  
  # 새로운 변수명 생성 (예: A_1, A_3, A_12)
  new_col_name <- paste0(deparse(substitute(column_name)), "_", diff_num)
  
  # 데이터프레임에 새로운 열 추가 및 반환
  data[[new_col_name]] <- diff_values
  return(data)
}


