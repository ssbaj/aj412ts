Areport <- function(fit, digits=4){

  if (base::missing(fit)) {
    cat("  #사용법 -------  ", '\n')
    cat("       Areport(RESULT) ) ", '\n')
    return( cat("       Areport(RESULT, fixed=c(NA, 0, 1, NA ...) ) ", '\n') )
  }
  
  # 패키지 로드 확인
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("forecast 패키지가 필요합니다. install.packages('forecast')")
  }
  suppressMessages(suppressWarnings(library(forecast)))

  # --- [1] 기본 정보 및 자유도 계산 ---
  
  # 1-1. 유효 관측치 수(nobs) 확보 (TSA 패키지 호환성)
  if(is.null(fit$nobs)) {
      fit$nobs <- length(fit$residuals)
  }

  # 1-2. fixed 벡터 추출 및 추정된 파라미터 개수 계산
  fixed_vec <- NULL
  if( !is.null(fit[["call"]]$fixed) ) {
    # call 객체의 fixed 부분을 안전하게 평가(eval)하여 벡터로 변환
    fixed_vec <- tryCatch(eval(fit[["call"]]$fixed), error=function(e) NULL)
  }
  
  # 추정된 모수(파라미터)의 수 계산 (NA인 값만 추정된 것임)
  if(!is.null(fixed_vec)){
      n_estimated <- sum(is.na(fixed_vec)) 
  } else {
      n_estimated <- length(fit$coef)
  }
  
  # 1-3. 자유도 및 LogLik 계산
  df_value <- fit$nobs - n_estimated
  logLik_value <- logLik(fit)
  
  # AIC, BIC 안전하게 추출
  aic_val <- tryCatch(AIC(fit), error=function(e) NA)
  bic_val <- tryCatch(BIC(fit), error=function(e) NA)

  # --- [2] 결과 요약 출력 (위치 변경됨) ---
  # 요청사항: AIC 위쪽에 관측치 수와 자유도 배치
  
  cat(" #------------------------------------- ", '\n')  
  cat("  차분을 적용한 유효 관측치 수:", fit$nobs, '\n')
  cat("  자유도(Degree of freedom):", df_value, '\n')
  cat("  AIC:", aic_val, "\n")
  cat("  BIC:", bic_val, "\n")
  cat("  Log Likelihood:", logLik_value, "\n")
  cat(" #------------------------------------- ", '\n')  

  # --- [3] 표준오차(SE) 매핑 및 t-검정 ---
  
  coef_len <- length(fit$coef)
  tmp_se <- numeric(coef_len)
  
  # fixed 옵션에 따른 SE 매핑
  if( !is.null(fixed_vec) ) {
      if(length(fixed_vec) != coef_len){
         # 길이가 안 맞을 경우 단순 대각원소 사용 (안전장치)
         var_diag <- sqrt(diag(fit$var.coef))
         if(length(var_diag) == coef_len) tmp_se <- var_diag
      } else {
          var_diag <- sqrt(diag(fit$var.coef))
          k <- 1
          for(i in 1:coef_len){
              if(is.na(fixed_vec[i])){
                  if(k <= length(var_diag)) {
                      tmp_se[i] <- var_diag[k]
                      k <- k + 1
                  }
              } else {
                  tmp_se[i] <- 0 # 고정된 값은 SE = 0
              }
          }
      }
  } else {
      tmp_se <- sqrt(diag(fit$var.coef))
  }

  # t값 계산 및 p값(t-분포 사용) 산출
  t_vec <- fit$coef / tmp_se
  p_vec <- 2 * (1 - pt(abs(t_vec), df = df_value))
  
  # 데이터프레임 생성
  final_df <- data.frame(
    "Coeff"    = fit$coef,
    "Std.Err"  = tmp_se,
    "t_values" = t_vec,
    "p_values" = p_vec
  )

  # --- [4] 결과 테이블 정제 ---
  # fixed된 변수(SE가 0이거나 t값이 비정상인 행) 제거
  if( !is.null(fixed_vec) ) {
      fixed_rows <- which(tmp_se == 0)
      bad_rows <- which(is.nan(final_df$t_values) | is.infinite(final_df$t_values))
      remove_indices <- unique(c(fixed_rows, bad_rows))
      
      if(length(remove_indices) > 0){
          final_df <- final_df[-remove_indices, ]
      }
  }

  print(round(final_df, digits))

  # --- [5] 모형 이름 출력 ---
  generate_arima_report <- function(model) {
    tryCatch({
        order <- arimaorder(model)
        seasonal_order <- order[4:6]
        sprintf("ARIMA(%g,%g,%g)(%g,%g,%g)[%g]", 
                order[1], order[2], order[3], 
                seasonal_order[1], seasonal_order[2], seasonal_order[3], 
                frequency(model$x))
    }, error = function(e) "Transfer Function / Custom Model")
  }

  cat("  ", '\n')  
  cat("Selected Model: ", generate_arima_report(fit), "\n")
  cat("  ", '\n')  
}