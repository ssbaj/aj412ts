Areport <- function(fit, digits=4){

  if (base::missing(fit)) {
    cat("  #사용법 -------  ", '\n')
    cat("        Areport(RESULT) ) ", '\n')
    return( cat("        Areport(RESULT, fixed=c(NA, 0, 1, NA ...) ) ", '\n') )
  }
  
  # 패키지 로드 확인
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("forecast 패키지가 필요합니다. install.packages('forecast')")
  }
  suppressMessages(suppressWarnings(library(forecast)))

  # --- [1] 기본 정보 및 자유도 계산 ---
  
  # 1-1. 유효 관측치 수(nobs) 확보
  if(is.null(fit$nobs)) {
      fit$nobs <- length(fit$residuals)
  }

  # 1-2. fixed 벡터 추출
  fixed_vec <- NULL
  if( !is.null(fit[["call"]]$fixed) ) {
    fixed_vec <- tryCatch(eval(fit[["call"]]$fixed), error=function(e) NULL)
  }
  
  # 추정된 모수 개수 계산
  if(!is.null(fixed_vec)){
      n_estimated <- sum(is.na(fixed_vec)) 
  } else {
      n_estimated <- length(fit$coef)
  }
  
  # 1-3. 자유도 및 LogLik 계산
  df_value <- fit$nobs - n_estimated
  logLik_value <- tryCatch(logLik(fit), error=function(e) NA)
  
  # --- [2] AIC, BIC 계산 (CSS 수동 계산 로직 수정됨) ---
  
  # 1. 일단 기본 함수로 시도
  aic_val <- tryCatch(stats::AIC(fit), error=function(e) NA)
  bic_val <- tryCatch(stats::BIC(fit), error=function(e) NA)

  # 2. CSS 방식인지 확인 (Call 객체 확인 또는 AIC가 NA인 경우)
  is_css <- FALSE
  
  # 사용자가 명시적으로 CSS를 썼거나, AIC가 계산되지 않았다면 CSS 로직 적용
  if ( (!is.null(fit$call$method) && fit$call$method == "CSS") || is.na(aic_val) ) {
      is_css <- TRUE
  }
  
  # 3. CSS일 경우 공식 적용
  if (is_css) {
      # 잔차 제곱합 (SSR)
      SSR <- sum(fit$residuals^2, na.rm = TRUE)
      
      # 사용자 요청 공식 적용
      # AIC = n * log(SSR) + 2 * k
      # BIC = n * log(SSR) + k * log(n)
      
      k_len <- length(fit$coef) # 전체 파라미터 수 (상수항 포함)
      n_obs <- fit$nobs
      
      if (SSR > 0) {
        aic_val <- n_obs * log(SSR) + 2 * k_len
        bic_val <- n_obs * log(SSR) + k_len * log(n_obs)
      } else {
        aic_val <- NA
        bic_val <- NA
      }
  }

  # --- [3] 결과 요약 출력 ---
  
  cat(" #------------------------------------- ", '\n')  
  cat("  차분을 적용한 유효 관측치 수:", fit$nobs, '\n')
  cat("  자유도(Degree of freedom):", df_value, '\n')
  
  # 값이 NA여도 출력되도록 처리
  cat("  AIC:", ifelse(is.na(aic_val), "NA", round(aic_val, digits)), 
      ifelse(is_css, "(CSS approx)", ""), "\n")
  cat("  BIC:", ifelse(is.na(bic_val), "NA", round(bic_val, digits)), 
      ifelse(is_css, "(CSS approx)", ""), "\n")
      
  cat("  Log Likelihood:", ifelse(is.na(logLik_value), "NA (CSS)", round(logLik_value, digits)), "\n")
  cat(" #------------------------------------- ", '\n')  

  # --- [4] 표준오차(SE) 매핑 및 t-검정 ---
  
  coef_len <- length(fit$coef)
  tmp_se <- numeric(coef_len)
  
  if( !is.null(fixed_vec) ) {
      if(length(fixed_vec) != coef_len){
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
                  tmp_se[i] <- 0
              }
          }
      }
  } else {
      tmp_se <- sqrt(diag(fit$var.coef))
  }

  t_vec <- fit$coef / tmp_se
  p_vec <- 2 * (1 - pt(abs(t_vec), df = df_value))
  
  final_df <- data.frame(
    "Coeff"    = fit$coef,
    "Std.Err"  = tmp_se,
    "t_values" = t_vec,
    "p_values" = p_vec
  )

  # --- [5] 결과 테이블 정제 ---
  if( !is.null(fixed_vec) ) {
      fixed_rows <- which(tmp_se == 0)
      bad_rows <- which(is.nan(final_df$t_values) | is.infinite(final_df$t_values))
      remove_indices <- unique(c(fixed_rows, bad_rows))
      
      if(length(remove_indices) > 0){
          final_df <- final_df[-remove_indices, ]
      }
  }

  print(round(final_df, digits))

  # --- [6] 모형 이름 출력 ---
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
