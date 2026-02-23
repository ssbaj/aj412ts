irf22 <- function(irf_obj, impulse_var = NULL) {
  
if (base::missing(irf_obj)) {
     cat("\033[1;33m#----------------------------- \033[0m  ", '\n')
     cat("  irf22( irf(re.var, impulse='lp', response='ly') )", '\n')
     cat("  irf22( irf(re.var, impulse='ly', n.ahead=15 ) ) ", '\n')
     return(cat("\033[1;33m#----------------------------- \033[0m ") )
  }

  # --- 패키지 체크 생략 (기존과 동일) ---
  if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  suppressPackageStartupMessages({
    library(tidyr); library(ggplot2); library(dplyr)
  })

  # --- [핵심 수정] impulse_var가 없으면 객체에서 자동으로 가져옴 ---
  if (is.null(impulse_var)) {
    impulse_var <- irf_obj$impulse
  }

  # 1. irf 객체에서 데이터 추출
  extract_data <- function(list_obj, type) {
    # irf_obj$irf[[impulse_var]] 구조로 접근
    data.frame(list_obj[[impulse_var]]) %>%
      mutate(period = 0:(n() - 1), type = type) %>%
      pivot_longer(-c(period, type), names_to = "response", values_to = "value")
  }
  
  df_irf <- extract_data(irf_obj$irf, "mean")
  df_low <- extract_data(irf_obj$Lower, "low")
  df_upp <- extract_data(irf_obj$Upper, "upp")
  
  # 2. 데이터 결합
  plot_data <- df_irf %>%
    bind_rows(df_low, df_upp) %>%
    pivot_wider(names_from = type, values_from = value)
  
  # 3. 시각화
  ggplot(plot_data, aes(x = period, y = mean)) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", alpha = 0.5) +
    geom_ribbon(aes(ymin = low, ymax = upp), fill = "steelblue", alpha = 0.2) +
    geom_line(color = "steelblue", linewidth = 1) +
    facet_wrap(~ response, scales = "free_y") +
    theme_minimal() +
    labs(
      title = paste("Orthogonal Impulse Response Function. Impulse varialble:", impulse_var),
      subtitle = "Shaded area represents 95% Bootstrap Confidence Interval",
      x = "Period", y = "Response"
    )
}
