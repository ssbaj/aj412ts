#'
#' pacff() Function
#'
#' This function draws PACF graph (rotated 90 degrees)
#' with 5-lag interval grid lines and dynamic title.
#'

pacff <- function(xdatas, lagsx = 15) {

  # graphics.off() # 필요 시 주석 해제

  if (base::missing(xdatas)) {
    return(cat("  pacff(df, lag numbers such as 20 or drop this option) \n"))
  }

  # 입력된 변수명 추출
  var_name <- deparse(substitute(xdatas))
  tm_length <- length(xdatas)

  # 시차(Lag) 설정 로직
  if (tm_length <= 40) {
    lags <- round(tm_length * (2/3), 0)
  } else if (tm_length <= 60) {
    lags <- round(tm_length * (1.2/2), 0)
  } else if (tm_length <= 100) {
    lags <- round(tm_length * (0.9/2), 0)
  } else {
    lags <- 40
  }

  if (!base::missing(lagsx)) lags <- lagsx

  n <- length(xdatas)
  b <- c()

  # PACF 값 계산 (기존 pac 함수 사용)
  for (k in 1:lags) {
    b <- c(b, pac(xdatas, k))
  }

  ## ---- reverse order for vertical display ----
  b_rev   <- rev(b)
  lag_rev <- rev(1:lags)

  if (TRUE) {
    # 타이틀 생성: Number of Data와 변수명을 줄바꿈하여 결합
    main_title <- paste0("Number of Data: ", n, "\n", var_name)

    # barplot의 반환값(Y축 위치)을 bp에 저장
    bp <- barplot(
      b_rev,
      horiz = TRUE,      # 90도 회전
      col = "steelblue",
      xlim = c(-1, 1),
      border = NA,
      ylab = "Lags",
      xlab = "PACF",
      names.arg = lag_rev,
      main = main_title,
      las = 1,           # 라벨 가로 정렬
      cex.main = 0.9      # 타이틀 크기 조정
    )

    box()
    abline(v = 0)
    # 신뢰구간 (Red dotted lines)
    abline(v =  2 / sqrt(n), lty = 3, col = "red")
    abline(v = -2 / sqrt(n), lty = 3, col = "red")

    ## ---- 5기간 간격 자동 수평 보조선 추가 ----
    # 5의 배수인 시차들을 계산
    target_lags <- seq(from = 0, to = max(lag_rev), by = 5)
    
    for (l in target_lags) {
      idx <- which(lag_rev == l)
      if (length(idx) > 0) {
        # 5단위마다 연한 회색 점선 추가
        abline(h = bp[idx], lty = 3, col = "gray70")
      }
    }
  }

  return(invisible(b))
}