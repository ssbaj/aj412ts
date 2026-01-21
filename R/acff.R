#'
#' acff() Function
#'
#' This function draws ACF graph (rotated 90 degrees, corrected order)
#' with 5-lag interval grid lines and dynamic title.
#'

acff <- function(xdatas, lagsx = 15) {
  
  # graphics.off() # 필요 시 주석 해제 (기존 창 모두 닫기)

  if (base::missing(xdatas)) {
    return(cat("  acff(x, lag numbers such as 20 or drop this option) \n"))
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

  # ACF 값 계산 (기존 ac 함수 사용)
  # 참고: ac 함수가 환경에 정의되어 있어야 합니다. 
  # 만약 없다면 stats::acf(xdatas, plot=F)$acf[-1] 등으로 대체 가능합니다.
  r <- c()
  for (k in 1:lags) {
    r <- c(r, ac(xdatas, k))
  }

  ## ---- reverse order for correct vertical direction ----
  r_rev <- rev(r)
  lag_rev <- rev(1:lags)

  if (TRUE) { # plot 옵션이 항상 참인 것으로 가정
    
    # 타이틀 생성: Number of Data와 변수명을 줄바꿈하여 결합
    main_title <- paste0("Number of Data: ", tm_length, "\n", var_name)

    # barplot의 반환값(Y축 위치)을 bp에 저장
    bp <- barplot(
      r_rev,
      horiz = TRUE,
      col = "steelblue",
      xlim = c(-1, 1),
      border = NA,
      ylab = "Lags",
      xlab = "ACF",
      names.arg = lag_rev,
      main = main_title,
      las = 1,      # 라벨 가로 정렬
      cex.main = 0.9 # 타이틀 크기 조정
    )

    box()
    abline(v = 0)
    # 신뢰구간 (Blue lines)
    abline(v =  2 / sqrt(tm_length), lty = 3, col = "red")
    abline(v = -2 / sqrt(tm_length), lty = 3, col = "red")

    ## ---- 5기간 간격 자동 수평 보조선 추가 ----
    # 5의 배수인 시차들을 계산
    target_lags <- seq(from = 0, to = max(lag_rev), by = 5)
    
    for (l in target_lags) {
      idx <- which(lag_rev == l)
      if (length(idx) > 0) {
        # 시차 0은 제외하고(이미 v=0 선이 있으므로) 5단위마다 수평선 추가
        abline(h = bp[idx], lty = 3, col = "gray70")
      }
    }
  }

  return(invisible(r))
}