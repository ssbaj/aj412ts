ccff <- function(x, y,
                 lag.max = NULL,
                 type = c("correlation", "covariance"),
                 na.action = na.fail,
                 plot = TRUE, ...) {
  
  type <- match.arg(type)
  
  # 입력된 변수명을 문자열로 추출
  name_x <- deparse(substitute(x))
  name_y <- deparse(substitute(y))
  var_names <- paste(name_x, "&", name_y)
  
  if (is.matrix(x) || is.matrix(y))
    stop("univariate time series only")
  
  X <- ts.intersect(as.ts(x), as.ts(y))
  colnames(X) <- c(name_x, name_y)
  
  acf.out <- acf(
    X,
    lag.max = lag.max,
    plot = FALSE,
    type = type,
    na.action = na.action
  )
  
  lag <- c(rev(acf.out$lag[-1, 2, 1]),
           acf.out$lag[, 1, 2])
  
  val <- c(rev(acf.out$acf[-1, 2, 1]),
           acf.out$acf[, 1, 2])
  
  n <- length(x)
  
  if (plot) {
    val_rev <- rev(val)
    lag_rev <- rev(lag)
    
    # [수정] 줄바꿈 기호(\n)를 사용하여 변수명을 다음 줄로 이동
    main_title <- paste0("Number of Data: ", n, "\n", var_names)
    
    bp <- barplot(
      val_rev,
      horiz  = TRUE,
      col    = "steelblue",
      border = NA,
      xlim   = c(-1, 1),
      xlab   = "CCF",
      ylab   = "Lags",
      names.arg = lag_rev,
      main = main_title, # 두 줄로 표시되는 타이틀
      las = 1,
      cex.main = 0.9     # 타이틀이 두 줄이므로 글자 크기를 약간 조절 (필요시 조정)
    )
    
    box()
    abline(v = 0)
    abline(v =  2 / sqrt(n), lty = 3, col = "red")
    abline(v = -2 / sqrt(n), lty = 3, col = "red")
    
    # 5기간 간격 자동 보조선
    min_lag <- min(lag_rev)
    max_lag <- max(lag_rev)
    target_lags <- seq(from = floor(min_lag/5)*5, 
                       to   = ceiling(max_lag/5)*5, 
                       by   = 5)
    
    for(l in target_lags) {
      idx <- which(lag_rev == l)
      if(length(idx) > 0) {
        line_col <- if(l == 0) "gray20" else "gray70"
        abline(h = bp[idx], lty = 3, col = line_col) 
      }
    }
  }
  
  acf.out$acf <- array(val, dim = c(length(val), 1L, 1L))
  acf.out$lag <- array(lag, dim = c(length(val), 1L, 1L))
  acf.out$snames <- paste(acf.out$snames, collapse = " & ")
  
  invisible(acf.out)
}