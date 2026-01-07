#'
#' pacff() Function
#'
#' This function draws PACF (rotated 90 degrees)
#'

pacff <- function(xdatas, lagsx = 15){

  if (base::missing(xdatas)) {
    return(cat("  pacff(df, lag numbers such as 20 or drop this option ) "))
  }

  tm_length <- length(xdatas)

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

  for (k in 1:lags){
    b <- c(b, pac(xdatas, k))
  }

  ## ---- reverse order for vertical display ----
  b_rev   <- rev(b)
  lag_rev <- rev(1:lags)

  barplot(
    b_rev,
    horiz = TRUE,                 # ★ 90도 회전
    col = "steelblue",
    xlim = c(-1, 1),
    border = NA,
    ylab = "Lags",
    xlab = "PACF",
    names.arg = lag_rev,
    main = paste("Number of Data:", length(xdatas))
  )

  box()
  abline(v = 0)
  abline(v =  2 / sqrt(n), lty = 3)
  abline(v = -2 / sqrt(n), lty = 3)

  return(b)
}
