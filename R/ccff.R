#'
#' ccff() Function
#'
#' This function draws CCF as a vertical barplot (rotated)
#'

ccff <- function(x, y,
                 lag.max = NULL,
                 type = c("correlation", "covariance"),
                 na.action = na.fail,
                 plot = TRUE, ...) {

  type <- match.arg(type)

  if (is.matrix(x) || is.matrix(y))
    stop("univariate time series only")

  ## ---- original ccf computation ----
  X <- ts.intersect(as.ts(x), as.ts(y))
  colnames(X) <- c(deparse(substitute(x))[1L],
                   deparse(substitute(y))[1L])

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

    ## ---- vertical barplot correction ----
    val_rev <- rev(val)
    lag_rev <- rev(lag)

    barplot(
      val_rev,
      horiz  = TRUE,              # 90도 회전
      col    = "steelblue",
      border = NA,
      xlim   = c(-1, 1),
      xlab   = "CCF",
      ylab   = "Lags",
      names.arg = lag_rev,
      main = paste("Number of Data:", n)
    )

    box()
    abline(v = 0)
    abline(v =  2 / sqrt(n), lty = 3)
    abline(v = -2 / sqrt(n), lty = 3)
  }

  ## ---- return structure (ccf compatible) ----
  acf.out$acf <- array(val, dim = c(length(val), 1L, 1L))
  acf.out$lag <- array(lag, dim = c(length(val), 1L, 1L))
  acf.out$snames <- paste(acf.out$snames, collapse = " & ")

  invisible(acf.out)
}
