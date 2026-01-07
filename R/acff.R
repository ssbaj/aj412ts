#'
#' This function draws ACF graph (rotated 90 degrees, corrected order)
#'

acff <- function(xdatas, lagsx = 15){

  if (base::missing(xdatas)) {
    return(cat("  acff(x, lag numbers such as 20 or drop this option ) "))
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

  r <- c()
  n <- length(xdatas)

  for (k in 1:lags){
    r <- c(r, ac(xdatas, k))
  }

  ## ---- reverse order for correct vertical direction ----
  r_rev <- rev(r)
  lag_rev <- rev(1:lags)

  barplot(
    r_rev,
    horiz = TRUE,
    col = "steelblue",
    xlim = c(-1, 1),
    border = NA,
    ylab = "Lags",
    xlab = "ACF",
    names.arg = lag_rev,
    main = paste("Number of Data:", length(xdatas))
  )

  box()
  abline(v = 0)
  abline(v =  2 / sqrt(n), lty = 3)
  abline(v = -2 / sqrt(n), lty = 3)

  return(r)
}
