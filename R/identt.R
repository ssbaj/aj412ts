#'
#' identt() Function
#'
#' This function draws vertical ACF & PACF as barplots
#'

identt <- function(xdatas, lagx2 = 15){

  graphics.off()

#------------------------------------
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


#-------------------------------------
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


#-------------------------------------

  if (base::missing(xdatas)) {
    return(cat("  identt(df, lag: You decide or blank: automatic_allocation ) "))
  }

  tm_length <- length(xdatas)

  if (tm_length <= 40) {
    lagx <- round(tm_length * (2/3), 0)
  } else if (tm_length <= 60) {
    lagx <- round(tm_length * (1.2/2), 0)
  } else if (tm_length <= 100) {
    lagx <- round(tm_length * (0.9/2), 0)
  } else {
    lagx <- 30
  }

  if (!base::missing(lagx2)) lagx <- lagx2

  ## ---- side-by-side plots ----
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2), mar = c(5, 6, 4, 2))

  acff(xdatas, lagx)    # vertical barplot ACF (steelblue)
  pacff(xdatas, lagx)   # vertical barplot PACF (steelblue)

  par(op)
}
