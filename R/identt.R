#'
#' identt() Function
#'
#' This function draws vertical ACF & PACF as barplots
#'

identt <- function(xdatas, lagx2 = 15){

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
