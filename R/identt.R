#'
#' identt() Function (Header positioned higher)
#'
#' ACF, PACF, qstat 수치를 출력하며, 표의 헤더를 데이터와 겹치지 않게 위로 올린 버전입니다.
#'

identt <- function(xdatas, lagx2 = 15){

  if (base::missing(xdatas)) {
    return(cat("  identt(df, lag: You decide or blank: automatic_allocation ) \n"))
  }

  var_name <- deparse(substitute(xdatas))
  tm_length <- length(xdatas)

  # --- [시차 계산 로직] ---
  if (tm_length <= 40) { lagx <- round(tm_length * (2/3), 0) } 
  else if (tm_length <= 60) { lagx <- round(tm_length * (1.2/2), 0) } 
  else if (tm_length <= 100) { lagx <- round(tm_length * (0.9/2), 0) } 
  else { lagx <- 20 } 
  if (!base::missing(lagx2)) lagx <- lagx2

  # --- [1. 데이터 계산] ---
  r_val <- c(); b_val <- c()
  for (k in 1:lagx) {
    r_val <- c(r_val, ac(xdatas, k))
    b_val <- c(b_val, pac(xdatas, k))
  }
  
  q_stat_vals <- c(); q_prob_vals <- c()
  for(i in 1:lagx){
    test <- Box.test(xdatas, lag=i, type='Ljung-Box')
    q_stat_vals <- c(q_stat_vals, test$statistic)
    q_prob_vals <- c(q_prob_vals, test$p.value)
  }

  # --- [2. 그래프 영역 설정: 1행 3열] ---
  op <- par(no.readonly = TRUE)
  par(mfrow = c(1, 3), mar = c(5, 4, 6, 1), cex.axis = 1)

  # --- [3. ACF Plot] ---
  bp <- barplot(rev(r_val), horiz = TRUE, col = "steelblue", xlim = c(-1, 1),
                border = NA, ylab = "Lags", xlab = "ACF",
                names.arg = rev(1:lagx), 
                main = paste0("Data: ", tm_length, "\n", var_name), 
                las = 1, cex.main = 0.8)
  box(); abline(v = 0); abline(v = c(2/sqrt(tm_length), -2/sqrt(tm_length)), lty = 3, col = "red")
  for(l in seq(0, lagx, by = 5)){
    idx <- which(rev(1:lagx) == l)
    if(length(idx) > 0) abline(h = bp[idx], lty = 3, col = "gray80")
  }

  # --- [4. PACF Plot] ---
  barplot(rev(b_val), horiz = TRUE, col = "steelblue", xlim = c(-1, 1),
          border = NA, ylab = "Lags", xlab = "PACF",
          names.arg = rev(1:lagx), 
          main = paste0("Data: ", tm_length, "\n", var_name), 
          las = 1, cex.main = 0.8)
  box(); abline(v = 0); abline(v = c(2/sqrt(tm_length), -2/sqrt(tm_length)), lty = 3, col = "red")
  for(l in seq(0, lagx, by = 5)){
    idx <- which(rev(1:lagx) == l)
    if(length(idx) > 0) abline(h = bp[idx], lty = 3, col = "gray80")
  }

  # --- [5. Q-stat Table Plot] ---
  plot(0, 0, type = "n", axes = FALSE, ann = FALSE, xlim = c(0, 1), ylim = c(min(bp), max(bp)))
  
  # 제목 위치와 별개로 표 헤더(Q.stat, Q.prob) 배치
  title(main = "Ljung-Box Q-test", cex.main = 0.8, line = 4) # 타이틀을 더 위로
  
  # 헤더 출력: line = 1 로 설정하여 데이터(bp) 영역보다 위쪽으로 배치
  mtext("Q.stat     Q.prob", side = 3, line = 1, cex = 0.9, adj = 0.5, font = 2)
  # 헤더 아래에 구분선 효과 (선택 사항)
  mtext("-------------------", side = 3, line = 0.2, cex = 0.8, adj = 0.5)

  rev_q_stat <- rev(q_stat_vals)
  rev_q_prob <- rev(q_prob_vals)
  
  for(i in 1:lagx) {
    text(x = 0.5, y = bp[i], 
         labels = sprintf("%.2f      %.4f", rev_q_stat[i], rev_q_prob[i]), 
         cex = 1.0, family = "mono", col = "black")
    
    if(rev(1:lagx)[i] %% 5 == 0) abline(h = bp[i], lty = 3, col = "gray80")
  }

  par(op)
}