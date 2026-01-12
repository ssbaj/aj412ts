ccff <- function(x, y, lag.max = 10) {
  # 1. CCF 계산 (데이터 추출용, 그래프는 아직 그리지 않음)
  # ts.intersect 등을 자동으로 처리해주는 R 내장 ccf 함수 활용
  out <- ccf(x, y, lag.max = lag.max, plot = FALSE, na.action = na.pass)
  
  # 2. 데이터 벡터화 (Lag와 Correlation 값 추출)
  # as.vector를 사용하여 [n, 1, 1] 형태의 배열을 단순 벡터로 변환
  lags <- as.vector(out$lag)
  corrs <- as.vector(out$acf)
  
  # 3. 그래프 설정
  n <- length(x) # 데이터 개수 (유의수준 계산용)
  main_title <- paste("Cross-Correlation Function\n", 
                      deparse(substitute(x)), "&", deparse(substitute(y)))
  
  # 4. Barplot 그리기
  # horiz = TRUE: 막대를 가로로 눕힘 (SAS처럼 Lag가 수직축에 위치)
  # bp: 각 막대의 중심 y좌표를 저장
  bp <- barplot(corrs, 
                horiz = TRUE,          # 가로 막대 (수직 Lag 축)
                names.arg = lags,      # y축 라벨 (-10, -9 ... 10)
                col = "steelblue",     # 막대 색상
                border = NA,           # 테두리 없음
                xlim = c(-1, 1),       # 상관계수 범위
                xlab = "Correlation", 
                ylab = "Lag",
                main = main_title,
                las = 1)               # 축 라벨 가로쓰기
  
  # 테두리 박스
  box()
  
  # 5. 기준선 추가
  # (1) 상관계수 0 기준선 (검은 실선)
  abline(v = 0, col = "black")
  
  # (2) 유의수준 점선 (빨간 점선, +/- 2/sqrt(n))
  ci <- 2 / sqrt(n)
  abline(v = c(-ci, ci), col = "red", lty = 2)
  
  # (3) [요청사항] 각각의 Lag에 해당되는 수평 Dotted Line
  # bp 변수에 저장된 y좌표를 사용하여 모든 막대 위치에 선을 그립니다.
  abline(h = bp, lty = 3, col = "gray50")
  
  # (선택사항) 막대를 선 위에 다시 그려서 글자나 선이 막대를 가리지 않게 하려면 
  # 아래 코드를 주석 해제하세요. (지금은 선이 막대 위로 지나가 가독성을 높임)
  # barplot(corrs, horiz=TRUE, names.arg=lags, col="steelblue", border=NA, add=TRUE, axes=FALSE)
}