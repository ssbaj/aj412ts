uroot <- function (x, lag = 4, method = c("adf"), lag.short = FALSE, output = TRUE) 
{
    if (base::missing(x)) {
        # ... (중략: 도움말 출력 부분은 동일) ...
        cat(" \033[1;34m# TS자료: 월 -> 분기: r<-aggregate(ir, nfrequency=4)/3  ", "\n")
        # ... (중략) ...
        return(cat("    uroot(데이터셋$변수, lag=4, method=c('adf', 'pp', 'kpss'), lag.short=TRUE, out=TRUE) "))
    }

    # --- [데이터 변환 코드 삽입 시작] ---
    # x가 데이터프레임의 열(df$x)이나 리스트 형태일 경우를 대비해 수치형 벡터로 강제 변환
    # stationary.test는 결측치가 있으면 오류가 발생하므로 na.omit 추가
    x <- as.numeric(na.omit(as.vector(x)))
    # --- [데이터 변환 코드 삽입 완료] ---

    if (!require(dplyr)) {
        cat("Automatically Installing dplyr package because", "\n")
        # ... (이하 패키지 설치 및 실행 로직 동일) ...
        install.packages("dplyr")
    }
    
    if (!require(aTSA)) {
        cat("Installing aTSA package for uroot .....", "\n")
        install.packages("aTSA")
    }
    
    suppressPackageStartupMessages(library("aTSA"))
    
    cat("\033[1;33m# p값이 0.05보다 크면 NON-Stationary → 차분 \033[0m", "\n")
    cat("\033[1;33m# 차분금지: 자체가 비율(ratio)인 자료 금리(%), 고령화비율(%), -경상수지 \033[0m", "\n")
    
    return(stationary.test(x, nlag = lag, method = method, lag.short = lag.short, 
        output = output))
}
