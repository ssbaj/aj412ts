prewhiten22 <- function (x, y, x.model, showxy = FALSE) 
{
    # 1. x.model에서 필터 계수 추출
    # Arima 객체에서 ar, ma, sar, sma 계수를 가져옵니다.
    model_list <- x.model$model
    
    # 2. x와 y를 동일한 필터로 거름 (Prewhitening)
    # stats::filter는 결과를 ts 객체로 반환하며, 
    # 여러 필터를 거치면 구조가 복잡해질 수 있어 확실히 정리해야 합니다.
    
    pw_filter <- function(data, mod) {
        filtered <- data
        # 차분(Delta) 처리
        if (length(mod$Delta) >= 1) 
            filtered <- stats::filter(filtered, filter = c(1, -mod$Delta), method = "convolution", sides = 1)
        # AR(phi) 처리
        if (length(mod$phi) >= 1 && any(mod$phi != 0)) 
            filtered <- stats::filter(filtered, filter = c(1, -mod$phi), method = "convolution", sides = 1)
        # MA(theta) 처리 (Recursive)
        if (length(mod$theta) >= 1 && any(mod$theta != 0)) 
            filtered <- stats::filter(filtered, filter = -mod$theta, method = "recursive", sides = 1)
        return(filtered)
    }

    x_pw <- pw_filter(x, model_list)
    y_pw <- pw_filter(y, model_list)

    # 3. 중요: ts.intersect를 사용해 시점을 맞추고 NA 제거
    # 여기서 데이터가 matrix 형태가 되는데, 이를 각각 분리해야 ccf 에러가 안 납니다.
    combined <- ts.intersect(x_pw, y_pw)
    
    # 4. 일변량 벡터로 강제 변환하여 CCF 실행
    # ccf(x, y)에서 x와 y는 반드시 '단일 열' 벡터여야 합니다.
    final_x <- as.numeric(combined[, 1])
    final_y <- as.numeric(combined[, 2])

    # 그래프 그리기
    ccf_result <- ccff(final_x, final_y)

    if (showxy == TRUE) {
        return(combined)
    }
    
    return(invisible(ccf_result))
}
