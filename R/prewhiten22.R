prewhiten22 <- function (x, y, x.model = ar.res, ylab = "CCF", ...) 
{
if (base::missing(x)) {
        cat("  TSA 패키지: Transfer function model을 위한 prewhiten 명령문 ----- ", '\n' )
        cat("   ", '\n' )
        cat("  set.seed(123) ", '\n' )
        cat("  x = arima.sim(n = 200, list(ar = 0.7)) ", '\n' )
        cat("  y = lag(x, 3) * 0.5 + rnorm(200, sd = 0.5) ", '\n' )
	cat("  fit_x = arima(x, order = c(1, 0, 1))  ", '\n' )
	cat('  prewhiten(x, y, x.model = fit_x) ', '\n' )
	cat("      ", '\n' )
	cat('  또는, eta1과 eta2를 구해 ccff(eta1, eta2)로 확인하려면 ... ', '\n' )
	cat("  prewhiten_x <- fit_x$resid  ", '\n' )
	cat('  prewhiten_y <- prewhiten(x, y, x.model = fit_x) ')
	return(cat('  ccff(prewhiten_x, prewhiten_y) ')) }

    filter.mod = function(x, model) {
        if (length(model$Delta) >= 1) 
            x = stats::filter(x, filter = c(1, -model$Delta), 
                method = "convolution", sides = 1)
        if (length(model$theta) >= 1 && any(model$theta != 0)) 
            x = stats::filter(x, filter = -model$theta, method = "recursive", 
                sides = 1)
        if (length(model$phi) >= 1 && any(model$phi != 0)) 
            x = stats::filter(x, filter = c(1, -model$phi), method = "convolution", 
                sides = 1)
        x
    }
    if (!missing(x.model)) {
        x = filter.mod(x, model = x.model$model)
        y = filter.mod(y, model = x.model$model)
    }
    else {
        ar.res = ar.ols(x, ...)
        x = stats::filter(x, filter = c(1, -ar.res$ar), method = "convolution", 
            sides = 1)
        y = stats::filter(y, filter = c(1, -ar.res$ar), method = "convolution", 
            sides = 1)
    }
    ccf.xy = ccff(x = x, y = y, na.action = na.omit, ylab = ylab, 
        ...)
    invisible(list(ccf = ccf.xy, model = x.model))
    return(y)
}
