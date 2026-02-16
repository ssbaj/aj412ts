prewhiten22 <- function (x, y, x.model = ar.res, showxy = FALSE, ylab = "CCF", ...) 
{
if (base::missing(x)) {
        cat("  TSA 패키지: Transfer function model을 위한 prewhiten을 수정한 명령문 ----- ", '\n' )
        cat("  fit_x = Arima(x, order=c(1,0,0) ) ", '\n' )
	cat('  prewhiten22(x, y, x.model = fit_x) ', '\n' )
	cat("      ", '\n' )
	cat('  또는, eta1과 eta2를 구해 ccff(eta1, eta2)로 확인하려면 ... ', '\n' )
	cat("  prewhiten22(x, y, x.model = fit_x, showxy=TRUE)  ", '\n' )
	return(cat('   ')) }

if (!require(TSA)) {
install.packages("TSA")
}

suppressPackageStartupMessages(library("TSA"))

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
    ccf.xy = ccff(x = x, y = y)
    invisible(list(ccf = ccf.xy, model = x.model))
    tmp.df<-cbind(x, y)
    if(showxy==TRUE) {return(tmp.df)}
}