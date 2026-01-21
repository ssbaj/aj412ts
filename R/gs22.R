#' plotting seasonal plot
gs22 <- function (x, season.labels = NULL, year.labels = TRUE, year.labels.left = FALSE, 
    type = NULL, col = NULL, continuous = FALSE, polar = FALSE, 
    labelgap = 0.04, text.size = 3, ...) 
{   # <--- 여기에 중괄호 시작! (이 위치가 중요합니다)

    if (base::missing(x)) {
         return(cat("  gs22(ts함수로 시간이 명시된 시계열자료) \n"))
    }

    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 is needed for this function to work. Install it via install.packages(\"ggplot2\")", 
            call. = FALSE)
    }
    if (!inherits(x, "ts")) {
        stop("autoplot.seasonplot requires a ts object, use x=object")
    }
    if (!is.null(type)) {
        message("Plot types are not yet supported for seasonplot()")
    }
    
    # 데이터 준비
    s <- round(frequency(x))
    if (s <= 1) {
        stop("Data are not seasonal")
    }
    xname <- deparse(substitute(x))
    tspx <- tsp(x)
    x <- ts(x, start = tspx[1], frequency = s)
    data <- data.frame(y = as.numeric(x), year = trunc(round(time(x), 
        8)), cycle = as.numeric(cycle(x)), time = as.numeric((cycle(x) - 
        1)/s))
    
    data$year <- if (continuous) {
        as.numeric(data$year)
    }
    else {
        as.factor(data$year)
    }
    
    if (polar) {
        startValues <- data[data$cycle == 1, ]
        if (data$cycle[1] == 1) {
            startValues <- startValues[-1, ]
        }
        startValues$time <- 1 - .Machine$double.eps
        levels(startValues$year) <- as.numeric(levels(startValues$year)) - 
            1
        data <- rbind(data, startValues)
    }
    
    # 플롯 생성
    p <- ggplot2::ggplot(ggplot2::aes(x = .data[["time"]], y = .data[["y"]], 
        group = .data[["year"]], colour = .data[["year"]]), data = data, 
        na.rm = TRUE)
    p <- p + ggplot2::geom_line()
    
    # 색상 설정
    if (!is.null(col)) {
        if (is.numeric(col)) {
            col <- palette()[(col - 1)%%(length(palette())) + 
                1]
        }
        if (continuous) {
            p <- p + ggplot2::scale_color_gradientn(colours = col)
        }
        else {
            ncol <- length(unique(data$year))
            if (length(col) == 1) {
                p <- p + ggplot2::scale_color_manual(guide = "none", 
                  values = rep(col, ncol))
            }
            else {
                p <- p + ggplot2::scale_color_manual(values = rep(col, 
                  ceiling(ncol/length(col)))[1:ncol])
            }
        }
    }
    
    # 연도 라벨 데이터 생성
    if (year.labels) {
        yrlab <- stats::aggregate(time ~ year, data = data, FUN = max)
        yrlab <- cbind(yrlab, offset = labelgap)
    }
    if (year.labels.left) {
        yrlabL <- stats::aggregate(time ~ year, data = data, 
            FUN = min)
        yrlabL <- cbind(yrlabL, offset = -labelgap)
        if (year.labels) {
            yrlab <- rbind(yrlab, yrlabL)
        }
    }
    
    # 연도 라벨 텍스트 추가
    if (year.labels || year.labels.left) {
        yrlab <- merge(yrlab, data)
        yrlab$time <- yrlab$time + yrlab$offset
        p <- p + ggplot2::guides(colour = "none")
        p <- p + ggplot2::geom_text(ggplot2::aes(x = .data[["time"]], 
            y = .data[["y"]], label = .data[["year"]]), 
            data = yrlab, 
            size = text.size, 
            hjust = -0.2, 
            fontface = "plain")
    }
    
    # 축 라벨 결정
    if (s == 12) {
        labs <- month.abb
        xLab <- "Month"
    }
    else if (s == 4) {
        labs <- paste("Q", 1:4, sep = "")
        xLab <- "Quarter"
    }
    else if (s == 7) {
        labs <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
        xLab <- "Day"
    }
    else if (s == 52) {
        labs <- 1:s
        xLab <- "Week"
    }
    else if (s == 24) {
        labs <- 0:(s - 1)
        xLab <- "Hour"
    }
    else if (s == 48) {
        labs <- seq(0, 23.5, by = 0.5)
        xLab <- "Half-hour"
    }
    else {
        labs <- 1:s
        xLab <- "Season"
    }
    
    if (!is.null(season.labels)) {
        if (length(season.labels) != length(labs)) {
            warning(paste0("Provided season.labels have length ", 
                length(season.labels), ", but ", length(labs), 
                " are required. Ignoring season.labels."))
        }
        else {
            labs <- season.labels
        }
    }
    
    breaks <- sort(unique(data$time))
    if (polar) {
        breaks <- head(breaks, -1)
        p <- p + ggplot2::coord_polar()
    }
    
    p <- p + ggplot2::scale_x_continuous(breaks = breaks, minor_breaks = NULL, 
        labels = labs,
        expand = ggplot2::expansion(mult = c(0.05, 0.15))) # 오른쪽 여백 확보
    
    p <- p + ggplot2::labs(title = paste("Seasonal plot:", xname), 
                           x = xLab, 
                           y = NULL) +
             ggplot2::theme_bw() # 깔끔한 테마 적용
    
    return(p)
}