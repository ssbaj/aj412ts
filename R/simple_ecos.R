simple_ecos <- function(ecos_dataset, start_year, end_year = "9999") 
{
    if (base::missing(ecos_dataset)) {
        cat("  Data는 순수한 numeric이면 충분함. ts지정은 할 필요 없음 ", "\n")
        cat("  ECOS자료에서 원하는 연도부터 자료 추출 / QoQYoY자료가 자동 첨부됨 ", "\n")
        cat("  Monthly/Quarterly/Yearly자료는 자동인식함", "\n")
        cat("  예제1: 2018년 부터 시작하는 자료만 남길 경우 ", "\n")
        cat("         Adata<-simple_ecos(gdp, '2018') ", "\n")
        cat("  예제2: 2018년~2020년 자료만 남길 경우 ", "\n")
        return(cat("       Adata<-simple_ecos(gdp, '2018', '2020') ", "\n"))
    }
    
    if (!require(dplyr)) {
        install.packages("dplyr")
    }
    suppressPackageStartupMessages(library("dplyr"))

    simple_ecos2 <- function(ecos_dataset, start_year, cycle = 1) {
        mkdate_series <- function(df, start_y, start_m = 0, mq = 0) {
            df <- as.data.frame(df)
            n <- nrow(df)
            tmp1 <- rep(NA, n); tmp2 <- rep(NA, n); tmp3 <- rep(NA, n)
            
            if (start_m > 0 & mq == 12) {
                for (i in 1:n) {
                    if (start_m == 12) {
                        tmp2[i] <- start_m; start_m <- 1; tmp1[i] <- start_y; start_y <- start_y + 1
                    } else {
                        tmp2[i] <- start_m; start_m <- start_m + 1; tmp1[i] <- start_y
                    }
                }
                tmp1 <- as.character(tmp1); tmp2 <- sprintf("%02d", as.numeric(tmp2))
            }
            if (start_m > 0 & mq == 4) {
                for (i in 1:n) {
                    if (start_m == 12) {
                        tmp2[i] <- start_m; start_m <- 3; tmp1[i] <- start_y; start_y <- start_y + 1
                    } else {
                        tmp2[i] <- start_m; start_m <- start_m + 3; tmp1[i] <- start_y
                    }
                }
                tmp1 <- as.character(tmp1); tmp2 <- sprintf("%02d", as.numeric(tmp2))
            }
            if (start_m != 0) {
                for (i in 1:n) { tmp3[i] <- paste0(tmp1[i], "-", tmp2[i], "-01") }
                df$DATE <- as.Date(tmp3)
            } else {
                for (i in 1:n) { tmp3[i] <- paste0((start_y + i - 1), "-12-31") }
                df$DATE <- as.Date(tmp3)
            }
            return(df %>% relocate(DATE))
        }

        df <- as.data.frame(ecos_dataset)
        # 데이터 구조 유연성 확보 (time, data_value 컬럼 추출)
        df <- df[, c(grep("time", names(df)), grep("data_value", names(df)))]
        names(df) <- c("time", "data_value")
        
        n <- nrow(df)
        nx <- 1 # 기본값 설정
        for (i in 1:n) {
            if (substring(df$time[i], 1, 4) == start_year) {
                nx <- i
                break
            }
        }
        
        # 주기(Cycle) 판단 로직
        sample_time <- as.character(df$time[nx])
        if (nchar(sample_time) == 4) {
            cycle <- 1
        } else if (grepl("Q", sample_time)) {
            cycle <- 4
            start_m <- as.numeric(substring(sample_time, 6, 6)) * 3
        } else {
            cycle <- 12
            start_m <- as.numeric(substring(sample_time, 5, 6))
        }

        df <- df[nx:n, ]
        colnames(df) <- c("time", "data")
        
        # 증감률 및 변동분 계산
        QoQYoY <- c(rep(NA, cycle), diff(df$data, lag = cycle))
        percentchange <- c(NA, (df$data[-1] / df$data[-nrow(df)]) - 1)
        df <- cbind(df, QoQYoY, percentchange)

        # 날짜 처리
        if (cycle != 1) {
            df <- mkdate_series(df, as.numeric(start_year), start_m, cycle)
        } else {
            df$DATE <- as.Date(paste0(df$time, "-12-31"))
        }
        return(df %>% relocate(DATE, .before = "time"))
    }

    # 메인 로직: end_year 처리 부분 수정
    if (end_year == "9999") {
        return(simple_ecos2(ecos_dataset, start_year))
    } else {
        tmp_df2 <- simple_ecos2(ecos_dataset, start_year)
        ncount <- nrow(tmp_df2)
        target_next_year <- as.character(as.numeric(end_year) + 1)
        
        nx_endyear <- ncount + 1 # 기본값을 마지막 행 다음으로 설정
        
        for (i in 1:ncount) {
            if (substring(tmp_df2$time[i], 1, 4) == target_next_year) {
                nx_endyear <- i
                break
            }
        }
        # nx_endyear가 발견되지 않으면 데이터 전체(1:ncount)를 반환함
        return(tmp_df2[1:(nx_endyear - 1), ])
    }
}