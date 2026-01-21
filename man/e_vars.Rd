\name{e_vars}
\alias{e_vars}
\title{e_vars}
\usage{e_vars}
\description{
Explanation on the Vector Auto-regression
}
\examples{

type 옵션은 VAR 모형에서 매개변수 추정 방법을 선택하는 데 사용됩니다.
기본값으로 type="const"가 설정되어 있습니다. 다른 가능한 값은 "trend"와 "both"입니다.
type="const": VAR 모형에서 상수항만을 포함합니다.
type="trend": VAR 모형에서 선형 추세만을 포함합니다.
type="both": VAR 모형에서 상수항과 선형 추세를 모두 포함합니다.

p 옵션은 VAR 모형의 자기회귀 차수를 결정하는 데 사용됩니다. 
p=2이면 2년 전의 데이터까지 차수로 설정됩니다.

trend 옵션은 VAR 모형에 선형 추세를 포함하는 데 사용됩니다. 
trend 옵션은 type = "trend" 또는 type = "both" 일 때만 사용됩니다. 
trend 옵션은 다음과 같은 값 중 하나를 가질 수 있습니다.
trend = "nc": 선형 추세 없음.
trend = "c": 상수항만 포함.
trend = "trend": 선형 추세만 포함.
trend = "ct": 상수항과 선형 추세를 모두 포함.
예를 들어, type = "both" 및 trend = "ct"로 설정하여 VAR 모형에서 
상수항과 선형 추세를 모두 포함하도록 지정할 수 있습니다.

library(vars)
# 데이터 로드
data("Canada")

# VAR 모형 적합
fit <- VAR(Canada, p = 2, type = "both", trend = "ct")
summary(fit)


library(vars)
# 자기회귀 차수를 결정하는 방법: p(차수)를 결정하는 데 사용
re.info<-VARselect(Bdata[,c(1:2)], lag.max=5, type='const')
re.info$selection

# 자기회귀 차수를 결정하는 방법: 자동 p값을 설정할 수 있음
Re1 <- VAR(Bdata, type='const', lag=5) # auto selecting lags
Re2 <- VAR(Bdata, type='const', p=2) # designating lags

# VAR분석
re.var<-VAR(Bdata[,c(1:2)], p=1, type='const')

# Granger Causility 검사를 한다
re.cause.gni_pc <- causality(re.var, cause='gni_pc')
re.cause.m2_pc <- causality(re.var, cause='m2_pc')

## 충격반응함수(irf: Impuse Response Function)
irf.gni<-irf(Re2, impulse='m2_pc', response='gni_pc', 
            n.ahead=40, boot=TRUE)
plot(irf.gni, ylab='GNI반응', main='M2 Shock') 
irf.M2<-irf(Re2, impulse='gni_pc', response='m2_pc', 
            n.ahead=40, boot=TRUE) 
plot(irf.M2, ylab='M2반응', main='GNI Shock') 

## FEVD: Forecasting Error Variance Decomposition 
Re2.decomp<-fevd(Re2, n.ahead=10) 
Re2.decomp 

## Forecasting:
predictions <- predict(Re2, n.ahead=8, ci=0.95 ) 
predictions

## SVAR
amat <- diag(1, 2)
amat[lower.tri(amat)] <- NA
svar_Amodel <- SVAR(Re1, Amat = a, max.iter = 1000)
svar_Amodel
svar_Bmodel=solve(svar_Amodel)

bmat <- diag(1, 2)
bmat[lower.tri(bmat)] <- NA
svar_Bmodel <- SVAR(Re1, Bmat = bmat)
svar_Bmodel

}
