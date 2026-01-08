\name{diff22}
\alias{diff22}
\title{Make differenced vector}
\usage{
diff22(dataset, variable, difference number)
}
\description{
Make differenced vector.
}
\examples{

df dataset에 변수 A, B가 있다
A변수의 분기별 차분 자료를 생성

df <- diff22(df, A, 1)  # 1차 차분
df <- diff22(df, A, 4)  # 분기 자료의 분기별 차분
df <- diff22(df, A, 12)  # 계절 자료의 계절차분

}