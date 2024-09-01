\name{simple_ecos}
\alias{simple_ecos}
\title{make ecos data simple}
\usage{
simple_ecos()
}
\description{
make ecos data simple
}
\examples{
Data는 순수한 numeric이고 ts지정은 않함  
  ECOS자료에서 원하는 연도부터 자료 추출 / YoY자료가 자동 첨부됨  
  Monthly/Quarterly/Yearly자료는 자동인식함 
  예제1: 2018년 부터 시작하는 자료만 남길 경우  
         Adata<-simple_ecos(gdp, '2018')  
  예제1: 2018년~2020년 자료만 남길 경우  
       Adata<-simple_ecos(gdp, '2018', '2020')
	   
}