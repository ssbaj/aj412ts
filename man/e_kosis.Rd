\name{e_kosis}
\alias{e_kosis}
\title{Loading KOSIS data}
\usage{
e_kosis()
}
\description{
Loading KOSIS data
}
\examples{

# KOSIS의 apkey 다운로드 받는 곳  
# https://kosis.kr/openapi/  
  library(kosis) 
  kosis.setKey(apiKey='당신의 API값을 입력하세요')
# KOSIS자료 Code Book  
# https://kosis.kr/openapi/devGuide/devGuide_0101List.do  
   
# orgIds = 조직 ID  
# prdSe = 수록주기: D(일), M, Q, H(3월), Y(년)  
  
 kosis데이터 <- getStatData(    
   orgId = '',   
   tblId = '',   
   prdSe = '',   
   itmId = '',   
   newEstPrdCnt = 숫자입력.따옴표 없음,  
   startPrdDe='',  
   endPrdDe='',  
   objL1='',  
   objL2='',  
   objL3=''  
 )  


}