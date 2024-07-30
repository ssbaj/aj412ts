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
   
# KOSIS자료 Code Book  
# https://kosis.kr/openapi/devGuide/devGuide_0101List.do  
   
  서비스뷰 코드(vwCd)   
  · MT_ZTITLE : 국내통계 주제별   
  · MT_OTITLE : 국내통계 기관별   
  · MT_GTITLE01 : e-지방지표(주제별)   
  · MT_GTITLE02 : e-지방지표(지역별)   
  · MT_CHOSUN_TITLE : 광복이전통계(1908~1943)   
  · MT_HANKUK_TITLE : 대한민국통계연감   
  · MT_STOP_TITLE : 작성중지통계   
  · MT_RTITLE : 국제통계   
  · MT_BUKHAN : 북한통계   
  · MT_TM1_TITLE : 대상별통계   
  · MT_TM2_TITLE : 이슈별통계   
  · MT_ETITLE : 영문 KOSIS   
   
library(kosis) 
topic<-getStatList(vwCd='MT_ZTITLE') 
kosis.setKey('MDE3ZDM1M2Y0MWIxNzI5OGIzZGExNmM1NWIzNTNlMjA=') 
 
topic_sub0<-getStatList(vwCd='MT_ZTITLE') 
topic_sub1<-getStatList(vwCd='MT_ZTITLE', parentListId='S2') 
topic_sub2<-getStatList(vwCd='MT_ZTITLE', parentListId='S2_11', ) 
  
starttime<-'202401'  
endtime<-'202406'  
  
# orgIds = 조직 ID  
# prdSe = 수록주기: D(일), M, Q, H(3월), Y(년)  
  
# 전체국가 추출  
data1<-getStatData(orgId='134', tblId='DT_134001_002', prdSe='M',   
       startPrdDe=starttime, endPrdDe=endtime, objL1='All')  
  
# 중국 추출  
data2<-getStatData(orgId='134', tblId='DT_134001_002', prdSe='M',   
       startPrdDe=starttime, endPrdDe=endtime, objL1='CN')  
  
# 중국, 미국, 일본 추출  
plist<-list()  
country<-c('CN', 'US', 'JP')  
for(i in country){  
tmp<-getStatData(orgId='134', tblId='DT_134001_002', prdSe='M',   
       startPrdDe=sper, endPrdDe=eper, objL1=i )   
plist[[i]] <-tmp  
 }  
# plits에 중국, 미국, 일본 자료 저장  

}