\name{e_datagokr}
\alias{e_datagokr}
\title{Download datagokr data}
\usage{
e_datagokr()
}
\description{
Download datagokr data
}
\examples{

# 사이트 주소 예시 ---- 
https://api.odcloud.kr/api/HousePriceTrendSvc/v1/getHousePriceIndex?page=1&perPage=10&cond%5BREGION_CD%3A%3AEQ%5D=11000&cond%5BRESEARCH_DATE%3A%3ALTE%5D=202202&cond%5BRESEARCH_DATE%3A%3AGTE%5D=202201&cond%5BAPT_TYPE%3A%3AEQ%5D=0&cond%5BTR_GBN%3A%3AEQ%5D=S&serviceKey=서비스키


rm(list=ls())
library(knitr); library(XML); library(RCurl); library(jsonlite); 
library(readsdmx); library(httr); library(tidyverse); library(lubridate)

mykey <- " Input your api key  "

reqURL<-"https://api.odcloud.kr/api/HousePriceTrendSvc/v1/getHousePriceIndex"

x<-c(11000, 41000, 28000, 26000, 27000, 29000, 30000, 31000, 36000, 42000, 43000, 44000, 45000, 46000, 47000, 48000, 50000)

plist<-list()

for(i in x){

param<-c(
"page","perPage","cond%5BREGION_CD%3A%3AEQ%5D","cond%5BRESEARCH_DATE%3A%3ALTE%5D", "cond%5BRESEARCH_DATE%3A%3AGTE%5D","cond%5BAPT_TYPE%3A%3AEQ%5D","cond%5BTR_GBN%3A%3AEQ%5D","serviceKey")

values<-c(1, 500, i, 202304, 201801, 0, "S" , mykey)
req<-paste(reqURL, paste(param, values, sep="=", collapse="&"), sep="?")

temp_file<-getURL(req)
temp_file
json_file<-fromJSON(temp_file)
json_file
plist[[i]]<-as.data.frame(json_file)


}