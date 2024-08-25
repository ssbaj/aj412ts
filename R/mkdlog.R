mkdlog <- function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
	  return( cat("  df<-mkdlog(df, 'variable name')  *NOTE: making differenced log-variable, the 2nd column of dataset ", '\n') )
	}
  
  
  ##-----------------------------------
# 변수명을 컬럼 번호로 변경시키는 함수
##------------------------------------
find_col2<-function(DataSet, index_id ){
  tmp_colnames<-colnames(DataSet)
  n<-length(tmp_colnames)  # DataSet의 총변수 갯수
  
  for(i in 1:n){
    if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
  }
}

##----------------------------------------
# find_col2()를 사용해 컬럼번호 찾기
if(is.numeric(select_columns)==F) {select_columns<-find_col2(name_dataset, select_columns) }

  
  tmp<-(name_dataset[select_columns])
  if( (sum(tmp<0) >0 ) |( sum(tmp<1 & tmp>0) >0 ) ) {
  cat('There are negative numbers or fraction numbers', '\n')
  break
  }
  
  tmp<-as.data.frame(log(tmp))
  tmp2<- tmp - dplyr::lag(tmp)
    
  ## Changing variable names
  colnames(tmp2) <- paste0( "dlog_" , colnames(tmp) , sep='')
  
  name_dataset2<-cbind(name_dataset, tmp2)
  name_dataset2<-as.data.frame(name_dataset2)
  return(name_dataset2)
}

