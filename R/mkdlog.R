mkdlog <- function(name_dataset, select_columns) {
  
  if (base::missing(name_dataset)) {
	  return( cat("  df<-mkdlog(df, 2)  *NOTE: making differenced log-variable, the 2nd column of dataset ", '\n') )
	}
  
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

