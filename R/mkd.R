mkd <- function(name_dataset, select_columns) {

  if (base::missing(name_dataset)) {
	  return( cat("  df<-mkd(df, 2)  *NOTE: making differenced variable, the 2nd column of dataset ", '\n') )
	}

  tmp<-(name_dataset[select_columns])
  tmp<-as.data.frame(tmp)
  tmp2<- tmp - dplyr::lag(tmp)
    
  ## Changing variable names
  colnames(tmp2) <- paste0( "d_" , colnames(tmp) , sep='')
  
  name_dataset2<-cbind(name_dataset, tmp2)
  name_dataset2<-as.data.frame(name_dataset2)
  return(name_dataset2)
}
