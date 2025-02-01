uroot <- function(dataset_name , ... , nlag=4 , method=c("adf"), lag.short=TRUE, output=TRUE) {

if (base::missing(dataset_name)) {
            cat(' # How to use --------------------------   ', '\n')
	    return(cat("  uroot(데이터셋, 변수, nlag=4, method=c('adf', 'pp', 'kpss'), lag.short=TRUE, out=TRUE) "))  }

  if (!require(dplyr)) {
    cat('Automatically Installing dplyr package because','\n')
    cat('dplyr is necessary for this function','\n')
    cat('If an error occurs, connect to the network','\n')
    install.packages("dplyr")
  }
  
  suppressPackageStartupMessages(library("dplyr"))
  
  # find_col2함수 ----------------
  find_col2<-function(dataset_name, index_id ){
    tmp_colnames<-colnames(dataset_name)
    n<-length(tmp_colnames) # DataSet의 총변수 갯수
    for(i in 1:n){
      if(index_id==tmp_colnames[i]) {return(as.numeric(i))}
    }
  }
  
  ##--------------------------------
  # 변수명을 읽는 함수
  dataset_name<-as.data.frame(dataset_name)
  var_names <- as.character(substitute(list(...)))[-1]
  print(var_names)
  no_var_names<-length(var_names)
  
  r=c()
  
  for(i in 1:no_var_names){
    t_counter<-find_col2(dataset_name, var_names[i])
    r=c(r, t_counter)
  }
  
  r_var_index_number <- r
  
##---------------------------------

x<-dataset_name[, c(r_var_index_number)]

if (!require(aTSA)) {
    cat('Installing aTSA package for uroot .....', '\n')
    install.packages("aTSA")
  }
  
suppressPackageStartupMessages(library("aTSA"))

return(stationary.test(x, method=method, nlag=nlag, lag.short=lag.short, output=output))
}

