#' max_ccf() Function
#' 
#' This function calculate ccf
#' 

max_ccf=function(s_x, s_y){
  
   if (base::missing(s_x)) {
	    cat("  max_ccf(x, y))", '\n')
		return(cat("  cor:0.84/lag:-4 = 상관계수는 0.84이고 x가 y보다 4일 뒤쳐져 있다 "))  }

  options(digits=3)
  s_d<-ccf(s_x,s_y, plot=F)
  cor=s_d$acf[,,1]
  lag=s_d$lag[,,1]
  res=data.frame(cor, lag)
  res_max=res[which.max(res$cor),]
  return(res_max) }

