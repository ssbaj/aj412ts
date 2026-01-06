#' cc() Function
#' 
#' This function makes CCF graph
#' 
cc=function(x,y,k=1){
                    n=length(x)
                    mx=mean(x); my=mean(y)
                    sx=sqrt(sum((x-mx)^2));
                    sy=sqrt(sum((y-my)^2))
                    if(k==0)  {x=x[1:n];y=y[1:n] } else{x=x[1:(n-k)] ; y=y[(k+1):n] }
                    cxy=sum((x-mx)*(y-my))
                    cxy/(sx*sy)
    }
					  