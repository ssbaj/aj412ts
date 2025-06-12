e_sbreak<-function(explaining=0){

if(explaining==0) {

cat(" 			 ", '\n')
cat(" library(aj412s2); library(aj412ts); library(svars) ", '\n')
cat(" library(strucchange);library(changepoint); library(tidyverse) ", '\n')
cat(" library(lubridate) ", '\n')
cat(" 			 ", '\n')
cat(" Result01 <- cpt.meanvar(Adata$growth, method = 'PELT')", '\n')
cat(" plot(Result01, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" change_point_indices <- cpts(Result01)", '\n')
cat(" df %>% slice(change_point_indices)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')
cat(" Result02 <- cpt.mean(Adata$growth, penalty = 'BIC', method = 'PELT')", '\n')
cat(" plot(Result02, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" cpts(Result02)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')
cat(" Result03 <- Fstats(tmp$dlgni ~ lag(tmp$dlm2), data=tmp)  ", '\n')
cat(" breakpoints(Result03)  ", '\n')
cat(" plot(Result03) ", '\n')
cat(" 			 ", '\n')
cat(" cusum <- efp(growth ~ lag(growth), type = 'OLS-CUSUM', data = Adata)", '\n')
cat(" plot(cusum)", '\n')
cat(" grid(col = 'lightgray', lty = 'dotted', lwd = 1)", '\n')
cat(" 			 ", '\n')

}

}
