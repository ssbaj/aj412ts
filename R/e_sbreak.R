# structuralbreak

e_sbreak<-function(explaining=0){
if(explaining==0) {
cat(" library(strucchange);library(changepoint); library(tidyverse); library(lubridate) ", '\n')
cat(" 			 ", '\n')
cat(" 			 ", '\n')
cat(" Result01 <- cpt.meanvar(Adata$growth, method = 'PELT')", '\n')
cat(" plot(Result01, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" slice(cpts(Result01))", '\n')
cat(" 			 ", '\n')
cat(" Result02 <- cpt.mean(Adata$growth, penalty = 'BIC', method = 'PELT')", '\n')
cat(" plot(Result02, type = 'l', cpt.col = 'blue', xlab = 'Index', cpt.width = 4)", '\n')
cat(" cpts(Result02)", '\n')
cat(" 			 ", '\n')
cat(" cusum <- efp(growth ~ lag(growth), type = 'OLS-CUSUM', data = Adata)", '\n')
cat(" plot(cusum)", '\n')
cat(" 			 ", '\n')
cat(" Result03 <- Fstats(Adata$growth ~ lag(Adata$growth, data=Adata))", '\n')
cat(" breakpoints(Result03)", '\n')
}}
