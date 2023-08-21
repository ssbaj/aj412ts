# e_arimax()

e_arimax<-function(explaining=0){
if(explaining==0) {
cat(" library(TSA); library(astsa) # astsa = including lead command ", '\n')
cat(" sales_140<-window(sales,end=140) ", '\n')
cat(" lead_140<-window(lead,end=140) ", '\n')
cat("  ", '\n')
cat(" lead_140_Z <- lead_140 - mean(lead_140) ", '\n')
cat(" sales_140_D <- diff(sales_140) ", '\n')
cat(" lead_140_D <- diff(lead_140_Z) ", '\n')
cat("  ", '\n')
cat(" re <- arimax( ", '\n')
cat("    sales_140_D, ", '\n')
cat("    order=c(0,0,1), ", '\n')
cat("    include.mean=TRUE, ", '\n')
cat("    xtransf=lead_140_D, ", '\n')
cat("    transfer=list(c(1,3)), ", '\n')
cat("    fixed=c(NA, NA,   NA,  0,   0,   0, NA), ", '\n')
cat("    # 계수: MA1,상수항,T1-AR1, T1-MA0, T1-MA1, T1-MA2, T1-MA3 ", '\n')
cat("    method='ML') ", '\n')
}}

