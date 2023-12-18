Areport<-function(Re2XX){

coef<-Re2XX$coef
s.e<-(sqrt(diag(Re2XX$var.coef)))

m<-length(coef)
n<-length(s.e)

r<-c()
if(m !=n ) {
for(i in 1:m)
  {if(coef[i] != 0) {r<-c(r, coef[i])}
   else next
  }
coef <- r
}


tmp<-base::summary(Re2XX)

if(Re2XX[9]$call[1]=="arima()" ) { n_calc<-tmp$nobs}

if(Re2XX[9]$call[1]=="auto.arima()" ) { n_calc<-tmp$nobs}

if(Re2XX[9]$call[1]=="Arima()" ) { n_calc<-tmp$nobs}

if(Re2XX[9]$call[1]=="arimax()") {n_calc<-( length(Re2XX$residuals)-Re2XX$arma[6]-Re2XX$arma[7]*Re2XX$arma[5])  }


df_value <- n_calc -length(coef)

t_val <- coef/s.e
p_val <-  round( ( 1- pt( abs( t_val ) , df_value ) ), 4)

options(scipen=100)
coeff<-as.data.frame(coef)
s.e<-as.data.frame(s.e)
t_value<-as.data.frame(t_val)
p_value<-as.data.frame(p_val)
temp<-cbind(coef, s.e , t_val, p_val)

cat('\n')
if(Re2XX[9]$call[1]=="auto.arima()" ) {print(Re2XX[9]$call[1:2]) } else { print(Re2XX[9]$call) }

cat('\n')

if(class(Re2XX$bic)=="numeric") { cat('Included observations for calculation: ', tmp$nobs , '\n') }
else {cat('Included observations for calculation: ', n_calc , '\n')}

cat('Number of Coefficients: ', length(coef),'\n')
cat('Log-likelihood: ', Re2XX$loglik, '\n')
cat('AIC: ', Re2XX$aic, '\n')
if(class(Re2XX$bic)=="numeric") {cat('BIC: ', Re2XX$bic, '\n') }

cat(' ', '\n')
temp<-round(temp,4)
print(temp)
}
