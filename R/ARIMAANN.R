ARIMAANN<-function(data,h){
  pp=auto.arima(data)# ARIMA model fitting
  summary(pp)
  kk=pp$residuals # Residuals obtained from the fitted ARIMA model
  kk1=pp$fitted # Fitted values of ARIMA model
  x <- as.ts(kk)
  w=terasvirta.test(x)# Checking the suitability of data for hybrid modelling
  p1=w$p.value
  if (p1>0.05){
    Test_Result<- (" Data is not suitable for hybrid modelling")
  } else {
    Test_Result<- ("Data is suitable for hybrid modelling")
  }
  p2=pp$coef #coefficients of the  ARIMA fitted model
  pvalues<-(1-pnorm(abs(pp$coef)/sqrt(diag(pp$var.coef))))*2 # p values of the coefficients
  ff<-forecast(pp, h)
  ff1=ff$mean
  zz<-nnetar(kk)
  zz1<-zz$model # ANN model summary
  zz2<-zz$p
  fitv=fitted(zz)
  ff2<-forecast(zz, h)
  ff3=ff2$mean
  pp1=fitv[(zz2+1):length(fitv)]
  pp2=kk1[(zz2+1):length(kk1)]
  pp3=abs(pp1+pp2)
  pp3
  pp4=data[(zz2+1):length(data)]
  pp5=fitv[1:zz2]
  Mape=mean(abs((pp4-pp3)/pp4))*100 # MAPE of the hybrid model
  Mse=mean((pp4-pp3)^2)#MSE of the hybrid model.
  pp6=c(pp5, pp3)# final fitted values from hybrid model
  ff4=ff1+ff3#final forecasted values employing hybrid model
  return(list(Test_Result, "ARIMA coefficients"=p2, pvalues=pvalues,"ANN Summary"=zz1,"MAPE"=Mape,"MSE"=Mse, "fitted"=pp6, "forecasted.values"=ff4))}
