#----------------------------------------------
#Dana, Dan and Ian R Doc

#----------------------------------------------
#Notes to Professsor
#Please import newdata1.csv 

#----------------------------------------------

#This project is a comprehensive analysis of the relationship
#between three point attempts, three point percentage and fouls

#We want to see if there are any patterns between bettwen the increase in
#attempts and the increase in percentage
#also we want to investigte the relationship between attempts and 
#fouls
#----------------------------------------------

#data is collected from the 2009-2017 NBA Season
#----------------------------------------------

#Data source: NBA Reference 
#----------------------------------------------

#making a time series, freqeuncy is 30 as there are 30 teams each year

Basketball_Time_SeriesAttempts = as.ts(newdata1$TPA,start = 1455,frequency = 30)
#View(RedwineTimeSeries)
#View(Basketball_Time_SeriesAttempts)

Basketball_Time_SeriesPercentage = as.ts(newdata1$TP.,start = 0.360,frequency = 30)
#View(RedwineTimeSeries)


#30 teams per year

#----------------------------------------------------


Basketball_Time_SeriesFouls =  as.ts(newdata1$PF_1,start = 1631.80,frequency = 30)

Basketball_Time_SeriesAttempts = ts(Basketball_Time_SeriesAttempts, start = 2009, frequency = 30)
plot.ts(Basketball_Time_SeriesAttempts, main="Three-Point Attempts",xlab="Time",ylab="Three-Point Attempts",lwd=1)


Basketball_Time_SeriesPercentage = ts(Basketball_Time_SeriesPercentage, start = 2009, frequency = 30)
plot.ts(Basketball_Time_SeriesPercentage, main="Three Point Percentage",xlab="Time",ylab="Three-Point Percentage",lwd=1)


Basketball_Time_SeriesFouls = ts(Basketball_Time_SeriesFouls, start = 2009, frequency = 30)
plot.ts(Basketball_Time_SeriesFouls, main="Fouls",xlab="Time",ylab="Fouls",lwd=1)



y2 = newdata$TP.
t4 = time(newdata$TP.)
fit5 = lm(y2~t4)
summary(fit5)


#attempted seasonaility fiting but warrented not results with attempts

#detrending the data finding the best model for ccf 


#----------------------------------------------------
Y =newdata1$TPA
t = time(newdata1$TPA)
fit = lm(Y~t)
summary(fit)
t2 = t^2/factorial(2)
fit2 = lm(Y~t+t2)
summary(fit2)

fit3 = lm(Y~t2)
summary(fit3)

t3 = t^3/factorial(3)

fit4 = lm(Y~t+t2+t3)
summary(fit4)
dev.off()

plot.ts(Basketball_Time_SeriesAttempts)
plot(fit$residuals)
plot(fit2$residuals)
plot(fit3$residuals)
summary(fit)
plot.ts(fit$residuals)
summary(fit)
summary(fit2)
summary(fit3)

t3 = t^3/factorial(3)
fit4 = lm(Y~t+t2+t3)

#best fit, all predictors are statistically significant
#highest adjusted r-sqared

#----------------------------------------------------

summary(fit4)

y2 = newdata$TP.
t4 = time(newdata$TP.)

fit5 = lm(y2~t4)

plot(fit4$residuals, main="Cubic Trend Model (3PT Attempts) Residual Plot", ylab="Residuals")
plot(fit5$residuals, main="3Pt Percentage Residual Plot", ylab = "Residuals")
summary(fit4) #for attempts
summary(fit5)

fit1 = fit$residuals
new_Model = lm(fit4$residuals~fit5$residuals)

ccf(fit4$residuals, fit5$residuals, main="CCF of 3-Point Attempts and 3-Point Percentage")

#detrended attempts against percentage
#----------------------------------------------------

TSReg = lm(fit4$residuals~ fit5$residuals)
summary(TSReg)
plot(TSReg$residuals, main="TS Regression Residual Plot", ylab="Residuals")
par(mfrow=c(2,1))
acf(TSReg$residuals, lag.max=25, main="Residual ACF of TS Regression Model (attempts~percentage)")
pacf(TSReg$residuals, lag.max=25, main="Residual PACF of TS Regression Model(attempts~percentage)")

library(astsa)
library(tseries)
library(forecast)
armamodel = arma(TSReg$residuals, order=c(1,1))
plot(armamodel$residuals, main="ARMA(1,1) Residual Plot", ylab="Residuals")
summary(armamodel)

#----------------------------------------------------

dev.off()

new_Auto_Arima = auto.arima(Basketball_Time_SeriesAttempts)

fitAtt = auto.arima(Basketball_Time_SeriesAttempts)
summary(fitAtt)

resid34 = fitAtt$residuals
#forcasting
fore34 = forecast(fitAtt, h=100)
plot(fore34)

#--------------------------------------------------
#attempts vs fouls

#fouls has no trend but seasonaility
#--------------------------------------------------
Basketball_Time_SeriesFouls =  as.ts(newdata1$PF_1,start = 1631.80,frequency = 30)

Basketball_Time_SeriesFouls = ts(Basketball_Time_SeriesFouls, start = 2009, frequency = 30)
plot.ts(Basketball_Time_SeriesFouls, main="Fouls",xlab="Time",ylab="Fouls",lwd=1)


#--------------------------------------------------
#attempts to deal with seasonality of fouls

#--------------------------------------------------

dev.off()

#----------------------------------------------------

t<-1:length(Basketball_Time_SeriesFouls)
per = 1 #for the year
c1<-cos(2*pi*t/per)
s1<-sin(2*pi*t/per)

model1<-lm(Basketball_Time_SeriesFouls~c1+s1)
summary(model1)

model2 = lm(Basketball_Time_SeriesFouls~s1)
summary(model2)

#seasonaility model does not improve fit tried more seaonility fits nothing worked 
#will continue on just mention seasonility


#--------------------------------------------------
#trend modeling

Y =newdata1$PF_1
t3 = time(newdata1$PF_1)
fit3 = lm(Y~t)
summary(fit3)
#poor r sqaured signincant predictor attempt other models 

t23 = t^2/factorial(2)
fit23 = lm(Y~t3+t23)
summary(fit23)

fit33 = lm(Y~t2)
summary(fit33)

t33 = t^3/factorial(3)

fit44 = lm(Y~t3+t23+t33)
summary(fit44)
dev.off()
#best fit, still poor r sqaured adjusted, makes no sence to capture trend

TSREG1 = lm(fit4$residuals~fit44$residuals)
dev.off()
plot(TSREG1$residuals, main="TS Regression Residual Plot")


#--------------------------------------------------
#look at acf pacf and ccf
#--------------------------------------------------
#no no variance issues no heteroscadistic issues

acf(TSREG1$residuals, lag.max=25, main="Residual ACF of TS Regression Model with Fouls")
pacf(TSREG1$residuals, lag.max=25, main="Residual PACF of TS Regression Model with Fouls")

dev.off()
ccf(fit4$residuals, fit44$residuals,lag.max = 25, main= "CCF of attempts and fouls")

# highest lag at lag 13 atttempt to look at lag relationship
#--------------------------------------------------
#OG model

fit = lm(Basketball_Time_SeriesAttempts~Basketball_Time_SeriesFouls,data=newFit)
summary(fit)


LEADLag <- ts.intersect(Basketball_Time_SeriesAttempts, Basketball_Time_SeriesFouls13=lag(Basketball_Time_SeriesFouls,+13),dframe=TRUE)
fit <- lm(Basketball_Time_SeriesAttempts~Basketball_Time_SeriesFouls13,data=LEADLag)
summary(fit)
#very poor fit

#try 19 lag

LEADLag <- ts.intersect(Basketball_Time_SeriesAttempts, Basketball_Time_SeriesFouls19=lag(Basketball_Time_SeriesFouls,19),dframe=TRUE)
fit <- lm(Basketball_Time_SeriesAttempts~Basketball_Time_SeriesFouls19,data=LEADLag)
summary(fit)

#----------------------------------------------------


TSReg2 = lm(fit4$residuals~ fit44$residuals)
summary(TSReg2)
plot(TSReg2$residuals, main="TS Regression Residual Plot", ylab="Residuals")
par(mfrow=c(2,1))
#appears to be an MA(1)
acf(TSReg2$residuals, lag.max=25, main="Residual ACF of TS Regression Model")
pacf(TSReg2$residuals, lag.max=25, main="Residual PACF of TS Regression Model")

library(astsa)
library(tseries)
library(forecast)
armamodel = arma(TSReg2$residuals, order=c(0,1))
plot(armamodel$residuals, main="ARMA(1,1) Residual Plot", ylab="Residuals")
summary(armamodel)

#----------------------------------------------------


dev.off()

#new_Auto_Arima = auto.arima(Basketball_Time_SeriesFouls)

fitAtt = auto.arima(Basketball_Time_SeriesFouls)
summary(fitAtt)

resid34 = fitAtt$residuals
#forcasting
fore34 = forecast(fitAtt, h=50)
dev.off()
plot(fore34)



#slightly better fit than orginal model but in general bad fit

#--------------------------------------------------





