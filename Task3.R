#Task 3
library(readxl)
df=read_excel('Data_Assignment.xlsx')

a=df[,7]
y=a$y5
y=y[-c(1)]
y=as.numeric(y)

a=df[,12]
x=a$x1
x1=x[-c(1)]
x1=as.numeric(x1)
options(scipen=999)

a=df[,13]
x=a$x2
x2=x[-c(1)]
x2=as.numeric(x2)

a=df[,15]
x=a$x4
x4=x[-c(1)]
x4=as.numeric(x4)

a=df[,16]
x=a$x5
x5=x[-c(1)]
x5=as.numeric(x5)



#I decided a model with only 4 dependent variables 
y3res = lm(y ~ x1 + x2 + x4 + x5)
summary(y3res)

par(mfrow=c(2,1))
acf(residuals(y3res))
pacf(residuals(y3res))


#From the acf graph we see an important residual autocorrelation for the lags 1 and 2
#From the pacf we see no noticeable partial autocorrelation


#For the autocorrelation of the residuals we will have to implement MA
#Starting with MA(1) 
ma1res=arima(residuals(y3res), order=c(0,0,1), include.mean=FALSE)  
ma1res
ma1res$residuals
ma1res$coef
acf(residuals(ma1res),50)
pacf(residuals(ma1res),50)

#Good but insufficient
#MA(1) & MA(2) 
ma2res=arima(residuals(y3res), order=c(0,0,2), include.mean=FALSE)
ma2res
ma2res$residuals
ma2res$coef
acf(residuals(ma2res),50)
pacf(residuals(ma2res),50)
#Serviceable
#No important autocorrelation or partial autocorrelation


#Heteroscedasticity
par(mfrow=c(3,1))
plot(ma2res$residuals, type="l", col='red', main="MA residuals")
acf(ma2res$residuals^2,12, main="ACF of residuals")
pacf(ma2res$residuals^2, 12, main="PACF of squared residuals")


Box.test(ma2res$residuals,lag=12,type="Ljung")
Box.test(ma2res$residuals^2,lag=12,type="Ljung")
#p-values>0.05 residuals homoscedastic


#Normality Test
shapiro.test(ma2res$residuals)
par(mfrow=c(2,1))
hist(ma2res$residuals, prob=TRUE, 15)
lines(density(ma2res$residuals))
qqnorm(ma2res$residuals,main="Normal QQplot of MA residuals")
qqline(ma2res$residuals)
#Almost normal...