#Task 1
#Import data

library(readxl)
df=read_excel('Data_Assignment.xlsx')

a=df[,7]
y=a$y5
y=y[-c(1)]
y=as.numeric(y)


#Create Time Series
j=ts(y, frequency=12, start = c(1990,4))


# Summary Statistics and plots
par(mfrow=c(3,2))
plot(y, type="l", col='red', main="ED stock monthly returns")
hist(y,col='red', main="Histogram of ED returns")
acf(y, 48,col='red', main="ACF of returns")
pacf(y, 48,col='red', main="PACF of returns")
acf(y^2,48,col='red', main="ACF of squared returns")
pacf(y^2, 48,col='red', main="PACF of squared returns")


#ACF lag 1 out of bounds
ma1fit = arima(x = y, order = c(0, 0, 1))
ma1fit
#lag 1 statistically important




#Example MA
ma3fit=arima(y,order=c(0,0,3))
ma3fit
#Lafs 2,3 not statistically important




#Example AR
ar1fit = arima(y, order = c(1, 0, 0))
ar1fit
#Lag 1 not statistically important



#Corrected Model
par(mfrow=c(3,2)) 
model_residuals=ma1fit$residuals
residuals=ts(model_residuals, frequency=12, start = c(1990,4))

acf(ts(residuals,freq=1),col='red', 48, main="ACF of residuals")
pacf(ts(residuals,freq=1),col='red', 48, main="PACF of residuals")
acf(ts(residuals^2,freq=1),col='red', 48, main="ACF of squared residuals")
pacf(ts(residuals^2,freq=1),col='red', 48, main="PACF of squared residuals")
#qqnorm(residuals,col='red',main="Normal QQplot of residuals")
#qqline(residuals, col='red')
#PACF of residuals good enough for real data