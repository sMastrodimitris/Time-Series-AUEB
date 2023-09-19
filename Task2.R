#Task 2
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

a=df[,14]
x=a$x3
x3=x[-c(1)]
x3=as.numeric(x3)

a=df[,15]
x=a$x4
x4=x[-c(1)]
x4=as.numeric(x4)

a=df[,16]
x=a$x5
x5=x[-c(1)]
x5=as.numeric(x5)

a=df[,17]
x=a$x6
x6=x[-c(1)]
x6=as.numeric(x6)

a=df[,18]
x=a$x7
x7=x[-c(1)]
x7=as.numeric(x7)

a=df[,19]
x=a$x8
x8=x[-c(1)]
x8=as.numeric(x8)

a=df[,20]
x=a$x9
x9=x[-c(1)]
x9=as.numeric(x9)

a=df[,21]
x=a$x10
x10=x[-c(1)]
x10=as.numeric(x10)

a=df[,22]
x=a$x11
x11=x[-c(1)]
x11=as.numeric(x11)

a=df[,23]
x=a$x12
x12=x[-c(1)]
x12=as.numeric(x12)

a=df[,24]
x=a$x13
x13=x[-c(1)]
x13=as.numeric(x13)

a=df[,25]
x=a$x14
x14=x[-c(1)]
x14=as.numeric(x14)

a=df[,26]
x=a$x15
x15=x[-c(1)]
x15=as.numeric(x15)




#pairs(cbind(y,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15))

#Put Everything in
y1res = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15)

#Analysis
coef(y1res)
alpha = coef(y1res)[1]
alpha
alphase = sqrt(diag(vcov(y1res)))[1]
alphase
tstatalpha = alpha/alphase
tstatalpha 
summary(y1res)



par(mfrow=c(2,1))
acf(residuals(y1res))
pacf(residuals(y1res))



#Keep important
y2res = lm(y ~ x1 + x2 + x4 + x5 + x6 + x8)



#Analysis
coef(y2res)
alpha = coef(y2res)[1]
alpha
alphase = sqrt(diag(vcov(y2res)))[1]
alphase
tstatalpha = alpha/alphase
tstatalpha 
summary(y2res)

par(mfrow=c(2,1))
acf(residuals(y2res))
pacf(residuals(y2res))

#A bit of a problem with the residuals but similar quality with alot less Xs 




#Keep 4 of the most important 
y3res = lm(y ~ x1 + x2 + x4 + x5)



#Analysis
coef(y3res)
alpha = coef(y3res)[1]
alpha
alphase = sqrt(diag(vcov(y3res)))[1]
alphase
tstatalpha = alpha/alphase
tstatalpha 
summary(y3res)

par(mfrow=c(2,1))
acf(residuals(y3res))
pacf(residuals(y3res))

#A matter of preferance between this and the previous model
