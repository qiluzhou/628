#read data
data=read.csv("BodyFat.csv")

#data preview
head(data) #Look at the first few data points 
tail(data) #Look at the last few data points

summary(data)


#outlier

#remove data that bodyfat=0
data[data$BODYFAT==0,]
data=data[-182,]

#density and bodyfat do not exactly match, use density as response
plot(data$BODYFAT~data$IDNO,col='black',type='l', main='body fat and density')
points((495/data$DENSITY-450)~data$IDNO, col='red', type='l')

#height, weight and BMI(kg/m), in this data should shift 2.5
plot(data$ADIPOSITY~data$IDNO, col='black', type='l', main='BMI')
points(((data$WEIGHT/2)/((data$HEIGHT/39.4)^2)-2.5)~data$IDNO, col='red', type='l')

data[data$HEIGHT==29.5,]#use adiposity to calculate height BMI=weight/height^2
data[data$HEIGHT==29.5,]$HEIGHT=39.4*sqrt(data[data$HEIGHT==29.5,]$WEIGHT/2/(data[data$HEIGHT==29.5,]$ADIPOSITY+2.5))
#the largest weight 363.15 is accurate from the other data

#change weight, height units to kg and cm
data[,5]=data[,5]*0.45359237
data[,6] = data[,6]*2.54

#Model 1&2: only fit height or weight
plot(data$BODYFAT~data$HEIGHT)
fit1=lm(data$BODYFAT~exp(data$HEIGHT))
summary(fit1)

plot(data$BODYFAT~data$WEIGHT)
fit2=lm(data$BODYFAT~data$WEIGHT)
summary(fit2)

#Model3: fit both height and weight R^2=0.49
fit3=lm(data$BODYFAT~data$WEIGHT+data$HEIGHT)
summary(fit3)
plot(fit3$residuals~fit3$fitted.values)

#model4: fit height, weight and age R^2=0.52
fit4=lm(data$BODYFAT~data$WEIGHT+data$HEIGHT+data$AGE)
summary(fit4)

#model5: stepwise regression



step_model<-lm(BODYFAT~HEIGHT+WEIGHT+AGE+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,
                  data=data
)
t_model=step(step_model)
summary(t_model)
mean(t_model$residuals^2)

#for simplicity, use abdomen and weight
fit5=lm(data$BODYFAT~data$WEIGHT+data$ABDOMEN)
par(mfrow=c(2,2))
summary(fit5)
plot(fit5)
mean(fit5$residuals^2)

#model6(consider interaction): full model
fit6=lm(data$BODYFAT~data$AGE*data$ABDOMEN*data$HEIGHT)
summary(fit6)


#lasso regression
library(glmnet)
x = model.matrix(BODYFAT~.-IDNO-DENSITY,data)[,-1]
y = data$BODYFAT
set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "gaussian")
model <- glmnet(x, y, alpha = 1, family = "gaussian",
                lambda = cv.lasso$lambda.min)
coef(model)
fit_lasso = lm(BODYFAT~AGE+HEIGHT+NECK+ABDOMEN+FOREARM+WRIST,data=data)
summary(fit_lasso)
mean(fit_lasso$residuals^2)


#model7: use only significant two-factor interaction
#use this one as final model
fit7=lm(data$BODYFAT~ABDOMEN*WEIGHT, data=data)
summary(fit7)

#MSE
mean(fit7$residuals^2)
plot(fit7)
confint(fit7)
#predict michael phelps's bodyfat
ph=c(1,83.38, 86,7170.68)
sum(fit7$coefficients*ph)

#model diagnostics
par(mfrow=c(1,2))
plot(fit7$residuals~fit7$fitted.values
     , xlab="fitted values", ylab="residuals"
     , main="residuals vs fitted values")
abline(h=0, col='red')
qqnorm(fit7$residuals)
qqline(fit7$residuals, col='red')

#visualization of final model
plot(data$DENSITY,type="l", ylab="density", main='density vs fitted values')
grid()
points(fit7$fitted.values, col='red', type="l")
legend("topright",legend=c("density","predict density"), fill=c("black","red"))

