data=read.csv("BodyFat.csv")

head(data) #Look at the first few data points 
tail(data) #Look at the last few data points

summary(data)


#outlier
data[data$BODYFAT==0,]#other data is correct, use density to calculate body fat, bodyfat=495/density-450
data[data$BODYFAT==0,]$BODYFAT=495/data[data$BODYFAT==0,]$DENSITY - 450

#density and bodyfat do not exactly match, use density as response
plot(data$BODYFAT~data$IDNO,col='black',type='l', main='body fat and density')
points((495/data$DENSITY-450)~data$IDNO, col='red', type='l')

#height, weight and BMI(kg/m), in this data should shift 2.5
plot(data$ADIPOSITY~data$IDNO, col='black', type='l', main='BMI')
points(((data$WEIGHT/2)/((data$HEIGHT/39.4)^2)-2.5)~data$IDNO, col='red', type='l')

data[data$HEIGHT==29.5,]#use adiposity to calculate height BMI=weight/height^2
data[data$HEIGHT==29.5,]$HEIGHT=39.4*sqrt(data[data$HEIGHT==29.5,]$WEIGHT/2/(data[data$HEIGHT==29.5,]$ADIPOSITY+2.5))
#the largest weight 363.15 is accurate from the other data

#fit height
plot(data$DENSITY~data$HEIGHT)
fit1=lm(data$DENSITY~exp(data$HEIGHT))
summary(fit1)

#fit weight
plot(data$DENSITY~data$WEIGHT)
fit2=lm(data$DENSITY~data$WEIGHT)
summary(fit2)

#fit height and weight R^2=0.49
fit3=lm(data$DENSITY~data$WEIGHT+data$HEIGHT)
summary(fit3)
plot(fit3$residuals~fit3$fitted.values)

#fit height, weight and age R^2=0.52
fit4=lm(data$DENSITY~data$WEIGHT+data$HEIGHT+data$AGE)
summary(fit4)

#stepwise regression
library(caret)
ctrl<-trainControl(method = "cv",number = 10)#设置10倍交叉验证

step_model<-train(DENSITY~HEIGHT+WEIGHT+AGE+NECK+CHEST+ABDOMEN+HIP+THIGH+KNEE+ANKLE+BICEPS+FOREARM+WRIST,
                  method = "leapSeq",
                  tuneGrid = data.frame(nvmax = 1:5),#变量个数
                  trControl = ctrl,
                  data=data
)
step_model$results

step_model$finalModel
coef(step_model$finalModel,4)
# best model weight, abdomen, forearm, wrist R^2 0.708

coef(step_model$finalModel,2)# most imp:abdomen(腰围)

#for simplicity, use abdomen and weight
fit5=lm(data$DENSITY~data$WEIGHT+data$ABDOMEN)
par(mfrow=c(2,2))
summary(fit5)
plot(fit5)

#full model
fit6=lm(data$DENSITY~data$WEIGHT*data$ABDOMEN*data$AGE*data$HEIGHT)
summary(fit6)
plot(fit6)

#interaction
fit7=lm(data$DENSITY~data$WEIGHT+data$ABDOMEN+data$HEIGHT
        +data$WEIGHT:data$ABDOMEN+data$WEIGHT:data$HEIGHT)
summary(fit7)
plot(fit7)
