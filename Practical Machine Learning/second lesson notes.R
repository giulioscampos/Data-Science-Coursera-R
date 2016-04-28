install.packages('caret')
install.packages('e1071')
library('caret')
library('kernlab')
library('e1071')

data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
modelFit <- train(type ~., data=training, method="glm")
modelFit
modelFit$finalModel

predictions <- predict(modelFit, newdata=testing)
predictions

confusionMatrix(predictions, testing$type)




set.seed(32323)
folds <- createFolds(y=spam$type, k = 10, list = TRUE, returnTrain = TRUE)
sapply(folds, length)
folds[[1]][1:10]


folds <- createResample(y=spam$type, times = 10, list = TRUE)
sapply(folds, length)
folds[[1]][1:10]

tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10)
names(folds)









install.packages('ISLR')
library('ISLR')
library('ggplot2')
data(Wage)
summary(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

featurePlot(x=training[, c("age", "education", "jobclass")], y = training$wage, plot = "pairs")
qplot(age, wage, data=training)
qplot(age, wage, colour=jobclass, data=training)

qq <- qplot(age, wage, colour=education, data=training)
qq + geom_smooth(method = "lm", formula = y~x)

library('Hmisc')
cutWage <- cut2(training$wage, g=3)
table(cutWage)


p1 <- qplot(cutWage, age, data = training, fill = cutWage, geom = c("boxplot"))
p1

#??
p2 <- qplot(cutWage, data = training, fill = cutWage, geom = c("boxplot", "jitter"))
grid.arrange(p1, p2, ncol=2)



t1 <- table(cutWage, training$jobclass)
t1
prop.table(t1, 1)


qplot(wage, colour = education, data = training, geom = "density")




data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main = "", xlab = "ave. capital run length")

mean(training$capitalAve)
sd(training$capitalAve)


trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)

testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd (trainCapAve)
mean(testCapAveS)


preObj <- preProcess(training[,-58], method=c("center", "scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)


testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)


set.seed(32343)
modelFit <- train(type ~., data=training, preProcess=c("center", "scale"), method="glm")


preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2));hist(trainCapAveS);qqnorm(trainCapAveS)





set.seed(13343)

#Make some values NA
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <-NA

#Impute and standardize
install.packages("RANN")
library("RANN")
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve

#Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)


quantile(capAve-capAveTruth)
quantile((capAve-capAveTruth)[selectNA])
quantile((capAve-capAveTruth)[!selectNA])








install.packages('kernlab');
library('kernlab');
data(spam)
spam$capitalAveSq <- spam$capitalAve^2


library('ISLR')
library('caret')
data(Wage)
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

#cleaning
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv


library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)


predict(bsBasis, age=testing$age)




#preProcessing

data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))

diag(M) <- 0

which(M>0.8, arr.ind=T)

#Principal component (x adds, y sub)
smallSpam <- spam[,c(34, 32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1], prComp$x[,2])


typeColor <- ((spam$type=="spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")


#same with caret
preProc <- preProcess(log10(spam[, -58]+1), method="pca", pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

#fitting a model with PC
preProc <- preProcess(log10(training[, -58]+1), method="pca", pcaComp=2)
trainPC <- predict(preProc, log10(training[,-58]+1))
modelFit <- train(training$type ~ ., method="glm", data=trainPC)

testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit, testPC))









#Regression
library(caret)
data("faithful")
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)


#fit a linear model
plot(x = trainFaith$waiting, y = trainFaith$eruptions, col="blue", xlab="Waiting", ylab="Erruptions", pch=19)
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)
lines(trainFaith$waiting, lm1$fitted, lwd=3)

coef(lm1)[1]+coef(lm1)[2]*80
newData <- data.frame(waiting=80)
predict(lm1, newData)

par(mfrow=c(1,2))
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(trainFaith$waiting, predict(lm1), lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")
lines(testFaith$waiting, predict(lm1, newdata=testFaith), lwd=3)



# calculate RMSE on the training set
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
# calculate RMSE on the test set (should be slightly bigger)
sqrt(sum((predict(lm1, newdata=testFaith) - testFaith$eruptions)^2))


# prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting, testFaith$eruptions, pch=19, color="blue")
matlines(testFaith$waiting[ord], pred1[ord,], type="l", , col=c(1, 2, 2), lty=c(1, 1, 1), lwd=3)


# same with caret
modFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modFit$finalModel)









# Predicting with regression multiple covariates
library(ISLR);library(ggplot2);library(caret)
data(Wage); Wage<-subset(Wage, select=-c(logwage))
summary(Wage)



inTrain <- createDataPartition(Wage$wage, p=0.7, list = FALSE)
training <- Wage[inTrain,];testing <- Wage[-inTrain,]
dim(training);dim(testing)


featurePlot(x=training[, c("age", "education", "jobclass")], y=training$wage, plot="pairs")
qplot(age, wage, data=training)
qplot(age, wage, data=training, colour=jobclass)
qplot(age, wage, data=training, colour=education)


modFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
finMod <- modFit$finalModel
print(modFit)

plot(finMod, 1, pch=19, cex=0.5, col="#0000010")

# color by variables not in the model
qplot(finMod$fitted, finMod$residuals, colour=race, data=training)

# plot by index
plot(finMod$residuals, pch=19)

# predicted vs truth in test set
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)

# all covariates
modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)


predName <- names(training)
(ILpredictor <- predName[substr(predName, 1, 2) == "IL"])
ProcPCA <- preProcess(training[, ILpredictor], method = "pca", thresh = .9)
ProcPCA$numComp


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]training = adData[ inTrain,]
testing = adData[-inTrain,]
