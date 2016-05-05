library(ISLR); data(Wage); library(ggplot2); library(caret);

# create a building data set and a validation set
Wage <- subset(Wage, select=-c(logwage))
inBuild <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
validation <- Wage[-inBuild,];buildData <- Wage[inBuild,]
inTrain <- createDataPartition(buildData$wage, p=0.7, list=FALSE)
training <- buildData[inTrain,];testing <- buildData[-inTrain,]

dim(training);dim(validation);dim(testing);

# build two different models in the training set
mod1 <- train(wage ~ ., method="glm", data=training)
mod2 <- train(wage ~ ., method="rf", data=training, trControl= trainControl(method="cv"), number=3)

# predict on the test set
pred1 <- predict(mod1, testing); pred2 <- predict(mod2, testing)
qplot(pred1, pred2, colour=wage, data=training)

# fit a model that combines predictors
predDF <- data.frame(pred1, pred2, wage=testing$wage)
combModFit <- train(wage ~ ., method="gam", data=predDF)
combPred <- predict(combModFit, predDF)


# errors
sqrt(sum((pred1 - testing$wage)^2))
sqrt(sum((pred2 - testing$wage)^2))
sqrt(sum((combPred - testing$wage)^2))



# predict on validation set
pred1V <- predict(mod1, validation); pred2V <- predict(mod2, validation)
predVDF <- data.frame(pred1=pred1V, pred2=pred2V)
combPredV <- predict(combModFit, predVDF)

sqrt(sum((pred1V - validation$wage)^2))
sqrt(sum((pred2V - validation$wage)^2))
sqrt(sum((combPredV - validation$wage)^2))



library(quantmod)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="google", from = from.dat, to = to.dat)

head(GOOG)

mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen, frequency=12)
plot(ts1, xlab="Years+1", ylab="GOOG")


plot(decompose(ts1), xlab="Years+1")


ts1Train <- window(ts1, start=1, end=5)
ts1Test <- window(ts1, start=5, end=(7-0.01))
ts1Train


# simple moving average
plot(ts1Train)
lines(ma(ts1Train, order=3), co=="red")

# exponential smoothing
ets1 <- ets(ts1Train, model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test, col="red")

accuracy(fcast, ts1Test)








# unsupervised prediction
data(iris); library(ggplot2);
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]; testing <- iris[-inTrain,]
dim(training);dim(testing)

# clusters with k-means
kMeans1 <- kmeans(subset(training, select=-c(Species)), centers=3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width, Petal.Length, colour=clusters, data=training)

# build predictor
modFit <- train(clusters ~., data=subset(training, select=-c(Species)), method="rpart")
table(predict(modFit, training), training$Species)

# apply on test
testClusterPred <- predict(modFit, testing)
table(testClusterPred, testing$Species)










# quiz 4
# 1)
install.packages('AppliedPredictiveModeling')
install.packages('caret')
install.packages('ElemStatLearn')
install.packages('pgmm')
install.packages('rpart')
install.packages('gbm')
install.packages('lubridate')
install.packages('forecast')
install.packages('e1071')
library('AppliedPredictiveModeling')
library('caret')
library('ElemStatLearn')
library('pgmm')
library('rpart')
library('gbm')
library('lubridate')
library('forecast')
library('e1071')
data(vowel.train)
data(vowel.test)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)

modFitRF <- train(y ~ ., method="rf", data=vowel.train, prox=TRUE)
modFitGBM <- train(y ~ ., method="gbm", data=vowel.train, verbose=FALSE)

predRF <- predict(modFitRF, vowel.test)
predGBM <- predict(modFitGBM, vowel.test)

confusionMatrix(predRF, vowel.test$y)$overall[1]
confusionMatrix(predGBM, vowel.test$y)$overall[1]

predDF <- data.frame(predRF, predGBM, y = vowel.test$y)
# Accuracy among the test set samples where the two methods agree
sum(predRF[predDF$predRF == predDF$predGBM] == 
            predDF$y[predDF$predRF == predDF$predGBM]) / 
        sum(predDF$predRF == predDF$predGBM)


# 2)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)

modFitRF <- train(diagnosis ~ ., data = training, method = "rf")
modFitGBM <- train(diagnosis ~ ., data = training, method = "gbm")
modFitLDA <- train(diagnosis ~ ., data = training, method = "lda")
predRF <- predict(modFitRF, testing)
predGBM <- predict(modFitGBM, testing)
predLDA <- predict(modFitLDA, testing)
predDF <- data.frame(predRF, predGBM, predLDA, diagnosis = testing$diagnosis)
combModFit <- train(diagnosis ~ ., method = "rf", data = predDF)
combPred <- predict(combModFit, predDF)


confusionMatrix(predRF, testing$diagnosis)$overall[1]
confusionMatrix(predGBM, testing$diagnosis)$overall[1]
confusionMatrix(predLDA, testing$diagnosis)$overall[1]
confusionMatrix(combPred, testing$diagnosis)$overall[1]









# 3)

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)

modFitLasso <- train(CompressiveStrength ~ ., data = training, method = "lasso")
install.packages('elasticnet')
library(elasticnet)
plot.enet(modFitLasso$finalModel, xvar = "penalty", use.color = TRUE)





# 4)

dat = read.csv("~/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
library(forecast)
modFitTS <- bats(tstrain)
fcast <- forecast(modFitTS, level = 95, h = dim(testing)[1])
sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / 
        dim(testing)[1]





# 5)
set.seed(3523)

library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
library(e1071)
modFitSVM <- svm(CompressiveStrength ~ ., data = training)
predSVM <- predict(modFitSVM, testing)
accuracy(predSVM, testing$CompressiveStrength)