# Subsetting
set.seed(13435)
X <- data.frame("var1"=sample(1:5), "var2"=sample(6:10), "var3"=sample(11:15))
X <- X[sample(1:5),]; X$var2[c(1,3)] = NA
X
X[,1]
X[,"var1"]
X[1:2,"var2"]

# Logical statements
X[(X$var1 <= 3 & X$var3>11),]
X[(X$var1 <= 3 | X$var3>15),]

# NA
X[which(X$var2>8)]

# Sorting
sort(X$var1)
sort(X$var1, decreasing = T)
sort(X$var2, na.last = T)

# Ordering
X[order(X$var1),]
X[order(X$var1, X$var3),]

# Arranging with plyr
library(plyr)
arrange(X, var1)
arrange(X, desc(var1))

# Adding rows
X$var4 <- rnorm(5)
Y <- cbind(X, rnorm(5))


# Summarizing data
fileUrl <-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
fileName <- paste("./data/restaurants_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
list.files("./data")
restData <- read.csv(fileName)
head(restData, n=3)
tail(restData, n=3)
summary(restData)
str(restData)
quantile(restData$councilDistrict, na.rm = T)
quantile(restData$councilDistrict, probs = c(0.5, 0.6, 1))
table(restData$zipCode, useNA="ifany")
table(restData$councilDistrict, restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)

colSums(is.na(restData))
colSums(is.na(restData)==0)

table(restData$zipCode %in% c("21212", "21213"))

restData[restData$zipCode %in% c("21212", "21213"),]

data("UCBAdmissions")
DF = as.data.frame(UCBAdmissions)
summary(DF)
xt <- xtabs(Freq ~ Gender + Admit, data=DF)
xt
xt <- xtabs(Freq ~ Gender + Admit + Dept, data=DF)
ftable(xt)

object.size(DF)
print(object.size(DF), units="Mb")


fileUrl <-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
fileName <- paste("./data/restaurants_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
list.files("./data")
restData <- read.csv(fileName)

s1 <- seq(1, 10, by=2); s1
s2 <- seq(1, 10, length = 3); s2
x <- c(1, 3, 8, 25, 100); seq(along = x)

restData$nearMe = restData$neighborhood %in% c("Roland Park","Homeland")
table(restData$nearMe)

restData$zipWrong = ifelse(restData$zipCode < 0, T, F)
table(restData$zipWrong)

restData$zipGroups = cut(restData$zipCode, breaks=quantile(restData$zipCode))
table(restData$zipGroups)

library("Hmisc")
restData$zipGroups = cut2(restData$zipCode, g=4)
table(restData$zipGroups)


restData$zcf <- factor(restData$zipCode)
restData$zcf[1:10]


yesno <- sample(c("yes", "no"), size = 10, replace = T)
yesnofac = factor(yesno, levels = c("yes", "no"))
relevel(yesnofac, ref = "yes")
as.numeric(yesnofac)

library(reshape2)
head(mtcars)
mtcars$carname <- rownames(mtcars)
carMelt <- melt(mtcars, id = c("carname", "gear", "cyl"),measure.vars=c("mpg", "hp"))
cylData <- dcast(carMelt, cyl ~ variable)
cylData <- dcast(carMelt, cyl ~ variable, mean)




