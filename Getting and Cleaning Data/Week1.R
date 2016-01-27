#Reading CSV
fileUrl <- "http://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
fileName <- paste("./data/cameras_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
list.files("./data")
cameraData <- read.csv(fileName, quote = "")


#Reading XLSX
library("xlsx")
fileUrl <- "http://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
fileName <- paste("./data/cameras_", format(Sys.time(), "%Y%m%d"), ".xlsx", sep = "")
data <- download.file(fileUrl, destfile = fileName, mode="wb")
list.files("./data")
cameraData <- read.xlsx(fileName, sheetIndex = 1, header = TRUE)


#Reading XML
library("XML")
fileUrl <- "http://www.w3schools.com/xml/simple.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
rootNode[[1]]
rootNode[[1]][[1]]
xmlSApply(rootNode, xmlValue)
xpathSApply(rootNode, "//name", xmlValue)
xpathSApply(rootNode, "//price", xmlValue)


#Reading HTML
library("XML")
fileUrl <- "http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc <- htmlTreeParse(fileUrl, useInternal = TRUE)
active <- xpathSApply(doc, "//li[@class='active']", xmlValue)

#Reading JSON
library("jsonlite")
jsonData <- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)
jsonData$owner$login
myJson <- toJSON(iris, pretty=TRUE)
cat(myJson)
iris2 <- fromJSON(myJson)
head(iris2)


# Data.table package
library("data.table")
DF = data.frame(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DF, 3)
DT = data.table(x=rnorm(9), y=rep(c("a", "b", "c"), each=3), z=rnorm(9))
head(DT, 3)
tables()
DT[2,]
DT[c(2,3)]
DT[,c(2,3)]
#perform calculations
DT[,list(mean(x), sum(z))]
DT[,table(y)]
#add col
DT[,w:=z^2]
#use copy!
DT2 <- DT
DT[, y:=2]
head(DT, n=3)
#temp
DT[,m:={tmp<-(x+z); log2(tmp+5)}]
DT[, a:=x>0]
#groups
DT[,b:=mean(x+w), by=a]
head(DT2, n=3)
set.seed(123)
DT<-data.table(x=sample(letters[1:3], 1E5, TRUE))
DT[, .N, by=x]
#keys
DT <- data.table(x=rep(c("a", "b", "c"), each=100), y=rnorm(300))
setkey(DT, x)
DT['a']
#joins
DT1<-data.table(x=c('a', 'a', 'b', 'dt1'), y=1:4)
DT2<-data.table(x=c('a', 'b', 'dt2'), y=5:7)
setkey(DT1, x)
setkey(DT2, x)
merge(DT1, DT2)
#fast reading
big_df <- data.frame(x=rnorm(1E6), y=rnorm(1E6))
file <- tempfile()
write.table(big_df, file=file, row.names = FALSE, col.names = TRUE, sep="\t", quote=FALSE)
system.time(fread(file))
system.time(read.table(file, header=TRUE, sep="\t"))

#http://stackoverflow.com/questions/13618488/what-you-can-do-with-data-frame-that-you-cant-in-data-table
