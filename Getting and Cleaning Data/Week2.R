#MySQL

install.packages("RMySQL")
library("RMySQL")

ucscDb<-dbConnect(MySQL(), user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery(ucscDb, "show databases;")
dbDisconnect(ucscDb)

hg19<-dbConnect(MySQL(), user="genome",db="hg19", host="genome-mysql.cse.ucsc.edu")
allTables<-dbListTables(hg19)
length(allTables)
allTables[1:5]
dbListFields(hg19, "affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2")
affyData<-dbReadTable(hg19, "affyU133Plus2")
head(affyData)
query<-dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis<-fetch(query)
quantile(affyMis$misMatches)
affyMisSmall<-fetch(query, n=10)
dbClearResult(query)
dim(affyMisSmall)

#HDF5
source("http://bioconductor.org/biocLite.R")
biocLite("rdhf5")
library("rdhf5")
created = h5createFile("example.h5")
created
created = h5createGroup("example.h5", "foo")
created = h5createGroup("example.h5", "baa")
created = h5createGroup("example.h5", "foo")

#Web Pages
url <- "https://gr.linkedin.com/in/ioannisoikonomidis"
con = url(url)
htmlCode = readLines(con)
close(con)
htmlCode
library(XML)
html<-htmlTreeParse(url, useInternalNodes=T)
library(httr)
html2 = GET(url)
content2=content(html2, as="text")
parsedHtml = htmlParse(content2, asText=TRUE)
xpathSApply(parsedHtml, "//title", xmlValue)



#APIs



