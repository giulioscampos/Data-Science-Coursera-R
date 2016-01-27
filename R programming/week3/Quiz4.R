# Question 1 "" "15"
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
fileName <- paste("./data/idaho_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
DF <- read.csv(fileName)
strsplit(names(DF), "wgtp") [[123]]



# Question 2 ???
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileName <- paste("./data/gdp_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
DF <- read.csv(fileName, skip = 4)
library("dplyr")
filtered <- DF %>% mutate(GDP = str_trim(str_replace_all(DF$X.4, ",", ""))) %>%
        filter(GDP != ".." & GDP != "" & !is.na(GDP))
mean(as.numeric(filtered$GDP))


# Question 2 ???
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileName <- paste("./data/gdp_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
GDP <- read.csv(fileName, skip = 4)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
fileName <- paste("./data/edu_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
ED <- read.csv(fileName)
GDPC <- GDP %>% filter(X != "" & !is.na(X)) %>%
        mutate(CountryCode = X)
EDC <- ED %>% filter(CountryCode != "" & !is.na(CountryCode))

merged <- merge(GDPC, EDC, by.x = "CountryCode", by.y = "CountryCode")
mf<-merged[grep("^Fiscal year end: June", levels(merged$Special.Notes)[merged$Special.Notes]),] %>% select(CountryCode, Special.Notes)
dim(mf)


library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
DF<-data.frame(date=index(amzn), coredata(amzn))
library(lubridate)
DF %>% mutate(day=weekdays(as.Date(date, "%y-%m-%d")),
                   year=year(as.Date(date, "%y-%m-%d"))) %>%
        filter(year==2012 & day=="Monday") %>%
        select(date, year, day)
