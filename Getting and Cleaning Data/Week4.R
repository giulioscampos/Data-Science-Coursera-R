fileUrl <-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
fileName <- paste("./data/cameras_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
list.files("./data")
cameras <- read.csv(fileName)
tolower(names(cameras))
splitNames = strsplit(names(cameras), "\\.")
splitNames

myList <- list(letters = c("A", "b", "c"), numbers = 1:3, matrix(1:25, ncol = 5))

splitNames[[6]][1]
firstElement <- function(x){x[1]}
sapply(splitNames,firstElement)

sub("\\.", "_", names(cameras),)
gsub("\\.", "_", names(cameras),)

grep("Caton Ave", cameras$street)
table(grep("Caton Ave", cameras$street))

grep("Caton Ave", cameras$street, value = T)


# ^ begining of the line
# $ begining of the line
# [Bb][Uu][Ss][Hh] Capital-Lower case letters
# ^[Ii] am
# ^[0-9][a-zA-Z]
# [^?.]$ not ending with ? or .
# 9.11 any character .
# flood|fire
# ^[Gg]ood|[Bb]ad
# ^([Gg]ood|[Bb]ad)
# [Gg]eorge( [Ww]\.)? [Bb]ush
# (.*) any or none
# [0-9]+ (.*)[0-9]+ at least one
# [Bb]ush( +[^ ]+ +){1, 5} debate  5 words between bush and debate
#  +([a-zA-Z]+) +\1 +


d1 = date()
d2 = Sys.Date()
format(d2, "%a %b %d")
dates <- c("02/27/92", "02/27/92", "01/14/92", "02/28/92", "02/01/92")
as.Date(dates, "%m/%d/%y")
weekdays(d2)
months(d2)
julian(d2)

library(lubridate)



