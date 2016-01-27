# Question 1 https://api.github.com/users/jtleek/repo


# Question 2 sqldf("select pwgtp1 from acs where AGEP < 50")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
fileName <- paste("./data/survey_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
acs <- download.file(fileUrl, destfile = fileName)
list.files("./data")
data <- read.csv(fileName, quote = "")

# Question 3 sqldf("select distinct AGEP from acs")

# Question 4 45 31 7 25
con = url(fileUrl)
htmlCode = readLines(con)
nchar(htmlCode[10])
nchar(htmlCode[20])
nchar(htmlCode[30])
nchar(htmlCode[100])

# Question 5
data <- read.fwf("https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
