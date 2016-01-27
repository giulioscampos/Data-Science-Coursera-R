# Question 1
fileUrl <-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
fileName <- paste("./data/communities_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
DF <- read.csv(fileName)

# aggricaltureLogical <- (Logical Vector) households greater than 10 acres (ACR == 3)&
#                                               agric prod sold (AGS == 6) > 10.000$
# which(agricultureLogical)

head(DF)
library("dplyr")
agricalureLogical <- DF$ACR == 3 & DF$AGS == 6
which(agricalureLogical)

# Question 2
install.packages("jpeg")
library(jpeg)
DF <- readJPEG("./data/getdata-jeff.jpg", native = T)
quantile(DF, probs = c(0.3,0.8) )

# Question 3 - 4 - 5
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
fileName <- paste("./data/gdp_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
GDP <- read.csv(fileName)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
fileName <- paste("./data/ed_", format(Sys.time(), "%Y%m%d"), ".csv", sep = "")
data <- download.file(fileUrl, destfile = fileName)
ED <- read.csv(fileName)
GDPC <- GDP %>% filter(X != "" & !is.na(X)) %>%
        mutate(CountryCode = X,
               GDP_ranking =
                as.numeric(levels(Gross.domestic.product.2012)[Gross.domestic.product.2012]))
EDC <- ED %>% filter(CountryCode != "" & !is.na(CountryCode))

merged <- merge(GDPC, EDC, by.x = "CountryCode", by.y = "CountryCode")
arranged <- merged %>% filter(GDP_ranking != "" &
        !is.na(GDP_ranking)) %>% arrange(desc(GDP_ranking)) %>% 
        select(CountryCode, GDP_ranking, X, Gross.domestic.product.2012, Income.Group)
summarized_by_income_group <- arranged %>% group_by(Income.Group) %>%
                summarise(mean(GDP_ranking))

quantile(merged$GDP_ranking, probs = 0.2, na.rm = T)
arranged %>% filter(GDP_ranking<=38 & Income.Group == "Lower middle income")