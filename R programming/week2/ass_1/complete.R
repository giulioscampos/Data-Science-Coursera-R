complete <- function(directory, id = 1:332) {
        fileList <- list.files(directory, full.names = TRUE)
        Data <- data.frame()
        for (i in id) {
                df <- read.csv(fileList[i])
                nobs <- sum(complete.cases(df))
                tmp <- data.frame(i, nobs)
                Data <- rbind(Data, tmp)
        }
        
        colnames(Data) <- c("id", "nobs")
        Data
}