## 'directory' is a character vector of length 1 indicating
## the location of the CSV files
corr <- function(directory, threshold = 0) {

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        result = numeric()
        for(idnum in 1:332) {
            file <- getFile(directory, idnum)

            ccases <- complete.cases(file)
            if(nrow(file[ccases,]) > threshold) {
                corl <- cor(file[ccases,"nitrate"], file[ccases, "sulfate"])
                result <- c(result, corl)
            }
        }
        result
}

# Reads file constructed from "directory/idnum.csv"
# prefix idnum with 0s if necessary
getFile <- function(directory, idnum) {
    prefix <- ""
    if(idnum < 10)
        prefix <- "00"
    else if(idnum < 100)
        prefix <- "0"

    s <- c(directory, "/", prefix,idnum,".csv")
    filename <- paste(s,collapse = "")
    file <- read.csv(filename)
    file
}
