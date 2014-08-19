## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {

    len <- length(id)
    ids <- numeric(len)
    nobs <- numeric(len)

    count <- 1
    for(idnum in id) {
        file <- getFile(directory, idnum)

        ccases <- complete.cases(file)
        nobs[count] <- nrow(file[ccases,])

        ids[count] <- idnum
        count <- count + 1
    }
    data <- data.frame(ids, nobs)
    data
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
