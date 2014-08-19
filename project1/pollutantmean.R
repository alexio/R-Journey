## Return the mean of the pollutant across all monitors list
#in the 'id' vector (ignoring NA values)
## pollutant supposed to be either sulfate or nitrate
## id corresponds to vector for files names. i.e. 1 -> '001.csv'
pollutantmean <- function(directory, pollutant, id = 1:332) {
    sum <- 0
    numRows <- 0
    for(idnum in id){
        file <- getFile(directory, idnum)

        #get pollutant vector without NAs
        polCol <- na.omit(file[,pollutant])

        numRows <- numRows + length(polCol)

        #don't have to omit NA since it was done before
        sum <- sum + sum(polCol)
    }
    mean <- sum/numRows
    mean
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
