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
