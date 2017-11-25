complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        files <- list.files(directory)
        wona_rows = c()
        for(index in id) {
                path <- file.path(directory, files[index])
                file <- read.csv(file = path, header = TRUE, sep = ",")
                wona_rows <- c(wona_rows, nrow(na.omit(file)))
        }

        data.frame(id = id, nobs = wona_rows)
}