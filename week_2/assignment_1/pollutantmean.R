pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!

        files <- list.files(directory)
        pollutant_values = c()
        for(index in id) {
                path <- file.path(directory, files[index])
                file <- read.csv(file = path, header = TRUE, sep = ",")
                clean_pollutants <- file[!is.na(file[, pollutant]), pollutant]
                pollutant_values <- c(pollutant_values, clean_pollutants)
        }

        mean(pollutant_values)
}