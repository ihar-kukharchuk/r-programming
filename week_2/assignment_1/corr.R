source("complete.R")

corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!

        observed_cases = complete(directory = directory)
        files <- list.files(directory)
        corr_vector = numeric()
        for(row in 1:nrow(observed_cases)) {
                if(observed_cases[row, "nobs"] <= threshold) {
                        next
                }
                path <- file.path(directory, files[observed_cases[row, "id"]])
                file <- read.csv(file = path, header = TRUE, sep = ",")
                wona_rows <- na.omit(file)
                corr_vector = c(corr_vector, cor(x = wona_rows$"nitrate",
                                                 y = wona_rows$"sulfate"))
        }

        corr_vector
}