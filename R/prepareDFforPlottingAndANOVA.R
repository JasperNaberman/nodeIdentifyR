#' Prepare dataframe for Plotting and Analysis of Variance
#'
#' Reshape the data to long format so that it can be plotted and used in an ANOVA
#' 
#' @param sum_Ising_samples a list containing vectors of sum scores
#'
#' @return a data.frame object in long format
#' @export
#'
#' @examples
#' # prepareDFforPlottingAndANOVA(listContainingSumScoreVectors)

prepareDFforPlottingAndANOVA <- function(sum_Ising_samples) {
    
    sumIsingSamplesLong <- data.frame(sumscore = double(), sample = character())
    for (i in 1:length(sum_Ising_samples)) {
        df <- data.frame(sumscore = sum_Ising_samples[[i]],
                         sample = rep(names(sum_Ising_samples)[i], 1000))
        sumIsingSamplesLong <- rbind(sumIsingSamplesLong, df)
    }
    
    return(sumIsingSamplesLong)
}