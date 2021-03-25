#' 
#' Prepare dataframe for Plotting and Analysis of Variance
#'
#' Reshape the data to long format so that it can be plotted and used in an ANOVA
#' 
#' @import data.table
#' 
#' @param sum_Ising_samples a list containing vectors of sum scores in numeric format
#' 
#'
#' @return A data.frame structure in long format.
#' @export
#'
#' @examples
#' # prepareDFforPlottingAndANOVA(listContainingSumScoreVectors)
#' 

prepareDFforPlottingAndANOVA <- function(sum_Ising_samples) {
    
    sumIsingSamplesLong <- base::data.frame(sumscore = base::double(),
                                            sample = base::character())
    
    for (i in 1:base::length(x = sum_Ising_samples)) {
        df <- base::data.frame(sumscore = sum_Ising_samples[[i]],
                               sample = base::rep(x = base::names(x = sum_Ising_samples)[i],
                                                  times = 1000))
        sumIsingSamplesLong <- base::rbind(sumIsingSamplesLong, df)
    }
    
    sumIsingSamplesLong <- data.table::as.data.table(x = sumIsingSamplesLong)
    
    allMeansDT <- sumIsingSamplesLong[, .(meanSumscore = base::mean(sumscore)), sample]
    
    data.table::setorder(x = allMeansDT, -meanSumscore)
    
    orderNames <- allMeansDT[sample != "original", sample]
    orderNames <- base::c("original", orderNames)
    
    sumIsingSamplesLong <- sumIsingSamplesLong %>%
        dplyr::arrange(base::factor(x = sample,
                                    levels = orderNames))
    
    return(sumIsingSamplesLong)
}