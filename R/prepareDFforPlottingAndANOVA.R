#' 
#' Prepare dataframe for Plotting and Analysis of Variance
#'
#' Reshape the data to long format so that it can be plotted and used in an ANOVA
#' 
#' @param sum_Ising_samples a list containing vectors of sum scores in numeric format
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
    
    allMeansDT <- data.table::data.table(sample = base::character(),
                                         meanSumScore = base::double())
    
    for (i in 1:base::length(x = base::names(x = sum_Ising_samples))) {
        tempDT <- sumIsingSamplesLong[sample == base::names(x = dataSumScores)[i]]
        meanDT <- data.table::data.table(sample = base::names(x = sum_Ising_samples)[i],
                                         meanSumScore = tempDT[, base::mean(x = sumscore)])
        
        allMeansDT <- base::rbind(allMeansDT, meanDT)
    }
    
    data.table::setorder(x = allMeansDT, -meanSumScore)
    
    orderNames <- allMeansDT[sample != "original", sample]
    orderNames <- base::c("original", orderNames)
    
    sumIsingSamplesLong <- sumIsingSamplesLong %>%
        dplyr::arrange(base::factor(x = sample,
                                    levels = orderNames))
    
    return(sumIsingSamplesLong)
}