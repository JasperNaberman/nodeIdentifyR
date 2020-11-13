#' Prepare dataframe for Plotting and Analysis of Variance
#'
#' Reshape the data to long format so that it can be plotted and used in an ANOVA
#' 
#' @param sum_Ising_samples a list containing vectors of sum scores
#'
#' @return A data.frame structure in long format
#' @export
#'
#' @examples
#' # prepareDFforPlottingAndANOVA(listContainingSumScoreVectors)

prepareDFforPlottingAndANOVA <- function(sum_Ising_samples) {
    
    sumIsingSamplesLong <- data.frame(sumscore = double(), sample = character())
    for (i in 1:length(sum_Ising_samples)) {
        df <- data.frame(sumscore = sum_Ising_samples[[i]],
                         sample = rep(base::names(sum_Ising_samples)[i], 1000))
        sumIsingSamplesLong <- rbind(sumIsingSamplesLong, df)
    }
    
    sumIsingSamplesLong <- as.data.table(sumIsingSamplesLong)
    
    allMeansDT <- data.table(sample = character(), meanSumScore = double())
    for (i in 1:length(base::names(sum_Ising_samples))) {
        currentSample <- base::names(sum_Ising_samples)[i]
        tempDT <- sumIsingSamplesLong[sample == currentSample]
        meanDT <- data.table(sample = base::names(sum_Ising_samples)[i],
                             meanSumScore = tempDT[, base::mean(sumscore)])
        
        allMeansDT <- base::rbind(allMeansDT, meanDT)
    }
    
    data.table::setorder(allMeansDT, -meanSumScore)
    
    orderNames <- allMeansDT[sample != "original", sample]
    orderNames <- base::c("original", orderNames)
    
    sumIsingSamplesLong <- sumIsingSamplesLong %>%
        dplyr::arrange(base::factor(sample, levels = orderNames))
    
    return(sumIsingSamplesLong)
}