#' Calculate Sum Scores
#'
#' Creates a list with sum scores based on the given responses.
#'
#' @param Ising_samples a list containing dataframes with binary responses
#'
#' @return A list containing vectors of sum scores.
#' @export
#'
#' @examples
#' # calculateSumScores(listContainingSumScores)

calculateSumScores <- function(Ising_samples) {
    
    sumIsingSamples <- vector(mode = "list", length = length(Ising_samples))
    base::names(sumIsingSamples) <- base::names(Ising_samples)
    
    # calculate sum scores for every row per Ising model sample dataframe
    for (i in 1:length(Ising_samples)) {
        sumIsingSamples[[i]] <- apply(Ising_samples[[i]], 1, sum)
    }
    
    return(sumIsingSamples)
}