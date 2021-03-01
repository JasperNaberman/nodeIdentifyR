#' 
#' Calculate Sum Scores
#'
#' Creates a list with sum scores based on the given responses.
#'
#' @param Ising_samples a list containing dataframes with binary responses in numeric format
#'
#' @return A list containing vectors of sum scores.
#' @export
#'
#' @examples
#' # calculateSumScores(listContainingSumScores)
#' 

calculateSumScores <- function(Ising_samples) {
    
    sumIsingSamples <- base::vector(mode = "list",
                                    length = base::length(x = Ising_samples))
    
    base::names(x = sumIsingSamples) <- base::names(x = Ising_samples)
    
    # calculate sum scores for every row per Ising model sample dataframe
    for (i in 1:base::length(x = Ising_samples)) {
        sumIsingSamples[[i]] <- base::apply(X = Ising_samples[[i]],
                                            MARGIN = 1,
                                            FUN = base::sum)
    }
    
    return(sumIsingSamples)
}