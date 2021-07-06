#' 
#' Simulate Responses
#'
#' Simulates binary responses based on the input of network structure parameters.
#'
#' @param edge_weights a matrix containing edge weights of a network structure.
#' @param thresholds a vector containing node thresholds of a network structure.
#' @param perturbation_type a string specifying a perturbation direction. Choose between "aggravating" (+) and "alleviating" (-).
#' @param amount_of_SDs_perturbation an integer specifying with how many standard deviations of the threshold distribution to perturbate the threshold values.
#'
#' @return A data.frame structure containing simulated binary responses.
#' @export
#'
#' @examples
#' # simulateResponses(edgeWeightMatrix, thresholdVector, "aggravating", 2)
#' 

simulateResponses <- function(edge_weights, thresholds, perturbation_type, amount_of_SDs_perturbation) {

    IsingSamples <- base::vector(mode = "list",
                                 length = base::ncol(x = edge_weights) + 1)
    base::names(x = IsingSamples) <- base::c(base::colnames(x = edge_weights),
                                             "original")

    # iterate through node, creating a dataframe with a perturbation for each iteration, effectively changing nodes one by one
    for (i in 1:base::length(x = IsingSamples)) {
        # sample an Ising model state with a pertubated threshold vector
        if (i %in% 1:(base::length(x = IsingSamples) - 1)) {
            perturbation <- thresholds

            if (perturbation_type == "aggravating") {
                perturbation[i] <- thresholds[i] + amount_of_SDs_perturbation * stats::sd(x = thresholds)
            } else if (perturbation_type == "alleviating") {
                perturbation[i] <- thresholds[i] - amount_of_SDs_perturbation * stats::sd(x = thresholds)
            }

            IsingModelState <- IsingSampler::IsingSampler(n = 5000,
                                                          graph = edge_weights,
                                                          thresholds = perturbation,
                                                          responses = base::c(0L, 1L))
            IsingSamples[[i]] <- IsingModelState
        }

        # sample an Ising model state with the original thresholds
        if (i == base::length(x = IsingSamples)) {
            IsingModelState <- IsingSampler::IsingSampler(n = 5000,
                                                          graph = edge_weights,
                                                          thresholds = thresholds,
                                                          responses = base::c(0L, 1L))
            IsingSamples[[i]] <- IsingModelState
        }
    }

    namesOrder <- base::c("original", base::names(x = IsingSamples)[1:base::ncol(x = edge_weights)])

    IsingSamples <- IsingSamples[namesOrder]

    return(IsingSamples)
}
