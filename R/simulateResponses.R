#' Simulate Responses
#'
#' Simulates binary responses based on the input of network structure parameters.
#'
#' @param edge_weights a matrix containing edge weights of a network structure.
#' @param thresholds a vector containing node thresholds of a network structure.
#' @param perturbation_direction a string specifying a perturbation direction. Choose between "positive" (+) and "negative" (-).
#' @param amount_of_SDs_perturbation an integer specifying with how many standard deviations of the threshold distribution to perturbate the threshold values.
#'
#' @return A data.frame structure containing simulated binary responses.
#' @export
#'
#' @examples
#' # simulateResponses(edgeWeightMatrix, thresholdVector, "positive", 2)

simulateResponses <- function(edge_weights, thresholds, perturbation_direction, amount_of_SDs_perturbation) {

    IsingSamples <- vector(mode = "list", length = ncol(edge_weights) + 1)
    names(IsingSamples) <- c(colnames(edge_weights), "original")

    # iterate through node, creating a dataframe with a perturbation for each iteration, effectively changing nodes one by one
    for (i in 1:length(IsingSamples)) {
        # sample an Ising model state with a pertubated threshold vector
        if (i %in% 1:(length(IsingSamples) - 1)) {
            perturbation <- thresholds

            if (perturbation_direction == "positive") {
                perturbation[i] <- thresholds[i] + amount_of_SDs_perturbation * stats::sd(thresholds)
            } else if (perturbation_direction == "negative") {
                perturbation[i] <- thresholds[i] - amount_of_SDs_perturbation * stats::sd(thresholds)
            }

            IsingModelState <- IsingSampler::IsingSampler(1000, edge_weights,
                                                          perturbation,
                                                          responses = c(0L, 1L))
            IsingSamples[[i]] <- IsingModelState
        }

        # sample an Ising model state with the original thresholds
        if (i == length(IsingSamples)) {
            IsingModelState <- IsingSampler::IsingSampler(1000, edge_weights,
                                                          thresholds,
                                                          responses = c(0L, 1L))
            IsingSamples[[i]] <- IsingModelState
        }
    }

    namesOrder <- c("original", names(IsingSamples)[1:ncol(edge_weights)])

    IsingSamples <- IsingSamples[namesOrder]

    return(IsingSamples)
}
