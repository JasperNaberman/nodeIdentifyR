#' 
#' Plot Sum Scores
#'
#' Plots the inputted sum scores data.frames as a lineplot with confidence intervals and also returns the plotted values. Orders the plot based on value and inputted 'perturbation_type'.
#'
#' @param sum_scores_long a data.frame containing sum scores
#' @param perturbation_type a string specifying a perturbation direction. Choose between "aggrevating" (+) and "alleviating" (-). Should be equal to the argument passed to simulateReponses().
#'
#' @return A list containing ggplot line graph object called sumScoresPlot and a data.table containing the data points that are plotted, including 95% confidence intervals of the mean value per threshold iteration.
#' @export
#'
#' @examples
#' # plotSumScores(sum_scores_long = LongFormatSumScoreDataframe)
#' 

plotSumScores <- function(sum_scores_long, perturbation_type) {
    
    dataSumScoresLongSummary <- Rmisc::summarySE(data = sum_scores_long,
                                                 measurevar = "sumscore",
                                                 groupvars = "sample",
                                                 conf.interval = .95)
    
    dataSumScoresLongSummary <- data.table::as.data.table(dataSumScoresLongSummary)
    
    if (perturbation_type == "alleviating") {
        data.table::setorder(x = dataSumScoresLongSummary, sumscore)
    } else if (perturbation_type == "aggrevating") {
        data.table::setorder(x = dataSumScoresLongSummary, -sumscore)
    }
    
    orderNames <- dataSumScoresLongSummary[sample != "original", sample]
    orderNames <- base::c("original", orderNames)
    
    data.table::setnames(dataSumScoresLongSummary,
                         base::c("sample", "sumscore"),
                         base::c("thresholdIteration", "meanSumscore"))
    
    dataSumScoresLongSummary[, ciLower := meanSumscore - ci]
    dataSumScoresLongSummary[, ciUpper := meanSumscore + ci]
    dataSumScoresLongSummary[, base::c("N", "sd", "se", "ci") := NULL]
    
    dataSumScoresLongSummary <- dataSumScoresLongSummary %>%
        dplyr::arrange(base::factor(x = thresholdIteration,
                                    levels = orderNames))
    
    sumScoresPlot <- ggplot2::ggplot(data = dataSumScoresLongSummary,
                                     mapping = ggplot2::aes(x = thresholdIteration,
                                                            y = meanSumscore,
                                                            group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = ciLower,
                                                      ymax = ciUpper),
                               width = .15) +
        ggplot2::scale_x_discrete(limits = orderNames) +
        ggplot2::labs(x = "Threshold iteration",
                      y = "Sum score") +
        ggplot2::theme(axis.line = ggplot2::element_line(colour = "grey20"),
                       axis.text = ggplot2::element_text(colour = "grey20",
                                                         size = 10),
                       axis.title = ggplot2::element_text(colour = "grey20",
                                                          size = 12),
                       axis.title.x = ggplot2::element_text(hjust = 0),
                       axis.title.y = ggplot2::element_text(hjust = 1),
                       axis.ticks = ggplot2::element_line(colour = "grey20"),
                       panel.background = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.grid.major.x = ggplot2::element_blank(),
                       panel.grid.major.y = ggplot2::element_line(colour = "grey80",
                                                                  size = .4))
    
    returnList <- base::list(sumScoresPlot = sumScoresPlot,
                             plottedInformation = dataSumScoresLongSummary)
    
    return(returnList)
}