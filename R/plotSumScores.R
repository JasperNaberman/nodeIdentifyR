#' Plot Sum Scores
#'
#' Plots the inputted sum scores data.frames as a lineplot with confidence intervals
#'
#' @param healthy_sum_score_long a data.frame containing sum scores
#'
#' @return A ggline line plot, including confidence intervals of the mean value per node.
#' @export
#'
#' @examples
#' # plotSumScores(LongFormatSumScoreDataframe)

plotSumScores <- function(healthy_sum_score_long) {

    sumScoresPlot <- ggpubr::ggline(healthy_sum_score_long, x = "sample", y = "sumscore",
                                    add = c("mean_ci"),
                                    ylab = "Sum Score", xlab = "Threshold Iteration") +
        ggplot2::theme_gray()

    return(sumScoresPlot)
}
