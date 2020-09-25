#' Plot Sum Scores
#'
#' plots the inputted sum scores data.frames as a lineplot with conficence intervalss
#'
#' @param healthy_sum_score_long a data.frame containing sum scores
#'
#' @return a ggline line plot
#' @export
#'
#' @examples
#' # plotSumScores(LongFormatSumScoreDataframe)

plotSumScores <- function(healthy_sum_score_long) {

    sumScoresPlot <- ggline(healthy_sum_score_long, x = "sample", y = "sumscore",
                            add = c("mean_ci"),
                            ylab = "Sum Score", xlab = "Threshold Iteration") +
        theme_gray()

    return(sumScoresPlot)
}
