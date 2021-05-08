#' 
#' Plot Sum Scores
#'
#' Plots the inputted sum scores data.frames as a lineplot with confidence intervals
#'
#' @param sum_scores_long a data.frame containing sum scores
#'
#' @return A ggplot line graph, including 95% confidence intervals of the mean value per threshold iteration.
#' @export
#'
#' @examples
#' # plotSumScores(sum_scores_long = LongFormatSumScoreDataframe)
#' 

plotSumScores <- function(sum_scores_long) {
    
    dataSumScoresLongSummary <- Rmisc::summarySE(data = sum_scores_long,
                                                 measurevar = "sumscore",
                                                 groupvars = "sample",
                                                 conf.interval = .95)
    
    dataSumScoresLongSummary <- data.table::as.data.table(dataSumScoresLongSummary)
    data.table::setorder(x = dataSumScoresLongSummary, -sumscore)
    orderNames <- dataSumScoresLongSummary[sample != "original", sample]
    orderNames <- base::c("original", orderNames)
    
    dataSumScoresLongSummary <- dataSumScoresLongSummary %>%
        dplyr::arrange(base::factor(x = sample,
                                    levels = orderNames))
    
    sumScoresPlot <- ggplot2::ggplot(data = dataSumScoresLongSummary,
                                     mapping = ggplot2::aes(x = sample,
                                                            y = sumscore,
                                                            group = 1)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::geom_errorbar(mapping = ggplot2::aes(ymin = sumscore - ci,
                                                      ymax = sumscore + ci),
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
    
    return(sumScoresPlot)
}