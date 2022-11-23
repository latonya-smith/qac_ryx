#' @title Visualize ryx function results in horizontal graph
#'
#' @description
#' This \code{plot} function that plots the variables against the correlation values
#' (data from ryx function) in a horizontal graph
#'
#' @details
#' The plot function is a wrapper for the
#' \href{https://ggplot2.tidyverse.org/reference/ggplot.html}{ggplot2}.
#'
#'
#' @seealso
#' \link[ggplot2]{ggplot2}
#'
#' @param x results from ryx function
#'
#' @import ggplot2
#' @import MASS
#'
#' @export
#' @examples
#' x<- ryx(Boston, y="medv")
#' plot(x)
#'

########################### PLOT


plot.ryx<- function(x, digits = 3){
 library(ggplot2)
 title<- paste("Correlations with", x$y)
 x$df$direction<- ifelse(x$df$r < 0, "negative" , "positive")
 Direction = factor(x$df$direction)
 ggplot(data =x$df ,
        aes(x= abs(x$df$r),
            y= reorder(x$df$variable, abs(x$df$r)), color = Direction)) + geom_point(size = 4) +
  geom_segment(aes(x = 0.0, xend = abs(x$df$r), y = reorder(x$df$variable, abs(x$df$r)),
                   yend = reorder(x$df$variable, abs(x$df$r)), color = "lightgrey"))  +
  labs(title = title, x = "Correlation(absolute value)", y = "Variables") +
  scale_color_manual(values = c("negative" = "red", "positive" = "blue")) +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(linetype = "dashed"), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(breaks = seq(0.0, 1.0, 0.1), limits=c(0.0,1))
}
