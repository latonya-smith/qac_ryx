#' @title ryx.summary
#'
#' @description
#' This \code{summary} function that summarizes the contents of the ryx function by
#' giving the median and range of the correlation values along with the number of values
#' that are significant.
#'
#' @details
#' The summary function uses the data from the results of the ryx function
#'
#' @param x list of y variable and x variables for correlation along with the data frame of correlations p values and level of significance
#'
#' @import MASS
#' @export
#' @examples
#'
#' x<- ryx(Boston, y="medv")
#' summary(x)
#'


######################## SUMMARY
summary.ryx<- function(x, digits=3){
 vars<- paste(as.list(x$df$variable), collapse= " ")
 text<- paste0(
  "Correlating ", x$y, " with ", vars,
  ". The median absolute correlation was ", round(median(abs(x$df$r)), digits),
  " with a range from ", round(min(x$df$r), digits), " to", round(max(x$df$r), digits), ".",
  length(x$df$sigif == "***"| x$df$sigif == "**" | x$df$sigif == "*"), " out of ", length(x$x), " variables where significant at the p <0.05 level"
 )
 cat(text)
}

