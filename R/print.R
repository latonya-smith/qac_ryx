#' @title Visualize ryx function results in dataframe
#'
#' @description
#' This \code{print} function presents a table of correlation values of x
#' variables with the specified y value(all defined from the ryx function), the corresponding
#' p-values and a column that specifies how significant these p-values are.
#'
#' @details
#' The print function is a wrapper for the cbind base R function
#' \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cbind}{cbind}.
#'
#' @param x results from ryx function
#'
#' @import MASS
#'
#'
#' @export
#' @examples
#' x<- ryx(Boston, y="medv")
#' print(x)
#'
#'

###################### PRINT
print.ryx<- function(x, digits = 3){
 Variable<- x$df$variable
 r<- as.numeric(round(x$df$r, digits))
 p<- format.pval(x$df$p, digits)
 sigif<- x$df$sigif
 as.data.frame(cbind(Variable, r, p, sigif))
}
