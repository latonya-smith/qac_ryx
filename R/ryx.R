#' @title ryx.print
#'
#' @description
#' This \code{print} function presents a table of variables, correlation values of x
#' variables with the specified y value(all defined from the ryx function), the corresponding
#' p-values and a column that specifies how significant these p-values are.
#'
#' @details
#' The print function is a wrapper for the cbind base R function
#' \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cbind}{cbind}.
#'
#' #' @param x list of y variable and x variables for correlation along with the data frame of correlations p values and level of significance
#'
#' @export
#' @examples
#'
#' x<- ryx(Boston, y="medv")
#' print(x)
#'
#'


ryx <- function(data, y, x){
 if(missing(x)){
  x <- names(data)[sapply(data, class)=="numeric"]
  x <- setdiff(x, y)
 }
 df <- data.frame()
 for (var in x){
  res <- cor.test(data[[y]], data[[var]])
  df_temp <- data.frame(variable = var,
                        r = res$estimate,
                        p = res$p.value)
  df <- rbind(df, df_temp)
  df <- df[order(-abs(df$r)),]
 }

 df$sigif <- ifelse(df$p < .001, "***",
                    ifelse(df$p < .01, "**",
                           ifelse(df$p < .05, "*", " ")))
 results <- list(y=y, x=x, df=df)
 class(results) <- "ryx"
 return(results)
}

