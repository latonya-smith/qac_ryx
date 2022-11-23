#' @title Visualize correlation tests' results
#'
#' @description
#' This \code{print} function presents a list containing:
#' 1. The y variables
#' 2. The list of x variables for correlation tests
#' 3. A dataframe of correlation values of x variables with the specified y values
#' (all defined from the ryx function), the corresponding p-values and a column that
#' specifies how significant these p-values are.
#'
#' @details
#' The print function is a wrapper for the cbind base R function
#' \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/cbind}{cbind}.
#'
#' @param data dataset to run correlation tests on.
#' @param x list of x variables for correlation tests(If this is missing then the function will run on all other numeric variables as x)
#' @param y y variable for correlation test.
#'
#' @export
#' @examples
#' x<-ryx(Boston, y="medv")
#' lapply(x, head)
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

