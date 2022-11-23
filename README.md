# ryx

![](stats.png) <!-- badges: start -->

<!-- badges: end -->

The goal of ryx is to provide a new way of presenting and visualizing correlation tests. It uses the correlation test base R function to create a data frame that shows the correlation values, p values and the level of significance of each p value using helper functions that 
show these in a dataframe, summarized text and a graph.

## Installation

You can install the development version of ryx like so:

``` r
# install.packages("ryx")
```

## Example

This is a basic example which shows how to use the functions in this package:

``` r
library(ryx)
library(MASS)
## basic example code

x<- ryx(Boston, y="medv")
print(x)
summary(x)
plot(x)
```
