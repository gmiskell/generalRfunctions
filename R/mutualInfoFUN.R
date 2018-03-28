#' Mutual Information function.
#' 
#' Checks the mutual information between two samples using the Kullback-Leibler divergence in the `entropy` package.
#' @param obs The first column under investigation.
#' @param comparison.value The second column.
#' @param bin Bin size for grouping the data. Default is 5.
#' @export
#' @examples
#' mutualInfoFUN()


mutualInfoFUN <- function(x, obs, comparison.value, bin = 5){
  
  # load libraries
  library(tidyverse); library(entropy); library(infotheo);
  
  x$obs <- x[, obs]
  x$comparison.value <- x[, comparison.value]
  
  # set binning profile - this makes bins of the specified size
  b <- c(0, bin * (1:max(x$obs)))
  
  # remove any NA values
  x <- x %>%
    filter(!is.na(obs) & !is.na(comparison.value)) %>%
    mutate(obs = .bincode(obs, b, F),
           comparison.value = .bincode(comparison.value, b, F)) %>%
    filter(obs > 0 & comparison.value > 0)
 
  # find the mutual information and the associated "correlation"
  mi.calc <- function(x){
    set.table <- discretize2d(x[, obs], x[, comparison.value], numBins1 = bin, numBins2 = bin)
    mi <- mi.plugin(set.table)
    mi.cor <- (1 - exp(-2 * mi))^(0.5)
    return(data.frame(mi = round(mi, 4), mi.cor = round(mi.cor, 4)))
    }
  
  mi <- mi.calc(x)
}