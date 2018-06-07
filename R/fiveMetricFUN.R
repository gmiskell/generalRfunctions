#' Five Performance Metrics.
#' 
#' A collection of tests that are output as one using the five performance metrics noted in Zikova, N., Hopke, P. K., & Ferro, A. R. (2017). Evaluation of new low-cost particle monitors for PM2.5 concentrations measurements. Journal of Aerosol Science, 105, 24-34. 10.1016/j.jaerosci.2016.11.010.
#' @param obs The column under investigation
#' @param comparison.value The comparison column.
#' @param bin The bin size to use in limit of detection. Default is 2.
#' @export
#' @examples
#' fiveMetricsFUN()


fiveMetricFUN <- function(x, obs, comparison.value, bin = 2){
  
  # load packages
  library(tidyverse)
  
  x <- as.data.frame(x)
  x$obs <- x[[obs]]
  x$comparison.value <- x[[comparison.value]]
  
  # load each function
  limitOfDetection <- function(x, obs, comparison.value, bin = bin){
    # minimum concentration for eahc bin of data where mean/sd > 3
    bins <- seq(0, max(x$obs), by = bin)
    x$bins <- .bincode(x$comparison.value, bins) * 2
    ratio <- x %>%
      group_by(bins) %>%
      mutate(ratio = mean(obs, na.rm = T) / sd(obs, na.rm = T)) %>%
      ungroup()
    LoD <- ratio %>%
      filter(!is.na(ratio)) %>%
      arrange(bins) %>%
      slice(which.min(bins))
    return(LoD$ratio)
  }
  
  linearRegression <- function(x, obs, comparison.value){
    # weighted least squares regression
    fit <- lm(obs ~ comparison.value, data = x)
    fit.df <- data.frame(intercept = fit$coefficients[1], slope = fit$coefficients[2], R2 = summary(fit)$r.squared)
    rownames(fit.df) <- NULL
    return(fit.df)
    }
  
  bias <- function(x, obs, comparison.value){
    # reported as a percentage
    bias <- mean(((x$comparison.value / x$obs) -1) * 100, na.rm = T)
    return(bias)
  }
  
  correlation <- function(x, obs, comparison.value){
    cor <- cor(x$obs, x$comparison.value, use = 'pairwise.complete.obs', method = 'pearson')
    return(cor)
  }
  
  precision <- function(x, obs, comparison.value){
    bias.corrected.value <- x$comparison.value * (mean(x$obs, na.rm = T) / mean(x$comparison.value, na.rm = T))
    prec <- mean(((abs(bias.corrected.value - mean(x$comparison.value, na.rm = T))) / mean(x$comparison.value, na.rm = T)) * 100, na.rm = T)
    return(prec)
  }
  
  # run tests and gather results into one dataframe to return
  gather.df <- data.frame(obs = obs,
                          comparison.value = comparison.value,
                          limit.of.detection = limitOfDetection(x),
                          linear.offset = linearRegression(x)[1],
                          linear.slope = linearRegression(x)[2],
                          linear.R2 = linearRegression(x)[3],
                          bias = bias(x),
                          correlation = correlation(x),
                          precision = precision(x))
  return(gather.df)
}
