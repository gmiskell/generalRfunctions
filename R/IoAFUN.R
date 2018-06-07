#' Index of Agreement over a running window function
#'
#' @param obs The column under investigation.
#' @param comparison.value The comparison column.
#' @param date Date column.
#' @param window.size The size of the running window.
#' @export
#' @examples
#' IoAFUN()


IoA <- function(x, obs, comparison.value, date, window.size){
  
  # load packages
  library(zoo);library(hydroGOF);library;library(tidyverse);
  
  # specify variables in x
  x <- as.data.frame(x)
  x$obs <- x[[obs]]; x$comparison.value <- x[[comparison.value]]; x$date <- x[[date]]
  
  # arrange data by date
  x <- x %>%
    dplyr::arrange(date)
  date <- x$date
  
  # convert x into a zoo structure
  y <- data.frame(x[,obs], x[,comparison.value])
  y.zoo <- zoo(y)
  colnames(y.zoo) <- c('s', 'r')
  # use `d` function in hydroGOF for finding the index of agreement
  ioa <- rollapply(y.zoo, function(x) d(x[, 's'], x[, 'r']), width = window.size, by.column = F, fill = NA, align = 'right') 
  
  # tidy output and join with the original dataframe x
  ioa <- data.frame(ioa)
  round <- data.frame(lapply(ioa, function(x) round(x, 3)))
  a <- cbind(date, round)
  b <- left_join(x, a, by = 'date')
}