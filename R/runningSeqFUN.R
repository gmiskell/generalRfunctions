#' Running Sequence Function for Blocks of Data Analysis.
#' 
#' A function for labelling data into their blocks.
#' @param date Date column.
#' @param run.length The size of a block of data to analyse.
#' @param measurement.freq Either `mins` (default) or `hours`.
#' @export
#' @examples
#' runningSeqFUN()


runningSeqFUN <- function(x, date, run.length, measurement.freq){
  
  # load required libraries
  library(lubridate)
  
  floored_date <- floor_date(min(x$date, na.rm = T))
  max_date <- max(x$date, na.rm = T)
  
  seq.by = 60
  if(measurement.freq == 'hours'){seq.by = 60 * 60}
  
  # if dates needed flooring, find the difference between min date and floor date
  diff.time <- as.numeric(difftime(min(x$date, na.rm = T), floored_date, units = measurement.freq))
  
  seq.length <- nrow(x) + diff.time
  
  # make a dataframe that includes the date and the run group (e.g. every three day group)
  seq.date = seq.POSIXt(floored_date, max_date, by = seq.by)
  seq.integer = rep(seq(1:nrow(x)), each = run.length)
  seq.integer = seq.integer[1:length(seq.date)]
  seq.df <- data.frame(date = seq.date, run.group = seq.integer)
  
  # join to the original dataframe, x
  x <- left_join(x, seq.df)
}