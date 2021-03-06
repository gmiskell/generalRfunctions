% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fiveMetricFUN.R
\name{fiveMetricFUN}
\alias{fiveMetricFUN}
\title{Five Performance Metrics.}
\usage{
fiveMetricFUN(x, obs, comparison.value, bin = 2)
}
\arguments{
\item{obs}{The column under investigation}

\item{comparison.value}{The comparison column.}

\item{bin}{The bin size to use in limit of detection. Default is 2.}
}
\description{
A collection of tests that are output as one using the five performance metrics noted in Zikova, N., Hopke, P. K., & Ferro, A. R. (2017). Evaluation of new low-cost particle monitors for PM2.5 concentrations measurements. Journal of Aerosol Science, 105, 24-34. 10.1016/j.jaerosci.2016.11.010.
}
\examples{
fiveMetricsFUN()
}
