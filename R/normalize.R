#' Function used to normalize a time series using min-max values
#'
#' This code is part of the HCTI package
#' Date: 2020/2021
#'
#' Developer: Ricardo Rios
#'
#' GNU General Public License v3.0
#'
#' Permissions of this strong copyleft license are
#'    conditioned on making available complete
#'    source code of licensed works and
#'    modifications, which include larger works
#'    using a licensed work, under the same license.
#'    Copyright and license notices must be
#'    preserved. Contributors provide an express
#'    grant of patent rights.
#'
#' @param series A set of observations (time series)
#' @return A normalized time series
#' @examples
#' normalization(rnorm(10, mean=100, sd=1))
normalization<-function(series){
  (series-min(series))/(max(series)-min(series))
}
